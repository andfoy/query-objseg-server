
import os
import re
import tqdm
import torch
import requests
import numpy as np
import os.path as osp
from referit.refer import REFER
import torch.nn.functional as F
from unittest.mock import MagicMock
from dmn_pytorch import DMN, ReferDataset
from referit.refer import mask as cocomask
from dmn_pytorch.utils.transforms import ResizeImage, ResizePad
from torchvision.transforms import Compose, ToTensor, Normalize

regex = re.compile(r'COCO_train2014_0*(\d+)[.]jpg')

args = MagicMock()

args.size = 512
args.emb_size = 1000
args.hid_size = 1000
args.vis_size = 2688
args.num_filters = 10
args.mixed_size = 1000
args.hid_mixed_size = 1005
args.lang_layers = 3
args.mixed_layers = 3
args.mix_we = True
args.lstm = False
args.backend = 'dpn92'
args.high_res = True
args.upsamp_mode = 'bilinear'
args.upsamp_size = 3
args.upsamp_amplification = 32
args.data = "../../referit_data"
args.dataset = 'unc'
args.split = 'train'
args.time = -1

input_transform = Compose([
    ToTensor(),
    ResizeImage(args.size),
    Normalize(
        mean=[0.485, 0.456, 0.406],
        std=[0.229, 0.224, 0.225])
])

# for dataset in ReferDataset.SUPPORTED_DATASETS:
    # if dataset == 'referit':
        # continue
print("Processing {0}".format(args.dataset))
dataset = args.dataset
if 'params' in ReferDataset.SUPPORTED_DATASETS[dataset]:
    refer_db = REFER(osp.join(args.data, 'other'),
                     **ReferDataset.SUPPORTED_DATASETS[dataset]['params'])
for split in ReferDataset.SUPPORTED_DATASETS[dataset]['splits']:
    if split == "trainval":
        continue
    args.split = split
    print('Processing {0}'.format(split))
    refer = ReferDataset(data_root=args.data,
                         split_root='../../query-objseg/data',
                         dataset=args.dataset,
                         split=args.split,
                         transform=input_transform,
                         max_query_len=args.time)
    first_entry = None
    prev_entry = None

    net = DMN(dict_size=len(refer.corpus),
             emb_size=args.emb_size,
             hid_size=args.hid_size,
             vis_size=args.vis_size,
             num_filters=args.num_filters,
             mixed_size=args.mixed_size,
             hid_mixed_size=args.hid_mixed_size,
             lang_layers=args.lang_layers,
             mixed_layers=args.mixed_layers,
             backend=args.backend,
             mix_we=args.mix_we,
             lstm=args.lstm,
             high_res=args.high_res,
             upsampling_mode=args.upsamp_mode,
             upsampling_size=args.upsamp_size,
             upsampling_amplification=args.upsamp_amplification)

    net = net.cuda()
    net.load_state_dict(torch.load('../../query-objseg-weights/highres/'
                        'dmn_{0}_weights.pth'.format(args.dataset)))

    for idx in tqdm.tqdm(range(0, len(refer.images))):
        img_file, mask_file, text_phrase = refer.images[idx]
        re_match = regex.match(img_file)
        img_id = int(re_match.group(1))
        img, mask, phrase = refer[idx]
        img = img.unsqueeze(0).cuda()
        phrase = phrase.unsqueeze(0).cuda()
        with torch.no_grad():
            out_mask = net(img, phrase)
        out_mask = F.upsample(out_mask, size=(mask.size(-2), mask.size(-1)),
                              mode='bilinear')
        out_mask = F.sigmoid(out_mask)
        out_mask = out_mask.cpu().numpy()
        mask = mask.unsqueeze(-1).numpy().astype(np.uint8)
        coco_mask = cocomask.encode(mask)[0]
        def find_ids():
            anns = [r for r in refer_db.anns
                    if refer_db.anns[r]['image_id'] == img_id]
            for ann_id in anns:
                possible_refs = [refer_db.refs[r] for r in refer_db.refs
                                 if refer_db.refs[r]['ann_id'] == ann_id]
                for ref in possible_refs:
                    for sentence in ref['sentences']:
                        if sentence['sent'] == text_phrase:
                            return (ann_id, ref['ref_id'],
                                    sentence['sent_id'])
        ann_id = ref_id = sent_id = None
        if args.dataset != 'referit':
            ann_id, ref_id, sent_id = find_ids()
        img_url = refer_db.imgs[img_id]['flickr_url']
        req_body = {'id': str(sent_id), 'ref_id': str(ref_id),
                    'ann_id': str(ann_id), 'img_id': str(img_id),
                    'img_url': img_url, 'dataset': args.dataset,
                    'split': args.split, 'query_expr': text_phrase,
                    'mask': str(coco_mask['counts'], 'utf-8')}
        if first_entry is None:
            first_entry = req_body
            prev_entry = req_body
            requests.post('http://10.1.0.4:4892/datasets', json={
                'name': args.dataset, 'split': args.split,
                'start_id': req_body['id']})
        else:
            req_body['prev_id'] = prev_entry['id']
            prev_entry['next_id'] = req_body['id']
            requests.post('http://10.1.0.4:4892/datasets/{0}'.format(
                args.dataset), json=prev_entry)
            prev_entry = req_body
        store_path = '../masks/{0}/{1}'.format(args.dataset, args.split)
        try:
            os.makedirs(store_path)
        except:
            pass
        if not osp.exists(osp.join(store_path, req_body['id'])):
            with open(osp.join(store_path, req_body['id']), 'wb') as f:
                f.write(out_mask.tobytes())

    prev_entry['next_id'] = first_entry['id']
    first_entry['prev_id'] = prev_entry['id']
    for entry in [prev_entry, first_entry]:
        requests.post('http://10.1.0.4:4892/datasets/{0}'.format(
            args.dataset), json=entry)
    net = net.cpu()
    del net
    torch.cuda.empty_cache()
