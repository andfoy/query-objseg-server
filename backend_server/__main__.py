# -*- coding: utf-8 -*-
# !/usr/bin/env python

"""LangVisNet backend Python server."""

# Standard lib imports
import os
import logging
import argparse
import os.path as osp

# Tornado imports
import tornado.web
import tornado.ioloop

# Torch imports
import torch
import torch.nn.functional as F
from torch.autograd import Variable
from torchvision.transforms import Compose, ToTensor, Normalize

# LangVisNet imports
from langvisnet import ReferDataset
from langvisnet import LangVisUpsample
from langvisnet.utils import ResizeImage

# Local imports
from backend_server.routes import ROUTES
from backend_server.listeners import LISTENERS
from backend_server.amqp.client import ExampleConsumer

# Other library imports
import coloredlogs

parser = argparse.ArgumentParser(
    description='Query Segmentation Network backend server')

# Dataloading-related settings
parser.add_argument('--data', type=str, default='../referit_data',
                    help='path to ReferIt splits data folder')
parser.add_argument('--split-root', type=str, default='data',
                    help='path to dataloader files folder')
parser.add_argument('--dataset', default='unc', type=str,
                    help='dataset used to train QSegNet')
parser.add_argument('--split', default='train', type=str,
                    help='name of the dataset split used to train')
parser.add_argument('--snapshot', default='weights/qsegnet_unc_snapshot.pth',
                    help='path to weight snapshot file')

# Training procedure settings
parser.add_argument('--no-cuda', action='store_true',
                    help='Do not use cuda to train model')
parser.add_argument('--batch-size', default=1, type=int,
                    help='Batch size for training')
parser.add_argument('--seed', type=int, default=1111,
                    help='random seed')
parser.add_argument('--eval', action='store_true',
                    help='enable PyTorch evaluation mode')
parser.add_argument('--old-weights', action='store_true', default=False,
                    help='load LangVisNet weights on a LangVisUpsample module')
parser.add_argument('--gpu-pair', type=int, default=None,
                    help='gpu pair to use: either 0 (GPU0 and GPU1)'
                         'or 1 (GPU2 and GPU3)')

# Model settings
parser.add_argument('--size', default=512, type=int,
                    help='image size')
parser.add_argument('--time', default=-1, type=int,
                    help='maximum time steps per batch')
parser.add_argument('--emb-size', default=1000, type=int,
                    help='word embedding dimensions')
parser.add_argument('--hid-size', default=1000, type=int,
                    help='language model hidden size')
parser.add_argument('--vis-size', default=2688, type=int,
                    help='number of visual filters')
parser.add_argument('--num-filters', default=1, type=int,
                    help='number of filters to learn')
parser.add_argument('--mixed-size', default=1000, type=int,
                    help='number of combined lang/visual features filters')
parser.add_argument('--hid-mixed-size', default=1005, type=int,
                    help='multimodal model hidden size')
parser.add_argument('--lang-layers', default=2, type=int,
                    help='number of SRU/LSTM stacked layers')
parser.add_argument('--mixed-layers', default=3, type=int,
                    help='number of mLSTM/mSRU stacked layers')
parser.add_argument('--backend', default='dpn92', type=str,
                    help='default backend network to LangVisNet')
parser.add_argument('--mix-we', action='store_true', default=False,
                    help='train linear layer filters based also on WE')
parser.add_argument('--lstm', action='store_true', default=False,
                    help='use LSTM units for RNN modules. Default SRU')
parser.add_argument('--high-res', action='store_true',
                    help='high res version of the output through '
                         'upsampling + conv')
parser.add_argument('--upsamp-channels', default=50, type=int,
                    help='number of channels in the upsampling convolutions')
parser.add_argument('--upsamp-mode', default='bilinear', type=str,
                    help='upsampling interpolation mode')
parser.add_argument('--upsamp-size', default=3, type=int,
                    help='upsampling convolution kernel size')
parser.add_argument('--upsamp-amplification', default=32, type=int,
                    help='upsampling scale factor')

parser.add_argument('--amqp', type=str, required=True,
                    help='AMQP url endpoint used to locate frontend service')
parser.add_argument('--port', type=int, default=True,
                    help='TCP port used to deploy the server')

# AMQP_URL = ('amqp://langvis_server:eccv2018-textseg@margffoy-tuay.com:5672/'
#             'queryobjseg')

LOG_FORMAT = ('%(levelname) -10s %(asctime)s %(name) -30s %(funcName) '
              '-35s %(lineno) -5d: %(message)s')
LOGGER = logging.getLogger(__name__)
coloredlogs.install(level='info')

args = parser.parse_args()
args.cuda = not args.no_cuda and torch.cuda.is_available()

torch.manual_seed(args.seed)
if args.cuda:
    torch.cuda.manual_seed(args.seed)

image_size = (args.size, args.size)

input_transform = Compose([
    ToTensor(),
    ResizeImage(args.size),
    Normalize(
        mean=[0.485, 0.456, 0.406],
        std=[0.229, 0.224, 0.225])
])


refer = ReferDataset(data_root=args.data,
                     split_root=args.split_root,
                     dataset=args.dataset,
                     split=args.split,
                     max_query_len=args.time)

net = LangVisUpsample(dict_size=len(refer.corpus),
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
                      upsampling_channels=args.upsamp_channels,
                      upsampling_mode=args.upsamp_mode,
                      upsampling_size=args.upsamp_size,
                      gpu_pair=args.gpu_pair,
                      upsampling_amplification=args.upsamp_amplification)

if osp.exists(args.snapshot):
    print('Loading state dict')
    snapshot_dict = torch.load(args.snapshot)
    if args.old_weights:
        state = {}
        for weight_name in snapshot_dict.keys():
            state['langvis.' + weight_name] = snapshot_dict[weight_name]
        snapshot_dict = state
    net.load_state_dict(snapshot_dict)

if args.cuda:
    net.cuda()

clr = 'clear'
if os.name == 'nt':
    clr = 'cls'


@tornado.gen.coroutine
def forward(img, phrase):
    h, w, _ = img.shape
    img = input_transform(img)
    words = refer.tokenize_phrase(phrase)
    img = Variable(img, volatile=True).unsqueeze(0)
    words = Variable(words, volatile=True).unsqueeze(0)
    if args.cuda:
        img = img.cuda()
        words = words.cuda()
    out = net(img, words)
    out = F.sigmoid(out)
    out = F.upsample(out, size=(h, w), mode='bilinear').squeeze()
    return out


def main():
    logging.basicConfig(level=logging.DEBUG, format=LOG_FORMAT)
    settings = {"static_path": os.path.join(
        os.path.dirname(__file__), "static")}
    application = tornado.web.Application(
        ROUTES, debug=True, serve_traceback=True, autoreload=True,
        **settings)
    print("Server is now at: 127.0.0.1:8000")
    ioloop = tornado.ioloop.IOLoop.instance()

    outq = ExampleConsumer(LOGGER, args.amqp_url, LISTENERS, net)
    application.outq = outq

    application.outq.connect()
    application.listen(args.port)
    try:
        ioloop.start()
    except KeyboardInterrupt:
        pass
    finally:
        print("Closing server...\n")
        tornado.ioloop.IOLoop.instance().stop()


if __name__ == '__main__':
    os.system(clr)
    main()
