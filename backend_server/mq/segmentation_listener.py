# -*- coding: utf-8 -*-

# Standard lib imports
import os
import time
import base64
import logging
from functools import wraps
from tempfile import TemporaryFile

# Torch imports
import torch
import torch.nn.functional as F
# from torch.autograd import Variable

# Tornado imports
import tornado.gen
from concurrent.futures import ThreadPoolExecutor

# Other imports
import cv2
import boto3
import visdom
import numpy as np
from PIL import Image
from io import BytesIO
from matplotlib import cm

# Local imports
from backend_server.amqp import APP


LOGGER = logging.getLogger(__name__)


REQUEST = 'REQUEST'
REQUEST_ANSWER = 'REQUEST_ANSWER'
EXCHANGE = 'queryobj'
ROUTING_KEY = 'query.answers'
results = {}

MAX_WORKERS = 4
executor = ThreadPoolExecutor(MAX_WORKERS)

S3_BUCKET = os.environ['S3_BUCKET']
VISDOM_ENABLED = bool(os.environ.get('VISDOM_ENABLED', False))

# def blocking(func):
#     """Wraps the func in an async func, and executes the
#        function on `executor`."""
#     @wraps(func)
#     async def wrapper(self, *args, **kwargs):
#         fut = executor.submit(func, self, *args, **kwargs)
#         return yield to_tornado_future(fut)
#     return wrapper
if VISDOM_ENABLED:
    vis = visdom.Visdom(server='http://visdom.margffoy-tuay.com', port=80)


def forward(net, transform, refer, message):
    img = Image.open(BytesIO(base64.b64decode(message['b64_img'])))
    in_img = BytesIO()
    img.save(in_img, 'jpeg')
    in_img.seek(0)

    phrase = message['phrase']
    if VISDOM_ENABLED:
        vis.image(np.transpose(np.array(img), (2, 0, 1)))
    # mpimg.imsave('in.jpg', np.array(img))
    w, h = img.size
    img = transform(img)
    words = refer.tokenize_phrase(phrase)
    img = img.unsqueeze(0)
    words = words.unsqueeze(0)
    LOGGER.info("Words: {0}".format(words))
    if torch.cuda.is_available():
        img = img.cuda()
        words = words.cuda()
    with torch.no_grad():
        out = net(img, words)
        out = F.upsample(out, size=(h, w), mode='bilinear').squeeze()
        out = F.sigmoid(out)
    out = out.data.cpu().numpy()
    LOGGER.info("Data type: {0}".format(out.dtype))
    LOGGER.info("Max value: {0}".format(np.max(out)))
    LOGGER.info("Min value: {0}".format(np.min(out)))

    heatmap = out.copy()
    heatmap *= 255 / (np.max(out) - np.min(out))
    heatmap -= np.min(out)
    # heatmap = cv2.convertScaleAbs(
    #     out, 255 / (np.max(out) - np.min(out)), - np.min(out))
    heatmap = np.uint8(heatmap)
    if VISDOM_ENABLED:
        vis.image(heatmap)
    heatmap = cv2.applyColorMap(heatmap, cv2.COLORMAP_JET)
    heatmap = cv2.cvtColor(heatmap, cv2.COLOR_BGR2RGB)
    if VISDOM_ENABLED:
        vis.image(np.transpose(heatmap, (2, 0, 1)))

    if VISDOM_ENABLED:
        vis.image(out * 255, opts={'caption': phrase})

    # out_file = TemporaryFile()
    b64_enc = base64.b64encode(out.tostring())

    # pil_heatmap = Image.fromarray(np.uint8(cm.jet(out) * 255)[..., -1])
    pil_heatmap = Image.fromarray(heatmap)
    out_heatmap = BytesIO()
    pil_heatmap.save(out_heatmap, 'jpeg')
    out_heatmap.seek(0)

    s3 = boto3.client('s3')
    key ="{0}/{1}".format(message['device_id'], message['id'])
    s3.put_object(
        Bucket=S3_BUCKET,
        # Body=out_file,
        Body=b64_enc,
        Key=key + '.bin')

    s3.put_object(
        Bucket=S3_BUCKET,
        Body=in_img,
        # Body=base64.b64encode(out),
        Key=key + '.jpg')

    s3.put_object(
        Bucket=S3_BUCKET,
        Body=out_heatmap,
        # Body=base64.b64encode(out),
        Key=key + '_mask.jpg')
    # out = str(base64.b64encode(out), 'ascii')
    # with open('output_b64.txt', 'w') as f:
    #     f.write(out)
    return key, h, w


@tornado.gen.coroutine
def on_message(mq, net, transform, refer, message):
    LOGGER.info(message['phrase'])
    _id = message['id']
    mask, h, w = yield executor.submit(forward, net, transform, refer, message)
    payload = {
        "id": _id,
        "server": APP,
        'device_id': message['device_id'],
        'processed_at': int(time.time()),
        'phrase': message['phrase'],
        "mask": mask,
        "width": w,
        "height": h,
        "place": message['place'],
        "address": message['address'],
        "latitude": message['latitude'],
        "longitude": message['longitude']
    }
    mq.send_message(payload, EXCHANGE, ROUTING_KEY)
