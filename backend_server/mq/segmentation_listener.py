# -*- coding: utf-8 -*-

# Standard lib imports
import time
import base64
import logging
from functools import wraps

# Torch imports
import torch
import torch.nn.functional as F
from torch.autograd import Variable

# Tornado imports
import tornado.gen
from concurrent.futures import ThreadPoolExecutor

# Other imports
import visdom
import numpy as np
from PIL import Image
from io import BytesIO
import matplotlib.image as mpimg

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


# def blocking(func):
#     """Wraps the func in an async func, and executes the
#        function on `executor`."""
#     @wraps(func)
#     async def wrapper(self, *args, **kwargs):
#         fut = executor.submit(func, self, *args, **kwargs)
#         return yield to_tornado_future(fut)
#     return wrapper

vis = visdom.Visdom(server='http://visdom.margffoy-tuay.com', port=80)


def forward(net, transform, refer, message):
    img = Image.open(BytesIO(base64.b64decode(message['b64_img'])))
    phrase = message['phrase']
    vis.image(np.transpose(np.array(img), (2, 0, 1)))
    # mpimg.imsave('in.jpg', np.array(img))
    h, w = img.size
    img = transform(img)
    words = refer.tokenize_phrase(phrase)
    img = Variable(img, volatile=True).unsqueeze(0)
    words = Variable(words, volatile=True).unsqueeze(0)
    if torch.cuda.is_available():
        img = img.cuda()
        words = words.cuda()
    out = net(img, words)
    out = F.upsample(out, size=(h, w), mode='bilinear').squeeze()
    out = F.sigmoid(out)
    out = out.data.cpu().numpy() * 255
    vis.image(out)
    out = str(base64.b64encode(out), 'utf-8')
    return out


@tornado.gen.coroutine
def on_message(mq, net, transform, refer, message):
    LOGGER.info(message['phrase'])
    _id = message['id']
    mask = yield executor.submit(forward, net, transform, refer, message)
    payload = {
        "id": _id,
        "server": APP,
        'device_id': message['device_id'],
        'processed_at': int(time.time()),
        "mask": mask
    }
    mq.send_message(payload, EXCHANGE, ROUTING_KEY)
