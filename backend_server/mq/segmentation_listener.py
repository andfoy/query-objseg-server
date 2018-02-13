# -*- coding: utf-8 -*-

# Standard lib imports
import time
import base64
import logging
from functools import wraps

# Tornado imports
import tornado.gen
from concurrent.futures import ThreadPoolExecutor
from tornado.platform.asyncio import to_tornado_future

# Other imports
from PIL import Image
from io import BytesIO

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


def forward(net, message):
    img = Image.open(BytesIO(base64.b64decode(message['b64_img'])))
    phrase = message['phrase']
    mask = net(img, phrase)
    mask = base64.b64encode(mask.numpy())
    return mask


@tornado.gen.coroutine
def on_message(mq, net, message):
    LOGGER.info(message['phrase'])
    _id = message['id']
    mask = yield executor.submit(forward, net, message)
    payload = {
        "id": _id,
        "server": APP,
        'device_id': message['device_id'],
        'processed_at': int(time.time()),
        "mask": mask
    }
    mq.send_message(payload, EXCHANGE, ROUTING_KEY)
