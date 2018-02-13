# -*- coding: utf-8 -*-

# Standard lib imports
import time
import base64
import logging

# Other imports
import tornado.gen
from PIL import Image
from io import BytesIO

# Local imports
from backend_server.amqp import APP
from backend_server.__main__ import forward


LOGGER = logging.getLogger(__name__)


REQUEST = 'REQUEST'
REQUEST_ANSWER = 'REQUEST_ANSWER'
EXCHANGE = 'queryobj'
ROUTING_KEY = 'query.answers'
results = {}


@tornado.gen.coroutine
def on_message(mq, message):
    LOGGER.info(message['phrase'])
    _id = message['id']
    img = Image.open(BytesIO(base64.b64decode(message['b64_img'])))
    phrase = message['phrase']
    mask = yield forward(img, phrase)
    mask = base64.b64encode(mask.numpy())
    payload = {
        "id": _id,
        "server": APP,
        'device_id': message['device_id'],
        'processed_at': int(time.time()),
        "mask": mask
    }
    mq.send_message(payload, EXCHANGE, ROUTING_KEY)
