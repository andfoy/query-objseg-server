# -*- coding: utf-8 -*-

# Standard lib imports
import base64
import logging
from collections import Iterable

# Other imports
import tornado.gen
import numpy as np

# Torch imports
import torch
import torch.nn.functional as F
from torch.autograd import Variable
from torchvision.transforms import Compose, ToTensor, Normalize

# LangVisNet imports
from langvisnet import LangVisUpsample
from langvisnet.utils import ResizeImage

LOGGER = logging.getLogger(__name__)


REQUEST = 'REQUEST'
REQUEST_ANSWER = 'REQUEST_ANSWER'
EXCHANGE = 'queryobj'
ROUTING_KEY = 'query.answers'
results = {}


transform = Compose([
    ToTensor(),
    ResizeImage(512),
    Normalize(
        mean=[0.485, 0.456, 0.406],
        std=[0.229, 0.224, 0.225])
])


@tornado.gen.coroutine
def on_message(mq, message):
    _id = message['id']
    LOGGER.info(message['phrase'])
    mq.send_message(
        "Some message", EXCHANGE, ROUTING_KEY, REQUEST_ANSWER, _id)
