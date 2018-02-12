
import os
import sys
import json
import logging
import tornado.gen
from langvisnet import LangVisUpsample
# import logic.dtm as dtm
LOGGER = logging.getLogger(__name__)

# print sys.modules
timeout = 5

REQUEST = 'REQUEST'
REQUEST_ANSWER = 'REQUEST_ANSWER'
EXCHANGE = 'queryobj'
ROUTING_KEY = 'query.answers'
# GENERAL_KEY = 'videos.general'

results = {}


@tornado.gen.coroutine
def on_message(mq, message):
    _id = message['id']
    LOGGER.info(message['phrase'])
    mq.send_message(
        "Some message", EXCHANGE, ROUTING_KEY, REQUEST_ANSWER, _id)
