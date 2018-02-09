
import os
import sys
import json
# import logic.dtm as dtm

# print sys.modules
timeout = 5

REQUEST = 'REQUEST'
REQUEST_ANSWER = 'REQUEST_ANSWER'
EXCHANGE = 'videos.test'
ROUTING_KEY = 'videos.general.app2'
GENERAL_KEY = 'videos.general'

results = {}


@tornado.gen.coroutine
def on_message(mq, _id, _from, status, to, message):
    if _from != 'app2':
        if status == REQUEST:
            local_videos = yield dtm.get_local_videos()
            mq.send_message(local_videos, EXCHANGE, ROUTING_KEY, to, REQUEST_ANSWER, _id)
        elif status == REQUEST_ANSWER:
            if _id in results:
                results[_id].append(message)