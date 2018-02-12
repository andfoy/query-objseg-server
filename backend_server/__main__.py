# -*- coding: utf-8 -*-
# !/usr/bin/env python

"""LangVisNet backend Python server."""

# Standard lib imports
import os
import logging
import argparse

# Tornado imports
import tornado.web
import tornado.ioloop

# Other library imports
import coloredlogs

# Local imports
from backend_server.routes import ROUTES
from backend_server.listeners import LISTENERS
from backend_server.amqp.client import ExampleConsumer

parser = argparse.ArgumentParser(
    description='Query Segmentation Network backend server')

# Dataloading-related settings
parser.add_argument('--data', type=str, default='../referit_data',
                    help='path to ReferIt splits data folder')
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

parser.add_argument('--amqp', type=str,
                    help='AMQP url endpoint used to locate frontend service')

# AMQP_URL = ('amqp://langvis_server:eccv2018-textseg@margffoy-tuay.com:5672/'
#             'queryobjseg')

LOG_FORMAT = ('%(levelname) -10s %(asctime)s %(name) -30s %(funcName) '
              '-35s %(lineno) -5d: %(message)s')
LOGGER = logging.getLogger(__name__)
coloredlogs.install(level='info')

clr = 'clear'
if os.name == 'nt':
    clr = 'cls'


def main():
    logging.basicConfig(level=logging.DEBUG, format=LOG_FORMAT)
    settings = {"static_path": os.path.join(
        os.path.dirname(__file__), "static")}
    application = tornado.web.Application(
        ROUTES, debug=True, serve_traceback=True, autoreload=True,
        **settings)
    print("Server is now at: 127.0.0.1:8000")
    ioloop = tornado.ioloop.IOLoop.instance()

    outq = ExampleConsumer(LOGGER, AMQP_URL, LISTENERS)
    application.outq = outq

    application.outq.connect()
    application.listen(8000)
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
