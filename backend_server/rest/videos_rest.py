# -*- coding: iso-8859-15 -*-

import os
import sys
import json
import tornado.web
import tornado.escape
import logic.tm as tm

class MainHandler(tornado.web.RequestHandler):
    def initialize(self, db=None):
        self.db = db

    @tornado.gen.coroutine
    def get(self):
        countries = yield tm.get_videos(self.application.outq)
        # print countries
        # countries = "?"
        self.set_header('Content-Type', 'text/javascript;charset=utf-8')
        self.write(tornado.escape.json_encode(countries))

    @tornado.gen.coroutine
    def post(self):
        self.status_code(403)