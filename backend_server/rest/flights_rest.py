# -*- coding: iso-8859-15 -*-

import os
import sys
import json
import tornado.web
import tornado.escape
import logic.tm as tm
import models.model as model

class MainHandler(tornado.web.RequestHandler):
    def initialize(self, db=None):
        self.db = db
    
    def prepare(self):
        if self.request.headers["Content-Type"].startswith("application/json"):
           print "Got JSON"
           self.json_args = json.loads(self.request.body)
        else:
           self.json_args = None

    @tornado.gen.coroutine
    def get(self):
        self.set_status(403)

    @tornado.gen.coroutine
    def post(self):
        print(self.request)
        draw = self.json_args['draw']
        start = self.json_args['start']+1
        length = self.json_args['length']
        order = self.json_args['order'][0]['dir']
        ind_column = self.json_args['order'][0]['column']
        col_name = self.json_args['columns'][ind_column]['data']

        search_data = {}
        search_data['from'] = self.json_args['from']
        search_data['to'] = self.json_args['to']
        search_data['day'] = self.json_args['day']

        if len(filter(lambda x: x != None, search_data.values())) < 3:
           search_data = None

        flights, total_num, filtered = yield tm.list_flights(search_data, col_name, start, length, order)
        response = {'draw':draw,
                    'recordsTotal':total_num,
                    'recordsFiltered':filtered,
                    'data':flights}
        # response = json.dumps(response, default=utils.json_serial)
        # print countries
        # countries = "?"
        self.set_header('Content-Type', 'text/javascript;charset=utf-8')
        self.write(model.Model(response).json())
