# -*- coding: utf-8 -*-

import os
import sys
import model
import datetime

class User(model.Model):
    def __init__(self, entries):
        super(User, self).__init__(entries)

    @classmethod
    def from_json(cls, entries):
        entries["birth_day"] = datetime.datetime.strptime(entries['birth_day'], '%d/%m/%Y').date()
        return super(User, cls).from_json(entries)
