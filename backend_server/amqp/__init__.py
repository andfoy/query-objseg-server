# -*- coding: utf-8 -*-

"""
ampq module
=========

Provides:
    1. Asynchronous interface for connecting to a RDBMS service
    2. Asynchronous execution of SQL transactions and queries

How to use the documentation
----------------------------
Documentation is available in one form: docstrings provided
with the code

Copyright (c) 2016, Edgar A. Margffoy.
MIT, see LICENSE for more details.
"""

import os
import sys
import uuid
import client


APP = str(uuid.uuid1())

__version__ = '1.0.0'
__all__ = ["client"]

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))
