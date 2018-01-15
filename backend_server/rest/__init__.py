# -*- coding: utf-8 -*-

"""
rest module
=========

Provides:
    1. Asynchronous execution of JSON services
    2. Asynchronous execution of Web Rendering

How to use the documentation
----------------------------
Documentation is available in one form: docstrings provided
with the code

Copyright (c) 2016, Edgar A. Margffoy.
MIT, see LICENSE for more details.
"""

import os
import sys
#New submodules defined inside this module must be imported here
import countries_rest
import flights_rest
import users_rest
import videos_rest

__version__ = '1.0.0'

sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))
