# -*- coding: iso-8859-15 -*-

"""
routes
======

This module establishes and defines the Web Handlers and Websockets
that are associated with a specific URL routing name. New routing
associations must be defined here.

Notes
-----
For more information regarding routing URL and valid regular expressions
visit: http://www.tornadoweb.org/en/stable/guide/structure.html
"""

import os
import sys
import web

# Define new rest associations
REST = []

# Define new web rendering route associations
WEB = [
    (r'/flights', web.flights_handler.MainHandler)
]

ROUTES = REST + WEB
