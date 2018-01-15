# -*- coding: utf-8 -*-

import os
import sys
import jsonpickle

class Model(object):
    """
    Base class definition for JSON/SQL object conversion,
    it allows to serialize and deserialize an object
    coming from/going to a REST consumer.
    """
    def __init__(self, entries):
        """
        Model initialization based on a default set of
        parameters defined inside a dictionary, it converts
        from JSON representation to a Python object
        """
        self.__dict__.update(entries)
    
    def json(self):
        """
        Serialize the model and converts into a JSON
        compatible object
        """
        return jsonpickle.encode(self, unpicklable=False)

    def sql(self, keys):
        """
        Returns a list of values ordered by a key list,
        associated to column names in a relation (Table)

        Parameters
        ----------
        keys: a list of column names which allow to order the object values
            list - Contains strings
        """
        return [self.__dict__[k].encode('utf-8') if isinstance(self.__dict__[k], unicode) else self.__dict__[k] for k in keys]

    @classmethod
    def from_json(cls, entries):
        """
        Create an instance of a Model object from a JSON object

        Parameters
        ----------
        entries: A dictionary which describes the object in JSON
        notation
        """
        return cls(entries)


