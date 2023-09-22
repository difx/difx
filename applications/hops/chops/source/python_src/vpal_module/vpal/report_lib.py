"""simple json serialization module for dumping report data"""

#core imports
from builtins import object

class JsonSerializableObject(object):
    """JsonSerializableObject, functions must be overloaded for complex types"""

    def export_json(self):
        return self.__dict__

    def import_json(self, json_dict):
        if isinstance(json_dict, dict):
            for key, val in list(json_dict.items()):
                if key in self.__dict__:
                    cast_val = type( getattr(self, key) )(val)
                    setattr(self, key, cast_val)

def NestedObjectEncoder(an_object):
    if hasattr(an_object, 'export_json'):
        return an_object.export_json()
    else:
        return an_object
