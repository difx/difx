import UserDict

class MultiDict(UserDict.DictMixin):

    def __init__(self, *args, **kwds):
        self._items = []
        if len(args) > 1:
            msg = "__init__() takes at most 2 arguments (%d given)" \
                  % (len(args) + 1)
            raise TypeError, msg
        if args:
            if hasattr(args[0], 'items'):
                items = args[0].items()
            else:
                items = list(args[0])
                pass
            self.extend(items)
            pass
        
        self.extend(list(kwds.iteritems()))
        pass

    def __len__(self):
        return len(self._items)

    def __getitem__(self, key):
        for pair in self._items:
            if pair[0] == key:
                return pair[1]
            continue
        raise KeyError, repr(key)

    def __setitem__(self, key, value):
        self._items.append((key, value))
        pass

    def append(self, key, value):
        self._items.append((key, value))
        pass

    def extend(self, seq):
        index = 0
        for pair in seq:
            if len(seq) != 2:
                msg = "dictionary update sequence element #%d has" \
                      " length %d; 2 is required" % (index, len(pair))
                raise ValueError, msg
            self.append(pair[0], pair[1])
            index += 1
            continue
        pass

    def clear(self):
        self._items = []
        pass

    def __str__(self):
        s = '{'
        for pair in self._items:
            s += str(pair[0]) + ": '" + str(pair[1]) + "', "
            continue
        return s.strip(', ') + '}'

    def __delitem__(self, key):
        found = False
        for pair in self._items:
            if key == pair[0]:
                self._items.remove(pair)
                found = True
                pass
            continue
        if not found:
            raise KeyError, repr(key)
        pass

    def __contains__(self, key):
        for pair in self._items:
            if key == pair[0]:
                return True
            continue
        return False

    has_key = __contains__

    def getall(self, key, default=[]):
        values = []
        for pair in self._items:
            if key == pair[0]:
                values.append(pair[1])
                pass
            continue
        return values

    def __iter__(self):
        for pair in self._items:
            yield pair[0]
            continue
        pass

    pass
