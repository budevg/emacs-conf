import os

class Node(object):
    def __init__(self, name, elements = [], **kwargs):
        self.__dict__.update(kwargs)
        self._name = name
        if not hasattr(self, "_parent"):
            self.set_parent(None)
        if not hasattr(self, "_elements"):
            self._elements = elements[:]
        for element in self:
            element.set_parent(self)
            

    def __iter__(self):
        for element in self._elements:
            yield element

    def __repr__(self):
        return "%s(%s)" % (self.__class__.__name__, self.get_path())
    
    def set_parent(self, parent):
        self._parent = parent

    def get_path(self):
        path_hierarchy = []
        node = self
        while 1:
            path_hierarchy.insert(0,node._name)
            if node._parent is None:
                break
            node = node._parent
        return os.path.abspath(os.path.join(*path_hierarchy))


class Package(Node):
    pass    

class Module(Node):
    pass

class Bunch(object):
    def __init__(self, **kwargs):
        self.__dict__.update(kwargs)
        
class Screen(Bunch):
    def __init__(self, **kwargs):
        super(Screen, self).__init__(**kwargs)
        
