class Module(object):
    pass
    #__tablename__ = 'Module'
    #id = Column(Integer, primary_key=True)
    #parent_id = Column(Integer, ForeignKey('Slot.id'))
    
    
    
class Slot (object):
    pass
    #__tablename__ = 'Slot'
    #id = Column(Integer, primary_key=True)
    #module_id = Column(Integer, ForeignKey('Module.id'))
    #child = relation("Module", uselist=False, backref="Slot")

    
    
class Experiment(object):
    pass

class ExperimentStatus(object):
    pass

