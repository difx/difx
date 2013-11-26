# -*- coding: utf-8 -*-
#===========================================================================
# SVN properties (DO NOT CHANGE)
#
# $Id$
# $HeadURL$
# $LastChangedRevision$
# $Author$
# $LastChangedDate$
#
#============================================================================
class Module(object):
    pass
    
class Slot (object):
    pass
     
class Experiment(object):
    def __eq__(self, other):
        if isinstance(other, self.__class__):
            return self.__dict__ == other.__dict__
        else:
            return False

    def __ne__(self, other):
        return not self.__eq__(other)
    

class ExperimentStatus(object):
    pass

class Job(object):
    pass

class JobStatus(object):
    pass

class Pass(object):
    pass

class PassType(object):
    pass

class Queue(object):
    pass

class VersionHistory(object):
    pass

class ExperimentType(object):
    pass

class User(object):
    pass