# -*- coding: utf-8 -*-
#===========================================================================
# Copyright (C) 2016  Max-Planck-Institut für Radioastronomie, Bonn, Germany
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
from difxdb.model import model

def getUserByName(session, userName):
    '''
    Returns the User object that has the given name
    '''
    if userName == "":
        return None
    
    return(session.query(model.User).filter_by(name=userName).one())

def getEnabledUsers(session):   
    '''
    Returns a collection of all enabled User objects
    '''
    
    return(session.query(model.User).filter_by(enabled=1).order_by("id").all())
