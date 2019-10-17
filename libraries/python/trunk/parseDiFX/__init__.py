# -*- coding: utf-8 -*-
#===========================================================================
# Copyright (C) 2019  Max-Planck-Institut für Radioastronomie, Bonn, Germany
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
#===========================================================================
# SVN properties (DO NOT CHANGE)
#
# $Id$
# $HeadURL: $
# $LastChangedRevision$
# $Author$
# $LastChangedDate$
#
#============================================================================

__version__ = '1.0.0'
__author__ = "Jan Wagner, Adam Deller"
__name__ = "parseDiFX"

# Common functions used by existing scripts that imported parseDiFX.py
from .Common import \
	parse_output_header, make_output_header_v1, \
	get_common_settings, get_telescopetable_info, get_configtable_info, \
	get_baselinetable_info, get_datastreamtable_info, get_freqtable_info, \
	put_baselinetable_info, put_datastreamtable_info, put_freqtable_info

# DiFX .input
from .Config import Config
from .Freq import Freq
from .Telescope import Telescope
from .Baseline import Baseline
from .Datastream import Datastream

# DiFX full set: .input and .difx/DIFX_*
from .InputFile import InputFile
from .DiFXFile import DiFXFile
from .VisibilityHeader import VisibilityHeader
from .VisibilityRecord import VisibilityRecord
