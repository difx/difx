/***************************************************************************
 *   Copyright (C) 2007-2011 by Walter Brisken                             *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate$
//
//============================================================================
#include <stdio.h>
#include "../difxmessage.h"
#include "../config.h"
#include "difxmessageinternal.h"

/* Note! Keep this in sync with enum Mk5Status in difxmessage.h */
const char Mk5StateStrings[][24] = 
{
	"Opening",
	"Open",
	"Close",
	"GetDirectory",
	"GotDirectory",
	"Play",
	"Idle",
	"Error",
	"Busy",
	"Initializing",
	"Resetting",
	"Rebooting",
	"PowerOff",
	"NoData",
	"NoMoreData",
	"PlayInvalid",
	"PlayStart",
	"Copy",
	"Condition",
	"CondError",
	"Test",
	"TestWrite",
	"TestRead",
	"Booting"
};

/* Note! Keep this in sync with enum DifxStatus in difxmessage.h */
const char DifxStateStrings[][24] =
{
	"Spawning",
	"Starting",
	"Running",
	"Ending",
	"Done",
	"Aborting",
	"Terminating",
	"Terminated",
	"MpiDone",
	"Crashed"
};

/* Note! Keep this in sync with enum DifxMessageType in difxmessage.h */
const char DifxMessageTypeStrings[][24] =
{
	"Unknown",
	"DifxLoadMessage",
	"DifxAlertMessage",
	"Mark5StatusMessage",
	"DifxStatusMessage",
	"DifxInfoMessage",
	"DifxDatastreamMessage",
	"DifxCommand",
	"DifxParameter",
	"DifxStart",
	"DifxStop",
	"Mark5VersionMessage",
	"Mark5ConditionMessage",
	"DifxTransientMessage"
};

/* Note! Keep this in sync with enum DifxAlertLevel in difxmessage.h */
const char difxMessageAlertString[][16] =
{
	"FATAL",
	"SEVERE",
	"ERROR",
	"WARNING",
	"INFO",
	"INFO",
	"DEBUG"
};

int isDifxMessageInUse()
{
	return difxMessageInUse;
}

const char *difxMessageGetVersion()
{
	return PACKAGE_STRING " svn: $HeadURL$ $Revision$";
}
