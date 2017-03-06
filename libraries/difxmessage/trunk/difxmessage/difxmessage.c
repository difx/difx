/***************************************************************************
 *   Copyright (C) 2007-2012 by Walter Brisken                             *
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
#include <strings.h>
#include "../difxmessage.h"
#include "../config.h"
#include "difxmessageinternal.h"

/* Note! Keep this in sync with enum Mark6Status in difxmessage.h */
const char Mark6StateStrings[][24] = 
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
	"Booting",
	"Recording",
	"CopyTo"
};

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
	"Booting",
	"Recording",
	"CopyTo",
	"FuseMk5"
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
	"Mark6StatusMessage",
	"Mark5StatusMessage",
	"DifxStatusMessage",
	"DifxInfoMessage",
	"DifxDatastreamMessage",
	"DifxCommand",
	"DifxParameter",
	"DifxStart",
	"DifxStop",
	"Mark5VersionMessage",
	"Mark5ConditionMessage", /* this is deprecated; use Mark5DriveStatsMessage instead */
	"DifxTransientMessage",
	"DifxSmartMessage",
	"Mark5DriveStatsMessage",
	"DifxDiagnosticMessage",
	"DifxFileTransfer",
	"DifxFileOperation",
	"DifxVex2DifxRun",
	"DifxMachinesDefinition",
	"DifxGetDirectory",
	"DifxMk5Control",
	"DifxMark5Copy",
	"DifxVsis",
};

/* Note! Keep this in sync with enum DifxDiagnosticType in difxmessage.h */
const char DifxDiagnosticStrings[][24] = 
{
	"MemoryUsage",
	"BufferStatus",
	"ProcessingTime",
	"DataConsumed",
	"InputDatarate",
	"NumSubintsLost"
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

/* Note! Keep this in sync with enum DriveStatsType in difxmessage.h */
const char DriveStatsTypeStrings[][24] =
{
	"condition",
	"condition_read",
	"condition_write",
	"read",
	"write",
	"unknown",	/* Really, this should never be used! */
	"test"		/* Testing the send mechansim */
};

/* Note! Keep this in sync with enum DifxStartFunctionType in difxmessage.h */
const char DifxStartFunctionString[][24] =
{
	"unknown",
	"default",
	"NRAO",
	"USNO",
	"RUN_MONITOR",
	"CONFIG_ONLY",
	"BAIL_ON_CONFIG_FAIL"
};

int isDifxMessageInUse()
{
	return difxMessageInUse;
}

int isDifxMessageUnicast()
{
	return difxMessageUnicast;
}

/* does case insensitive comparison */
enum DriveStatsType stringToDriveStatsType(const char *str)
{
	enum DriveStatsType t;

	for(t = 0; t < NUM_DRIVE_STATS_TYPES; t++)
	{
		if(strcasecmp(str, DriveStatsTypeStrings[t]) == 0)
		{
			return t;
		}
	}

	return DRIVE_STATS_TYPE_UNKNOWN;
}

const char *difxMessageGetVersion()
{
	return PACKAGE_STRING " svn: $HeadURL$ $Revision$";
}
