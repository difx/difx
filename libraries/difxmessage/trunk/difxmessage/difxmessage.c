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
	"CondError"
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
	"MpiDone"
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
	"Mark5ConditionMessage"
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

const char *difxMessageGetVersion()
{
	return PACKAGE_STRING " svn: $HeadURL$ $Revision$";
}
