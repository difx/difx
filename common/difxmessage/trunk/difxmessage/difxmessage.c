#include <stdio.h>
#include "../difxmessage.h"

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
	"Error"
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
	"Info",
	"MpiDone"
};
