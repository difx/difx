#include <stdio.h>
#include "../difxmessage.h"

/* Note! Keep this in sync with enum Mm5Status in difxmessage.h */
const char Mk5StatusStrings[][24] = 
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
