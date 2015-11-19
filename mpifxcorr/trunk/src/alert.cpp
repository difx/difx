/***************************************************************************
 *   Copyright (C) 2007-2015 by Walter Brisken and Adam Deller             *
 *                                                                         *
 *   This program is free for non-commercial use: see the license file     *
 *   at http://astronomy.swin.edu.au:~adeller/software/difx/ for more      *
 *   details.                                                              *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *
 ***************************************************************************/
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id$
// $HeadURL$
// $LastChangedRevision$
// $Author$
// $LastChangedDate: 2008-09-10 10:13:06 -0400 (Wed, 10 Sep 2008) $
//
//============================================================================

#include "alert.h"
#include "config.h"
#include <difxmessage.h>

// Initialize 7 alert streams.  Use these instead of cout and cerr.

Alert cfatal(DIFX_ALERT_LEVEL_FATAL);		// alert level 0
// A problem resulting in immediate termination of correlator
// Example: illegal configuration

Alert csevere(DIFX_ALERT_LEVEL_SEVERE);		// alert level 1
// A error that indicates possible program bug or OS instability
// Examples: bad status from vector routines or from mutex locks/unlocks

Alert cerror(DIFX_ALERT_LEVEL_ERROR);		// alert level 2
// An error that most certainly means loss of data
// Example: stale data received from a core node

Alert cwarn(DIFX_ALERT_LEVEL_WARNING);		// alert level 3
// An unexpected condition of minor consequence
// Examples: execution of untested modes or possible loss of some data

Alert cinfo(DIFX_ALERT_LEVEL_INFO);		// alert level 4
// Purely informational message indicating progress

Alert cverbose(DIFX_ALERT_LEVEL_VERBOSE);	// alert level 5
// Extra purely informational messages that 

Alert cdebug(DIFX_ALERT_LEVEL_DEBUG);		// alert level 6
// Overly verbose messages that would be of use only to developers

Alert& Alert::sendAlert()
{
	// Send alert to appropriate place
        if(alertLevel != DIFX_ALERT_LEVEL_DO_NOT_SEND)
        {
	  difxMessageSendDifxAlert(alertString.str().c_str(), alertLevel);
        }

	// Reset alert stream
	alertString.str("");

	return *this;
}
// vim: shiftwidth=2:softtabstop=2:expandtab
