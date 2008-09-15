/***************************************************************************
 *   Copyright (C) 2007 by Walter Brisken and Adam Deller                  *
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
#include <difxmessage.h>

// Initialize 7 alert streams
Alert cfatal(DIFX_ALERT_LEVEL_FATAL);
Alert csevere(DIFX_ALERT_LEVEL_SEVERE);
Alert cerror(DIFX_ALERT_LEVEL_ERROR);
Alert cwarn(DIFX_ALERT_LEVEL_WARNING);
Alert cinfo(DIFX_ALERT_LEVEL_INFO);
Alert cverbose(DIFX_ALERT_LEVEL_VERBOSE);
Alert cdebug(DIFX_ALERT_LEVEL_DEBUG);


Alert& Alert::sendAlert()
{
	// Send alert to appropriate place
	difxMessageSendDifxAlert(alertString.str().c_str(), alertLevel);

	// Reset alert stream
	alertString.str("");

	return *this;
}
