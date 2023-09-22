/***************************************************************************
 *   Copyright (C) 2007-2014 by Walter Brisken                             *
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
// $Id: difxmessageinternal.h 7219 2016-02-11 02:22:01Z ChrisPhillips $
// $HeadURL: https://svn.atnf.csiro.au/difx/master_tags/DiFX-2.8.1/libraries/difxmessage/difxmessage/difxmessageinternal.h $
// $LastChangedRevision: 7219 $
// $Author: ChrisPhillips $
// $LastChangedDate: 2016-02-11 10:22:01 +0800 (四, 2016-02-11) $
//
//============================================================================
#ifndef __DIFX_MESSAGE_INTERNAL_H__
#define __DIFX_MESSAGE_INTERNAL_H__

#include "../difxmessage.h"

#define DIFX_MESSAGE_FORMAT_LENGTH    320
#define DIFX_MESSAGE_FILENAME_TAG_LENGTH	(DIFX_MESSAGE_FILENAME_LENGTH + 32)

extern char difxMessageGroup[16];
extern int difxMessagePort;
extern char difxMessageIdentifier[DIFX_MESSAGE_PARAM_LENGTH];
extern char difxMessageHostname[DIFX_MESSAGE_PARAM_LENGTH];
extern int difxMessageMpiProcessId;
extern char difxMessageXMLFormat[DIFX_MESSAGE_FORMAT_LENGTH];
extern char difxMessageInputFilenameTag[DIFX_MESSAGE_FILENAME_TAG_LENGTH];
extern char difxMessageToXMLFormat[DIFX_MESSAGE_FORMAT_LENGTH];
extern int difxMessageSequenceNumber;
extern char difxBinarySTAGroup[16];
extern int difxBinarySTAPort;
extern char difxBinaryLTAGroup[16];
extern int difxBinaryLTAPort;
extern int difxMessageInUse;
extern int difxMessageUnicast;

#endif
