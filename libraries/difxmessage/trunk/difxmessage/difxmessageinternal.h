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
#ifndef __DIFX_MESSAGE_INTERNAL_H__
#define __DIFX_MESSAGE_INTERNAL_H__

#include "../difxmessage.h"

extern char difxMessageGroup[16];
extern int difxMessagePort;
extern char difxMessageIdentifier[DIFX_MESSAGE_IDENTIFER_LENGTH];
extern char difxMessageHostname[DIFX_MESSAGE_PARAM_LENGTH];
extern int difxMessageMpiProcessId;
extern char difxMessageXMLFormat[256];
extern int difxMessageSequenceNumber;
extern char difxBinarySTAGroup[16];
extern int difxBinarySTAPort;
extern char difxBinaryLTAGroup[16];
extern int difxBinaryLTAPort;
extern int difxMessageInUse;

#endif
