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

#define MAX_DIFX_MESSAGE_IDENTIFER 128

extern char difxMessageGroup[16];
extern int difxMessagePort;
extern char difxMessageIdentifier[MAX_DIFX_MESSAGE_IDENTIFER];
extern char difxMessageHostname[32];
extern int difxMessageMpiProcessId;
extern char difxMessageXMLFormat[256];
extern int difxMessageSequenceNumber;

#endif
