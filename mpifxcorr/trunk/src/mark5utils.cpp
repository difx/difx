/***************************************************************************
 *   Copyright (C) 2008-2013 by Walter Brisken                             *
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
/*===========================================================================
 * SVN properties (DO NOT CHANGE)
 *
 * $Id: mark5utils.cpp 5453 2013-06-28 02:13:50Z WalterBrisken $
 * $HeadURL: $
 * $LastChangedRevision: 5453 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2013-06-27 20:13:50 -0600 (Thu, 27 Jun 2013) $
 *
 *==========================================================================*/

#include <cstdio>
#include <difxmessage.h>
#include "mark5dir.h"

int dirCallback(int scan, int nscan, int status, void *data)
{
	const int MessageLength=200;
	char message[MessageLength];
	int v;
	DifxMessageMk5Status *mk5status;

	mk5status = reinterpret_cast<DifxMessageMk5Status *>(data);
	mk5status->scanNumber = scan + 1;
	mk5status->position = nscan;
	v = snprintf(mk5status->scanName, MODULE_SCAN_NAME_LENGTH, "%s", Mark5DirDescription[status]);
	if(v >= MessageLength)
	{
		fprintf(stderr, "Warning: dirCallback: scanName: v=%d >= %d\n", v, MODULE_SCAN_NAME_LENGTH);
	}

	difxMessageSendMark5Status(mk5status);

	if(status == MARK5_DIR_READ_ERROR)
	{
		v = snprintf(message, MessageLength, "XLR read error in decoding of scan %d\n", scan+1);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
	}
	else if(status == MARK5_DIR_DECODE_ERROR)
	{
		v = snprintf(message, MessageLength, "cannot decode scan %d\n", scan+1);
		difxMessageSendDifxAlert(message, DIFX_ALERT_LEVEL_ERROR);
	}

	if(v >= MessageLength)
	{
		fprintf(stderr, "Warning: dirCallback: message: v=%d, >= %d\n", v, MessageLength);
	}

	return 0;
}

bool legalVSN(const char *vsn)
{
	int nSep = 0;

	for(int i = 0; i < 8; ++i)
	{
		if(vsn[i] == '+' || vsn[i] == '-')
		{
			if(nSep > 0 || i == 0 || i == 7)
			{
				return false;
			}
			++nSep;
		}
		else if(isalpha(vsn[i]))
		{
			if(nSep != 0)
			{
				return false;
			}
		}
		else if(isdigit(vsn[i]))
		{
			if(nSep != 1)
			{
				return false;
			}
		}
		else
		{
			return false;
		}
	}

	if(nSep != 1)
	{
		return false;
	}

	return true;
}
