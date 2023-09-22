/***************************************************************************
 *   Copyright (C) 2008-2012 by Walter Brisken                             *
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
 * $Id$
 * $HeadURL: https://svn.atnf.csiro.au/difx/libraries/mark5access/trunk/mark5access/mark5_stream.c $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "difxmessage.h"

int main(int argc, char **argv)
{
	char message[DIFX_MESSAGE_LENGTH];
	char *rv;
	int v;
	
	difxMessageInit(-1, argv[0]);
	difxMessagePrint();

	for(;;)
	{
		rv = fgets(message, DIFX_MESSAGE_LENGTH, stdin);
		if(!rv)
		{
			break;
		}
		
		message[DIFX_MESSAGE_LENGTH-1] = 0;
		v = difxMessageSend2(message, strlen(message));
		if(v < 0)
		{
			break;
		}
	}

	return EXIT_SUCCESS;
}
