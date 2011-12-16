/***************************************************************************
 *   Copyright (C) 2010 by Walter Brisken                                  *
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
 * $HeadURL$
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include "difxmessage.h"

int main(int argc, char **argv)
{
	DifxMessageTransient transient;
	
	if(argc < 5)
	{
		printf("\nUsage: %s startMJD stopMJD priority identifier [outputDir]\n\n", argv[0]);

		return EXIT_SUCCESS;
	}

	difxMessageInit(-1, argv[0]);
	difxMessagePrint();

	transient.startMJD = atof(argv[1]);
	transient.stopMJD = atof(argv[2]);
	transient.priority = atof(argv[3]);
	transient.dm = -1.5;
	snprintf(transient.jobId, DIFX_MESSAGE_IDENTIFIER_LENGTH, "%s", argv[4]);
	if(argc > 4)
	{
		snprintf(transient.destDir, DIFX_MESSAGE_FILENAME_LENGTH, "%s", argv[5]);
	}
	else
	{
		snprintf(transient.destDir, DIFX_MESSAGE_FILENAME_LENGTH, "%s", "/tmp/");
	}
	snprintf(transient.comment, DIFX_MESSAGE_COMMENT_LENGTH, "%s", "from sendtransient");

	difxMessageSendDifxTransient(&transient);

	return EXIT_SUCCESS;
}
