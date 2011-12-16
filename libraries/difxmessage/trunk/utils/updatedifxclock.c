/***************************************************************************
 *   Copyright (C) 2010 by Adam Deller                                     *
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
 * $Id: sendtransient.c 2788 2010-11-12 23:04:31Z WalterBrisken $
 * $HeadURL: https://svn.atnf.csiro.au/difx/libraries/difxmessage/trunk/utils/sendtransient.c $
 * $LastChangedRevision: 2788 $
 * $Author: WalterBrisken $
 * $LastChangedDate: 2010-11-12 16:04:31 -0700 (Fri, 12 Nov 2010) $
 *
 *==========================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include "difxmessage.h"

int main(int argc, char **argv)
{
	if(argc != 2)
	{
		printf("\nUsage: %s antennaindex:clockpoly0[:clockpoly1:clockpoly2...]\n\n", argv[0]);

		return EXIT_SUCCESS;
	}

	difxMessageInit(-1, argv[0]);
	difxMessagePrint();
	difxMessageSendDifxParameter("clockupdate", argv[1], DIFX_MESSAGE_ALLMPIFXCORR);

	return EXIT_SUCCESS;
}
