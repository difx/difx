/***************************************************************************
 *   Copyright (C) 2010 by Adam Deller and Walter Brisken                  *
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
 * $Id: vlbadefaults.h 2002 2010-03-04 16:24:27Z AdamDeller $
 * $HeadURL:  $
 * $LastChangedRevision: 2002 $
 * $Author: AdamDeller $
 * $LastChangedDate: 2010-03-04 09:24:27 -0700 (Thu, 04 Mar 2010) $
 *
 *==========================================================================*/
#ifndef __VLBADEFAULTS_H__
#define __VLBADEFAULTS_H__

#include <string>

const int  MAX_DBE = 2;
const int  MAX_IF_PER_DBE = 2;
const int  MAX_IF = MAX_DBE*MAX_IF_PER_DBE;
const int  MAX_DBE_CHAN=4;
const int  MAX_DBE_PFB_CHAN=16;

const int  DBE_0 = 0;
const int  DBE_1 = 1;

const int  IN_0 = 0;
const int  IN_1 = 1;

const int CHANDIST_NONE  = 0;
const int CHANDIST_1IF_1 = 1;
const int CHANDIST_1IF_2 = 2;
const int CHANDIST_2IF_1 = 3;
const int CHANDIST_2IF_2 = 4;
const int CHANDIST_2IF_3 = 5;
const int CHANDIST_3IF_1 = 6;
const int CHANDIST_3IF_2 = 7;
const int CHANDIST_4IF_1 = 8;

const std::string VLBAantennas[] = {"BR", "FD", "HN", "KP", "LA", "MK", "NL", "OV", "PT", "SC", ""};   // terminate list with "" !


#endif
