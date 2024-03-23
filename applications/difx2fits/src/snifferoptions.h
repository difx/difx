/***************************************************************************
 *   Copyright (C) 2024 by Walter Brisken                                  *
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
#ifndef __SNIFFEROPTIONS_H__
#define __SNIFFEROPTIONS_H__

extern const long long int DefaultSnifferMaxMemory;
extern const double DefaultSnifferSolutionInterval;
extern const double DefaultSnifferBandpassInterval;

typedef struct
{
	double solutionInterval;	/* [sec]; solution interval for APD, XCB and ACB records; -1 means no sniffing */
	double bandpassInterval;	/* [min]; minimum time between writing XCB and ACB records */
	long long int maxMemory;
	int writeBandpass;
} SnifferOptions;

SnifferOptions *newSnifferOptions();

void resetSnifferOptions(SnifferOptions *sOpts);

void deleteSnifferOptions(SnifferOptions *sOpts);

void printSnifferOptions(const SnifferOptions *sOpts);

#endif
