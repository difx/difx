/***************************************************************************
 *   Copyright (C) 2008 by Walter Brisken                                  *
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

#include "difxio/difx_write.h"

int writeDifxLine(FILE *out, const char *key, const char *value)
{
	char line[1024];
	int i;

	for(i = 0; i < 32 && key[i]; i++)
	{
		line[i] = key[i];
	}
	line[i++] = ':';
	while(i < 20)
	{
		line[i++] = ' ';
	}
	value -= i;
	for(; i < 1023 && value[i]; i++)
	{
		line[i] = value[i];
	}
	line[i++] = 0;
	fprintf(out, "%s\n", line);

	return 0;
}

int writeDifxLineInt(FILE *out, const char *key, int value)
{
	char v[32];

	sprintf(v, "%d", value);

	return writeDifxLine(out, key, v);
}

int writeDifxLineInt1(FILE *out, const char *key, int i1, int value)
{
	char v[32];
	char k[128];

	sprintf(v, "%d", value);

	sprintf(k, key, i1);
	
	return writeDifxLine(out, k, v);
}

int writeDifxLine1(FILE *out, const char *key, int i1, const char *value)
{
	char k[128];

	sprintf(k, key, i1);
	
	return writeDifxLine(out, k, value);
}

int writeDifxLine2(FILE *out, const char *key, int i1, int i2, 
	const char *value)
{
	char k[128];

	sprintf(k, key, i1, i2);
	
	return writeDifxLine(out, k, value);
}

int writeDifxLine3(FILE *out, const char *key, int i1, int i2, int i3, 
	const char *value)
{
	char k[128];

	sprintf(k, key, i1, i2, i3);
	
	return writeDifxLine(out, k, value);
}
