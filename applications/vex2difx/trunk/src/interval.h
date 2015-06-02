/***************************************************************************
 *   Copyright (C) 2015 by Walter Brisken                                  *
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
 * $HeadURL: https://svn.atnf.csiro.au/difx/applications/vex2difx/trunk/src/util.h $
 * $LastChangedRevision$
 * $Author$
 * $LastChangedDate$
 *
 *==========================================================================*/

#ifndef __INTERVAL_H__
#define __INTERVAL_H__

class Interval
{
public:
	double mjdStart;
	double mjdStop;

	Interval(double start=0.0, double end=0.0) : mjdStart(start), mjdStop(end) {}
	Interval(const Interval &vi) : mjdStart(vi.mjdStart), mjdStop(vi.mjdStop) {}
	double duration() const { return mjdStop-mjdStart; }
	double duration_seconds() const { return 86400.0*(mjdStop-mjdStart); }
	double overlap(const Interval &v) const;
	double overlap_seconds(const Interval &v) const { return 86400.0*overlap(v); }
	double center() const { return 0.5*(mjdStart+mjdStop); }
	void shift(double deltaT) { mjdStart += deltaT; mjdStop += deltaT; }
	void setTimeRange(double start, double stop) { mjdStart = start; mjdStop = stop; }
	void setTimeRange(const Interval &v) { mjdStart = v.mjdStart; mjdStop = v.mjdStop; }
	void logicalAnd(double start, double stop);
	void logicalAnd(const Interval &v);
	void logicalOr(double start, double stop);
	void logicalOr(const Interval &v);
	bool contains(double mjd) const { return (mjdStart <= mjd) && (mjd <= mjdStop); }
	bool contains(const Interval &v) const { return (mjdStart <= v.mjdStart) && (mjdStop >= v.mjdStop); }
	bool containsAbsolutely(double mjd) const { return (mjdStart < mjd) && (mjd < mjdStop); }
	bool containsAbsolutely(const Interval &v) const { return (mjdStart < v.mjdStart) && (mjdStop > v.mjdStop); }
	bool isCausal() const { return (mjdStart <= mjdStop); }
};

#endif
