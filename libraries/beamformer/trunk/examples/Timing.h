/***************************************************************************
 *   Copyright (C) 2011 by Jan Wagner                                      *
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

#ifndef _TIMING_H
#define _TIMING_H

#include <sys/time.h>

#include <iostream>

namespace bf {

class Timing {

  private:

     Timing();

  public:

     /**
      * Begin taking time required to process Nelem elements.
      * @param[in] Nelem Number of elements processed while Timing object alive
      */
     Timing(double Nelem) { _Nelem=Nelem; start(); }

     /**
      * Stop taking time.
      */
     ~Timing() { 
        double throughput = stop();
        std::cout << "Timing object d'stor reached. Time: " << _dT << ", throughput: " << throughput << " elems/sec\n";
     }

  public:

     /**
      * Stop taking time.
      * @param[out] dT Reference for storing final time delta in seconds
      * @return Throughput of Nelem elements per second.
      */
     double stop(double& dT) { 
        if (!_running) {
           dT = 0.0;
           return 0.0;
        }
        gettimeofday(&_tv_stop, NULL);
        _dT = (_tv_stop.tv_sec - _tv_start.tv_sec) + 1e-6*(_tv_stop.tv_usec - _tv_start.tv_usec);
        dT = _dT;
        return (_Nelem/_dT);
     }

     /** 
      * Stop taking time.
      * @return Throughput of Nelem elements per second.
      */
     double stop() {
        double tmp;
        return stop(tmp);
     }

     /**
      * Start or restart taking time.
      */
     void start() {
         _running = true;
         _dT = 0;
         gettimeofday(&_tv_start, NULL);
     }

  private:

     bool _running;
     double _Nelem;
     double _dT;

     struct timeval _tv_stop;
     struct timeval _tv_start;
};

} // namespace bf

#endif // _BEAMFORMER_DATA_H
