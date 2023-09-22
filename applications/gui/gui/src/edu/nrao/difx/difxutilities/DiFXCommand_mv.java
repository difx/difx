/***************************************************************************
 *   Copyright (C) 2016 by John Spitzak                                    *
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
package edu.nrao.difx.difxutilities;

import edu.nrao.difx.difxview.SystemSettings;

import edu.nrao.difx.xmllib.difxmessage.DifxFileOperation;

/**
 *
 * @author jspitzak
 */
public class DiFXCommand_mv extends DiFXCommand {
    
    public DiFXCommand_mv( String from, String to, SystemSettings settings ) {
        super( settings );
        this.header().setType( "DifxFileOperation" );
        DifxFileOperation mv = this.factory().createDifxFileOperation();
        mv.setPath( from );
        mv.setOperation( "mv" );
        mv.setArg( to );
        //  The "data" node is assumed to be the same as the DiFX "control" node
        //  (at least for now).
        mv.setDataNode( settings.difxControlAddress() );
        this.body().setDifxFileOperation( mv );
    }
    
}
