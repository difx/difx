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
/*
 * This window displays the software version and any other stuff we want.  It is
 * the official keeper of the version number(s).
 */
package edu.nrao.difx.difxview;

import javax.swing.JFrame;
import javax.swing.JLabel;

public class VersionWindow extends JFrame {
    
    public VersionWindow() {
        super();
        this.setLayout( null );
        JLabel label = new JLabel();
        label.setText( "USNO DiFX GUI Version " + _version );
        this.add( label );
        label.setBounds( 20, 20, 200, 50 );
    }
    
    public static String version() { return _version; }
    
    protected static String _version = "DEVEL r7283";
    
}
