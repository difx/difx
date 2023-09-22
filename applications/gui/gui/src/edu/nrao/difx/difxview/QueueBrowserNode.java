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
package edu.nrao.difx.difxview;

import mil.navy.usno.widgetlib.BrowserNode;

/**
 *
 * @author jspitzak
 */
public class QueueBrowserNode extends BrowserNode {
    
    public QueueBrowserNode( String name ) {
        super( name );
    }

    public boolean found() { return _found; }
    public void found( boolean newVal ) { _found = newVal; }
        
    public void id( int newVal ) { _id = newVal; }
    public Integer id() { return _id; }
    
    public void inDatabase( boolean newVal ) { _inDataBase = newVal; }
    public boolean inDatabase() { return _inDataBase; }
    
    protected boolean _persist;
    protected boolean _found;
    protected Integer _id;
    protected boolean _inDataBase;
    
}
