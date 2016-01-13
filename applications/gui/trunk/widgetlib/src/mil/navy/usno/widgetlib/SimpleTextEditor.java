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
 * This class is used to produce a text editor widget that does basic things -
 * insert, delete, copy, paste, cut, etc. all using control keys.  Most of this
 * is handled by the JTextArea, but some things have to be added.
 */
package mil.navy.usno.widgetlib;

import java.awt.Font;
import javax.swing.JTextArea;
import javax.swing.JScrollPane;

/**
 *
 * @author jspitzak
 */
public class SimpleTextEditor extends JScrollPane {
    
    public SimpleTextEditor() {
        super();
        _textArea = new JTextArea();
        _textArea.setFont( new Font( "Courier", Font.PLAIN, 14 ) );//_textArea.getFont().getSize() ) );
        this.setViewportView( _textArea );
    }
    
    public JTextArea textArea() { return _textArea; }
    
    public void text( String newText ) {
        _textArea.setText( newText );
    }
    public void top() {
        _textArea.setSelectionStart( 0 );
        _textArea.setSelectionEnd( 0 );
    }
    public String text() { 
        String ret = _textArea.getText();
        return ret;
    }
    public void addText( String newText ) {
        _textArea.append( newText );
    }
    
    protected JTextArea _textArea;
    
}
