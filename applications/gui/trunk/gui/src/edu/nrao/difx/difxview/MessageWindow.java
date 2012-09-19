/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxview;

import javax.swing.JFrame;
import mil.navy.usno.widgetlib.MessageDisplayPanel;

/**
 *
 * @author jspitzak
 */
public class MessageWindow extends JFrame {
    
    public MessageWindow( String name ) {
        super( name );
        _messagePanel = new MessageDisplayPanel();
        this.add( _messagePanel );
    }
    
    public MessageDisplayPanel messagePanel() { return _messagePanel; }
    
    MessageDisplayPanel _messagePanel;
}
