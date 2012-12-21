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
    
    protected static String _version = "1.01";
    
}
