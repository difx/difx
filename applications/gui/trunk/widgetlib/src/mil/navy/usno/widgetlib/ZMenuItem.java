/*
 * Extension of the JMenuItem class to include ComplexToolTips.
 */
package mil.navy.usno.widgetlib;

import javax.swing.JMenuItem;
import javax.swing.text.JTextComponent;

import java.awt.Point;
import java.awt.event.MouseEvent;
import javax.swing.text.JTextComponent;
import javax.swing.JToolTip;

public class ZMenuItem extends JMenuItem {
    
    public ZMenuItem( String label ) {
        super( label );
    }

    @Override
    public JToolTip createToolTip() {
        _tip = new ComplexToolTip();
        _tip.setComponent( this );
        _tip.dynamicLinkPath( _dynamicLinkPath );
        return _tip;
    }
    @Override
    public Point getToolTipLocation( MouseEvent e) {
        return new Point( 10, getHeight() );
    }
    
    public ComplexToolTip toolTip() { return _tip; }
    
    /*
     * Allows the user of this widget to set a "dynamic path" for links in the
     * tooltip string.  See the ComplexToolTip class for details.
     */
    public void toolTipDynamicLinkPath( JTextComponent textField ) {
        _dynamicLinkPath = textField;
    }
    
    /*
     * A tooltip-setting function that includes the above.
     */
    public void toolTip( String str, JTextComponent textField ) {
        this.setToolTipText( str );
        this.toolTipDynamicLinkPath( textField );
    }

    ComplexToolTip _tip;
    JTextComponent _dynamicLinkPath;

}
