/*
 * Extension of the NumberBox class to make it handle only powers of 2.
 */
package mil.navy.usno.widgetlib;
import java.awt.event.MouseWheelEvent;

public class Power2NumberBox extends NumberBox {

    public Power2NumberBox() {
        precision( 0 );
        minimum( 2.0 );
    }

    public void mouseWheelMoved( MouseWheelEvent e ) {
        if ( e.getWheelRotation() > 0 )
            checkNewValue( value() * 2.0 * (double)( e.getWheelRotation() ) );
        else
            checkNewValue( value() / ( 2.0 * (double)( -e.getWheelRotation() ) ) );
    }

    protected void upKey() {
        checkNewValue( value() * 2.0 );
    }

    protected void downKey() {
        checkNewValue( value() / 2.0 );        
    }

}


