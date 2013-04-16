/*
 * This is a simple frame used to draw "DrawObjects".
 */
package mil.navy.usno.plotlib;

import javax.swing.JPanel;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;

/**
 *
 * @author jspitzak
 */
public class DrawWindow extends JPanel {
    
    public void drawObject( DrawObject newObject ) {
        drawObject = newObject;
    }
    
    @Override
    public void paintComponent( Graphics g ) {
        super.paintComponent( g );
        Graphics2D g2 = (Graphics2D)g;  // Graphics2 for antialiasing.
        g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING,
                     RenderingHints.VALUE_ANTIALIAS_ON );
        //  Draw the current draw object if it has been set.
        if ( drawObject != null ) {
            drawObject.draw( g2, null, null, false );
        }
    }
    
    DrawObject drawObject;
   
}
