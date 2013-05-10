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
    
    /*
     * Create a string of Encapsulated PostScript code that will draw the current DrawObject.
     */
    public String postScriptDraw() {
        return postScriptDraw( true );
    }
    
    /*
     * Create a string of Encapsulated PostScript code that will draw the current DrawObject.
     * If "fitToPage" is true, the object will be measured in screen space and then scaled
     * such that it fits on a piece of paper.  Otherwise it just translates screen pixels
     * into PostScript units (72 per inch).
     */
    public String postScriptDraw( boolean fitToPage ) {
        DrawObject.PrintParameters printParameters = new DrawObject.PrintParameters();
        //  Fit this on a page.
        double xScale = 1.0;
        double yScale = 1.0;
        if ( fitToPage ) {
            xScale = (double)this.getWidth() / 612.0;
            yScale = (double)this.getHeight() / 792.0;
            if ( xScale > yScale )
                yScale = xScale;
            else
                xScale = yScale;
        }
        String str = new String( "" );
        str += "%!PS-Adobe-3.0 EPSF-3.0\n";
        double W = (double)this.getWidth();
        double H = (double)this.getHeight();
        if ( fitToPage ) {
            W = W / xScale;
            H = H / yScale;
        }
        str += "%%BoundingBox: 0 " + ( 792.0 - H ) + " " + W + " 792\n";
        str += "%%Creator:\n";
        str += "%%CreationDate: \n";
        str += "%%EndComments\n";
        str += "/Helvetica findfont 12 scalefont setfont\n";  // temporary!
        printParameters.fontSize = 12.0;
        //  This stuff to make the top left of the PS area the origin (and y values
        //  offset DOWN).
        str += "/y {-1 mul} def\n";
        //  Other defines...
        str += "/m {y moveto} def\n";
        str += "/rm {y rmoveto} def\n";
        str += "/l {y lineto} def\n";
        str += "/t {y translate} def\n";
        str += "/s {gsave} def\n";
        str += "/r {grestore} def\n";
        str += "0 792 translate\n";
        if ( fitToPage )
            str += 1.0 / xScale + " " + 1.0 / yScale + " scale\n";
        str += drawObject.postScriptDraw( printParameters, null, false, false );
        str += "showpage\n";
        str += "%%EOF\n";
        return str;
    }
    
    DrawObject drawObject;
   
}
