package mil.navy.usno.widgetlib;

/*
 * Tooltip that accepts embedded characters for specific text settings to build
 * tooltips with multiple lines, multicolored text, and links.  This was
 * adapted from:
 * 
 *    http://www.java2s.com/Code/Java/Swing-JFC/MultiLineToolTipExample.htm 
 * 
 * To use:
 *            JButton button = new JButton("Hello, world") {
 *                public JToolTip createToolTip() {
 *                    MultiLineToolTip tip = new MultiLineToolTip();
 *                    tip.setComponent(this);
 *                    return tip;
 *                }
 *            };
 *            button.setToolTipText("Hello\nworld");
 */

import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.font.LineMetrics;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.Enumeration;
import java.util.Vector;
import java.awt.Color;
import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;

import javax.swing.JComponent;
import javax.swing.JToolTip;
import javax.swing.SwingUtilities;
import javax.swing.plaf.metal.MetalToolTipUI;


public class ComplexToolTip extends JToolTip implements MouseListener {
    public ComplexToolTip() {
        setUI( new ComplexToolTipUI() );
        setBackground( new Color( (float)0.960, (float)0.900, (float)0.675 ) );
        addMouseListener( this );
    }

    @Override
    public void mouseExited( MouseEvent e ) {
    }
    
    @Override
    public void mouseEntered( MouseEvent e ) {
    }
    
    @Override
    public void mouseReleased( MouseEvent e ) {
    }
    
    @Override
    public void mousePressed( MouseEvent e ) {
    }
    
    @Override
    public void mouseClicked( MouseEvent e ) {
        System.out.println( e.getX() + "  " + e.getY() );
    }
    
}
class ComplexToolTipUI extends MetalToolTipUI {
    private String[] strs;

    private int maxWidth = 0;

    public void paint( Graphics g, JComponent c ) {
        Graphics2D g2 = (Graphics2D)g;
        g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING,
              RenderingHints.VALUE_ANTIALIAS_ON );
        Dimension size = c.getSize();
        g2.setColor( c.getBackground() );
        g2.fillRect( 0, 0, size.width, size.height );
        g2.setColor( c.getForeground() );
        LineMetrics metrics = g2.getFont().getLineMetrics( "Foo", g2.getFontRenderContext() );
        if ( strs != null ) {
            for ( int i = 0; i < strs.length; i++ ) {
                g2.drawString( strs[i], 3, metrics.getHeight() * (i + 1) );
            }
        }
    }

    public Dimension getPreferredSize( JComponent c ) {
        FontMetrics metrics = c.getFontMetrics( c.getFont() );
        //  Break the tooltip text string into components that have positions, colors,
        //  and potential links.
        String tipText = ( (JToolTip)c ).getTipText();
        if ( tipText == null ) {
            tipText = "";
        }
        else {
            boolean done = false;
            //System.out.println( ">>>>\n" + tipText + "\n<<<<" );
            int ptr = 0;
            int strLen = tipText.length();
            while ( ptr < strLen ) {
                //  Locate the next control.
                int newline = tipText.indexOf( '\n', ptr );
                int control = tipText.indexOf( "<<", ptr );
                String textBite = "";
                //  No controls at all - keep the whole string.
                if ( newline == -1 && control == -1 ) {
                    textBite = tipText.substring( ptr, tipText.length() );
                    ptr = tipText.length();
                }
                //  Next is a formating command.
                else if ( newline == -1 ) {
                    textBite = tipText.substring( ptr, control );
                }
                //  Next must be a newline.
                else {
                    textBite = tipText.substring( ptr, newline );
                    ptr = newline + 1;
                }
                //System.out.println( "LINE: \"" + textBite + "\"" );
            }
        }
        BufferedReader br = new BufferedReader( new StringReader( tipText ) );
        String line;
        int maxWidth = 0;
        Vector v = new Vector();
        try {
            while ( ( line = br.readLine() ) != null ) {
                int width = SwingUtilities.computeStringWidth( metrics, line );
                maxWidth = ( maxWidth < width ) ? width : maxWidth;
                v.addElement( line );
            }
        } catch ( IOException ex ) {
            ex.printStackTrace();
        }
        int lines = v.size();
        if ( lines < 1 ) {
            strs = null;
            lines = 1;
        } else {
            strs = new String[lines];
            int i = 0;
            for ( Enumeration e = v.elements(); e.hasMoreElements(); i++ ) {
                strs[i] = (String)e.nextElement();
            }
        }
        int height = metrics.getHeight() * lines;
        this.maxWidth = maxWidth;
        return new Dimension( maxWidth + 6, height + 4 );
    }


}

