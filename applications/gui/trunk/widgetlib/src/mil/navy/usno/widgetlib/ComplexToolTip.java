package mil.navy.usno.widgetlib;

/*
 * Tooltip that accepts embedded characters for specific text settings to build
 * tooltips with multiple lines, multicolored text, links, and other stuff using
 * a fairly simple set of embedded text formatting commands.  This code was
 * originally adapted from:
 * 
 *    http://www.java2s.com/Code/Java/Swing-JFC/MultiLineToolTipExample.htm 
 * 
 * Although it bears little resemblance to it now, credit where credit is due.
 * 
 * The following in-text commands are recognized:
 * 
 *          \n    Insert a line break in the text.
 *   <<color>>    Make all subsequent text to a specified color, which can be any one of:
 *                BLACK, BLUE, CYAN, DARK_GRAY, GRAY, GREEN, LIGHT_GRAY,
 *                MAGENTA, ORANGE, PINK, RED, WHITE, or YELLOW.
 * <<COLOR=#>>    Specify the color of subsequent text in RGB format with R, G, and
 *                B values comma separated floating point values between 0 and 1.0
 *                or integer values between 0 and 255.
 *  <</COLOR>>    "Shut off" specified colors and make subsequent text the defined
 *                foreground color.  A properly formatted pair of color commands
 *                would look like this: 
 *                      background-colored text <<BLUE>>blue text<</COLOR>> background again
 *    <<BOLD>>    Make following text bold.
 *   <</BOLD>>    Turn off bold.
 *  <<ITALIC>>    Make following text italic.
 * <</ITALIC>>    Turn off italic
 *   <<FIXED>>    Change to fixed-width font.
 *    <<SANS>>    Change to sans-serif font.
 *   <<SERIF>>    Change to serif font.
 *   <<UNDER>>    Underline subsequent text.
 *  <</UNDER>>    Cease underlining.
 *  <<SIZE=#>>    Change the font size, either to a integer value (no decimal point)
 *                number of pixels, or by the given floating point (containing decimal
 *                point) multiple.  A value of 4 would make the fontsize 4 pixels,
 *                while a value of 4.0 would make it four times its previously-set
 *                size (whatever that was).
 *  <<LINE=#>>    Change the line spacing, either to an integer number of pixels or
 *                a multiple of the current font size.  Note that line spacing is
 *                implemented when a newline character is encountered.
 *     <<X=#>>    Change the X position of subsequent text by an integer number of
 *                pixels or a floating point multiple of the font size.  Not sure what
 *                value this has exactly - it is included for completeness.
 *     <<Y=#>>    Change the Y position of subsequent text by an integer number of
 *                pixels (negative shifts things up!) or a floating point multiple
 *                of the font size.  Can be used for super/subscripts.
 * <<LINK=ST>>    Make subsequent text a hypertext link to the given string.  Clicking
 *                on this text while the tooltip is displayed will cause the
 *                current browser to display the given link using whatever practices
 *                or conventions are normal for your installation.  The "linked"
 *                text does not appear different from other text (unless you make
 *                it so - color it blue, underline it, etc.).
 *  <</LINK>>     Mark the end of text that will be a link.
 * 
 * These commands can appear anywhere in the text, although it probably makes sense
 * for them to be first:
 * 
 * <<BG=COLOR>>   Set the background color, where the COLOR string can be any of
 *                the COLOR values above or "R,G,B" where R, G, and B are floating
 *                point values between 0.0 and 1.0 or integers between 0 and 255.
 * <<FG=COLOR>>   Same deal for the foreground color (which is the default text
 *                color).
 * 
 * To use:
 *            JButton button = new JButton("Hello, world") {
 *                public JToolTip createToolTip() {
 *                    ComplexToolTip tip = new ComplexToolTip();
 *                    tip.setComponent(this);
 *                    return tip;
 *                }
 *                //  This controls where the tooltip appears - you may need it
 *                //  to assure that you can mouse over it if you have links.
 *                public Point getToolTipLocation( MouseEvent e) {
 *                   return new Point( 10, getHeight() );
 *                }
 *            };
 *            button.setToolTipText( "This button says:\n<<bold>>Hello, world<</bold>");
 * 
 * One rather important thing to note...there is limited error checking done here,
 * so if you put strange text-formatting commands in your text you will get strange,
 * possibly crash-inducing behavior.  Testing tooltips during debugging is an
 * obvious way to avoid this, although dynamically-created tooltips introduce hazards
 * that can't be discovered this way.
 * 
 * SOME DETAILS WHEN USING LINKS
 * 
 * A facility is provided for using a "dynamic" root to links embedded in the text.
 * This takes the form of a JTextField widget (or any of its derivatives), the
 * identity of which is passed when creating the tool tip.  An example follows:
 * 
 *            JTextField _dynamicLinkField;
 *            JButton button = new JButton("Hello, world") {
 *                public JToolTip createToolTip() {
 *                    ComplexToolTip tip = new ComplexToolTip();
 *                    tip.setComponent(this);
 *                    tip.dynamicLinkPath( _dynamicLinkField );
 *                    return tip;
 *                }
 *                //  This controls where the tooltip appears - you may need it
 *                //  to assure that you can mouse over it if you have links.
 *                public Point getToolTipLocation( MouseEvent e) {
 *                   return new Point( 10, getHeight() );
 *                }
 *            };
 *            button.setToolTipText( "This button says:\n<<bold>>Hello, world<</bold>\n" +
 *                                   "Documention is <<link=/hello_world.html>>here<</link>>" );
 * 
 * When a user clicked on the link included in this tooltip, the specified string
 * "/hello_world.html" would be appended to the text in the widget "_dynamicTextField". 
 * If this text were to change during further operation of the GUI, the next click
 * on the tooltip would reflect this change.
*/

import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.util.ArrayDeque;
import java.util.Iterator;
import java.awt.Font;
import java.awt.Color;
import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;
import java.text.AttributedString;
import java.awt.font.TextAttribute;

import javax.swing.JComponent;
import javax.swing.text.JTextComponent;
import javax.swing.JToolTip;
import javax.swing.SwingUtilities;
import javax.swing.plaf.metal.MetalToolTipUI;


public class ComplexToolTip extends JToolTip implements MouseListener {
    
    protected ComplexToolTipUI _toolTipUI;
    
    public ComplexToolTip() {
        _toolTipUI = new ComplexToolTipUI();
        setUI( _toolTipUI );
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
    
    /*
     * Mouse click positions are checked against any links the user may have installed.
     */
    @Override
    public void mouseClicked( MouseEvent e ) {
        _toolTipUI.checkClick( e.getX(), e.getY() );
    }
    
    /*
     * See ComplextToolTipUI!
     */
    public void dynamicLinkPath( JTextComponent textField ) {
        _toolTipUI.dynamicLinkPath( textField );
    }

}

class ComplexToolTipUI extends MetalToolTipUI {
    private String[] strs;
    Color backgroundColor;
    Color foregroundColor;

    //private int maxWidth = 0;

    public void paint( Graphics g, JComponent c ) {
        Graphics2D g2 = (Graphics2D)g;
        g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING,
              RenderingHints.VALUE_ANTIALIAS_ON );
        Dimension size = c.getSize();
        if ( backgroundColor != null )
            g2.setColor( backgroundColor );
        else
            g2.setColor( c.getBackground() );
        g2.fillRect( 0, 0, size.width, size.height );
        if ( foregroundColor != null )
            g2.setColor( foregroundColor );
        else
            g2.setColor( c.getForeground() );
        boolean underline = false;
        if ( _textItems == null )
            return;
        for ( Iterator<TextItem> iter = _textItems.iterator(); iter.hasNext(); ) {
            TextItem nextItem = iter.next();
            if ( nextItem.text != null ) {
                if ( underline ) {
                    AttributedString as = new AttributedString( nextItem.text );
                    as.addAttribute( TextAttribute.FONT, g2.getFont() );
                    as.addAttribute( TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_ON, 0, nextItem.text.length() );
                    g2.drawString( as.getIterator(), nextItem.x, nextItem.y );  
                }
                else
                    g2.drawString( nextItem.text, nextItem.x, nextItem.y );
            }
            if ( nextItem.format != null ) {
                //  Do whatever formatting activities are required by this format
                //  instruction.
                if ( nextItem.format.equalsIgnoreCase( "RED" ) )
                    g2.setColor( Color.RED );
                else if ( nextItem.format.equalsIgnoreCase( "BLACK" ) ) 
                    g2.setColor( Color.BLACK );
                else if ( nextItem.format.equalsIgnoreCase( "BLUE" ) ) 
                    g2.setColor( Color.BLUE );
                else if ( nextItem.format.equalsIgnoreCase( "CYAN" ) ) 
                    g2.setColor( Color.CYAN );
                else if ( nextItem.format.equalsIgnoreCase( "DARK_GRAY" ) ) 
                    g2.setColor( Color.DARK_GRAY );
                else if ( nextItem.format.equalsIgnoreCase( "GRAY" ) ) 
                    g2.setColor( Color.GRAY );
                else if ( nextItem.format.equalsIgnoreCase( "GREEN" ) ) 
                    g2.setColor( Color.GREEN );
                else if ( nextItem.format.equalsIgnoreCase( "LIGHT_GRAY" ) ) 
                    g2.setColor( Color.LIGHT_GRAY );
                else if ( nextItem.format.equalsIgnoreCase( "MAGENTA" ) ) 
                    g2.setColor( Color.MAGENTA );
                else if ( nextItem.format.equalsIgnoreCase( "ORANGE" ) ) 
                    g2.setColor( Color.ORANGE );
                else if ( nextItem.format.equalsIgnoreCase( "PINK" ) ) 
                    g2.setColor( Color.PINK );
                else if ( nextItem.format.equalsIgnoreCase( "RED" ) ) 
                    g2.setColor( Color.RED );
                else if ( nextItem.format.equalsIgnoreCase( "WHITE" ) ) 
                    g2.setColor( Color.WHITE );
                else if ( nextItem.format.equalsIgnoreCase( "YELLOW" ) ) 
                    g2.setColor( Color.YELLOW );
                else if ( nextItem.format.length() > 5 && nextItem.format.substring( 0, 5 ).equalsIgnoreCase( "COLOR" ) ) {
                    //  We need three numbers...
                    String colorString = nextItem.format.substring( nextItem.format.indexOf( '=' ) + 1 ).trim();
                    String str = colorString.substring( 0, colorString.indexOf( ',' ) ).trim();
                    float R;
                    if ( str.contains( "." ) )
                        R = Float.parseFloat( str );
                    else
                        R = (float)(Integer.parseInt( str ))/(float)256.0;
                    colorString = colorString.substring( colorString.indexOf( ',' ) + 1 ).trim();
                    str = colorString.substring( 0, colorString.indexOf( ',' ) ).trim();
                    float G;
                    if ( str.contains( "." ) )
                        G = Float.parseFloat( str );
                    else
                        G = (float)(Integer.parseInt( str ))/(float)256.0;
                    str = colorString.substring( colorString.indexOf( ',' ) + 1 ).trim();
                    float B;
                    if ( str.contains( "." ) )
                        B = Float.parseFloat( str );
                    else
                        B = (float)(Integer.parseInt( str ))/(float)256.0;
                    g2.setColor( new Color( R, G, B ) );
                }
                else if ( nextItem.format.equalsIgnoreCase( "/COLOR" ) ) {
                    if ( foregroundColor != null )
                        g2.setColor( foregroundColor );
                    else
                        g2.setColor( c.getForeground() );
                }
                else if ( nextItem.format.equalsIgnoreCase( "BOLD" ) ) {
                    Font f = g2.getFont();
                    if ( f.isPlain() )
                        g2.setFont( new Font( f.getFontName(), Font.BOLD, f.getSize() ) );
                    else if ( f.isItalic() )
                        g2.setFont( new Font( f.getFontName(), Font.ITALIC | Font.BOLD, f.getSize() ) );
                }
                else if ( nextItem.format.equalsIgnoreCase( "/BOLD" ) ) {
                    Font f = g2.getFont();
                    if ( f.isItalic() )
                        g2.setFont( new Font( f.getFontName(), Font.ITALIC, f.getSize() ) );
                    else
                        g2.setFont( new Font( f.getFontName(), Font.PLAIN, f.getSize() ) );
                }
                else if ( nextItem.format.equalsIgnoreCase( "ITALIC" ) ) {
                    Font f = g2.getFont();
                    if ( f.isPlain() )
                        g2.setFont( new Font( f.getFontName(), Font.ITALIC, f.getSize() ) );
                    else if ( f.isBold() )
                        g2.setFont( new Font( f.getFontName(), Font.ITALIC | Font.BOLD, f.getSize() ) );
                }
                else if ( nextItem.format.equalsIgnoreCase( "/ITALIC" ) ) {
                    Font f = g2.getFont();
                    if ( f.isBold() )
                        g2.setFont( new Font( f.getFontName(), Font.BOLD, f.getSize() ) );
                    else
                        g2.setFont( new Font( f.getFontName(), Font.PLAIN, f.getSize() ) );
                }
                else if ( nextItem.format.equalsIgnoreCase( "UNDER" ) || nextItem.format.equalsIgnoreCase( "UNDERLINE" ) ) 
                    underline = true;
                else if ( nextItem.format.equalsIgnoreCase( "/UNDER" ) || nextItem.format.equalsIgnoreCase( "/UNDERLINE" ) ) 
                    underline = false;
                else if ( nextItem.format.equalsIgnoreCase( "FIXED" ) ) {
                    Font f = g2.getFont();
                    if ( f.isBold() && f.isItalic() )
                        g2.setFont( new Font( "Monospaced", Font.ITALIC | Font.BOLD, f.getSize() ) );
                    else if ( f.isBold() )
                        g2.setFont( new Font( "Monospaced", Font.BOLD, f.getSize() ) );
                    else if ( f.isItalic() )
                        g2.setFont( new Font( "Monospaced", Font.ITALIC, f.getSize() ) );
                    else
                        g2.setFont( new Font( "Monospaced", Font.PLAIN, f.getSize() ) );
                }
                else if ( nextItem.format.equalsIgnoreCase( "SANS" ) ) {
                    Font f = g2.getFont();
                    if ( f.isBold() && f.isItalic() )
                        g2.setFont( new Font( "SansSerif", Font.ITALIC | Font.BOLD, f.getSize() ) );
                    else if ( f.isBold() )
                        g2.setFont( new Font( "SansSerif", Font.BOLD, f.getSize() ) );
                    else if ( f.isItalic() )
                        g2.setFont( new Font( "SansSerif", Font.ITALIC, f.getSize() ) );
                    else
                        g2.setFont( new Font( "SansSerif", Font.PLAIN, f.getSize() ) );
                }
                else if ( nextItem.format.equalsIgnoreCase( "SERIF" ) ) {
                    Font f = g2.getFont();
                    if ( f.isBold() && f.isItalic() )
                        g2.setFont( new Font( "Serif", Font.ITALIC | Font.BOLD, f.getSize() ) );
                    else if ( f.isBold() )
                        g2.setFont( new Font( "Serif", Font.BOLD, f.getSize() ) );
                    else if ( f.isItalic() )
                        g2.setFont( new Font( "Serif", Font.ITALIC, f.getSize() ) );
                    else
                        g2.setFont( new Font( "Serif", Font.PLAIN, f.getSize() ) );
                }
                else if ( nextItem.format.substring( 0, 4 ).equalsIgnoreCase( "SIZE" ) ) {
                    //  Parse out the size value...
                    String sizeString = nextItem.format.substring( nextItem.format.indexOf( '=' ) + 1 ).trim();
                    Font f = g2.getFont();
                    int newSize = f.getSize();
                    if ( sizeString.contains( "." ) )
                        newSize = (int)( Double.parseDouble( sizeString ) * (double)newSize );
                    else
                        newSize = Integer.parseInt( sizeString );
                    if ( f.isBold() && f.isItalic() )
                        g2.setFont( new Font( f.getFontName(), Font.ITALIC | Font.BOLD, newSize ) );
                    else if ( f.isBold() )
                        g2.setFont( new Font( f.getFontName(), Font.BOLD, newSize ) );
                    else if ( f.isItalic() )
                        g2.setFont( new Font( f.getFontName(), Font.ITALIC, newSize ) );
                    else
                        g2.setFont( new Font( f.getFontName(), Font.PLAIN, newSize ) );
                }
            }
        }
    }

    public Dimension getPreferredSize( JComponent c ) {
        FontMetrics metrics = c.getFontMetrics( c.getFont() );
        //  Break the tooltip text string into components that have positions, colors,
        //  and potential links.
        String tipText = ( (JToolTip)c ).getTipText();
        if ( tipText == null || tipText.contentEquals( "" ) ) {
            return new Dimension( 0, 0 );
        }
        else {
            boolean done = false;
            if ( _textItems == null )
                _textItems = new ArrayDeque<TextItem>();
            _textItems.clear();
            if ( _linkItems == null )
                _linkItems = new ArrayDeque<LinkItem>();
            _linkItems.clear();
            int ptr = 0;
            int strLen = tipText.length();
            Integer y = null;
            Double dLine = null;
            Integer iLine = null;
            String link = null;
            int x = 3;
            int maxWidth = 0;
            Font f = c.getFont();
            //  Break the text for this tooltip into segments of text and interspersed
            //  controls.
            while ( ptr < strLen ) {
                //  Locate the next control (there may not be one).
                int newline = tipText.indexOf( '\n', ptr );
                int control = tipText.indexOf( "<<", ptr );
                //  No controls at all - keep the whole string.
                if ( newline == -1 && control == -1 ) {
                    TextItem newItem = new TextItem( tipText.substring( ptr, tipText.length() ) );
                    newItem.x = x;
                    if ( y == null ) {
                        y = metrics.getHeight();
                        if ( dLine != null )
                            y = (int)( dLine * (double)y );
                        else if ( iLine != null )
                            y = iLine;
                    }
                    newItem.y = y;
                    newItem.format = null;
                    _textItems.add( newItem );
                    if ( link != null ) {
                        LinkItem newLink = new LinkItem( link );
                        newLink.x = x;
                        newLink.y = y - metrics.getHeight();
                        newLink.y2 = y;
                        newLink.x2 = x + SwingUtilities.computeStringWidth( metrics, tipText.substring( ptr, tipText.length() ) );
                        _linkItems.add( newLink );
                    }
                    x += SwingUtilities.computeStringWidth( metrics, tipText.substring( ptr, tipText.length() ) );
                    maxWidth = ( maxWidth < x ) ? x : maxWidth;
                    ptr = tipText.length();
                }
                //  Next is a formating command.
                else if ( newline == -1 || ( control > -1 && control < newline ) ) {
                    //  Don't bother with this stuff if this string is empty.
                    if ( tipText.substring( ptr, control ).length() > 0 ) {
                        TextItem newItem = new TextItem( tipText.substring( ptr, control ) );
                        newItem.x = x;
                        if ( y == null ) {
                            y = metrics.getHeight();
                            if ( dLine != null )
                                y = (int)( dLine * (double)y );
                            else if ( iLine != null )
                                y = iLine;
                        }
                        newItem.y = y;
                        newItem.format = null;
                        if ( link != null ) {
                            LinkItem newLink = new LinkItem( link );
                            newLink.x = x;
                            newLink.y = y - metrics.getHeight();
                            newLink.y2 = y;
                            newLink.x2 = x + SwingUtilities.computeStringWidth( metrics, tipText.substring( ptr, control ) );
                            _linkItems.add( newLink );
                        }
                        _textItems.add( newItem );
                        x += SwingUtilities.computeStringWidth( metrics, tipText.substring( ptr, control ) );
                        maxWidth = ( maxWidth < x ) ? x : maxWidth;
                    }
                    //  Extract the formatting command and advance the text pointer to the end of it.
                    int endControl = tipText.indexOf( ">>", control );
                    //  If for some reason this control is not complete, assume it is meant to be
                    //  text.
                    if ( endControl == -1 ) {
                        TextItem newItem = new TextItem( "<<" );
                        newItem.x = x;
                        if ( y == null ) {
                            y = metrics.getHeight();
                            if ( dLine != null )
                                y = (int)( dLine * (double)y );
                            else if ( iLine != null )
                                y = iLine;
                        }
                        newItem.y = y;
                        newItem.format = null;
                        _textItems.add( newItem );
                        if ( link != null ) {
                            LinkItem newLink = new LinkItem( link );
                            newLink.x = x;
                            newLink.y = y - metrics.getHeight();
                            newLink.y2 = y;
                            newLink.x2 = x + SwingUtilities.computeStringWidth( metrics, "<<" );
                            _linkItems.add( newLink );
                        }
                        x += SwingUtilities.computeStringWidth( metrics, "<<" );
                        maxWidth = ( maxWidth < x ) ? x : maxWidth;
                        ptr = control + 2;
                    }
                    //  If this is a normal control, make a new (empty) text item with the control.
                    else {
                        TextItem newItem = new TextItem( null );
                        newItem.format = tipText.substring( control + 2, endControl );
                        _textItems.add( newItem );
                        ptr = endControl + 2;
                        //  Some controls require font changes which need to be implemented
                        //  here in a temporary way to change the metrics.
                        if ( newItem.format.equalsIgnoreCase( "BOLD" ) ) {
                            if ( f.isPlain() )
                                f = new Font( f.getFontName(), Font.BOLD, f.getSize() );
                            else if ( f.isItalic() )
                                f = new Font( f.getFontName(), Font.ITALIC | Font.BOLD, f.getSize() );
                            metrics = c.getFontMetrics( f );
                        }
                        else if ( newItem.format.equalsIgnoreCase( "/BOLD" ) ) {
                            if ( f.isItalic() )
                                f = new Font( f.getFontName(), Font.ITALIC, f.getSize() );
                            else
                                f = new Font( f.getFontName(), Font.PLAIN, f.getSize() );
                            metrics = c.getFontMetrics( f );
                        }
                        else if ( newItem.format.equalsIgnoreCase( "ITALIC" ) ) {
                            if ( f.isPlain() )
                                f = new Font( f.getFontName(), Font.ITALIC, f.getSize() );
                            else if ( f.isBold() )
                                f = new Font( f.getFontName(), Font.ITALIC | Font.BOLD, f.getSize() );
                            metrics = c.getFontMetrics( f );
                        }
                        else if ( newItem.format.equalsIgnoreCase( "/ITALIC" ) ) {
                            if ( f.isBold() )
                                f = new Font( f.getFontName(), Font.BOLD, f.getSize() );
                            else
                                f = new Font( f.getFontName(), Font.PLAIN, f.getSize() );
                            metrics = c.getFontMetrics( f );
                        }
                        else if ( newItem.format.equalsIgnoreCase( "FIXED" ) ) {
                            if ( f.isBold() && f.isItalic() )
                                f = new Font( "Monospaced", Font.ITALIC | Font.BOLD, f.getSize() );
                            else if ( f.isBold() )
                                f = new Font( "Monospaced", Font.BOLD, f.getSize() );
                            else if ( f.isItalic() )
                                f = new Font( "Monospaced", Font.ITALIC, f.getSize() );
                            else
                                f = new Font( "Monospaced", Font.PLAIN, f.getSize() );
                            metrics = c.getFontMetrics( f );
                        }
                        else if ( newItem.format.equalsIgnoreCase( "SANS" ) ) {
                            if ( f.isBold() && f.isItalic() )
                                f = new Font( "SansSerif", Font.ITALIC | Font.BOLD, f.getSize() );
                            else if ( f.isBold() )
                                f = new Font( "SansSerif", Font.BOLD, f.getSize() );
                            else if ( f.isItalic() )
                                f = new Font( "SansSerif", Font.ITALIC, f.getSize() );
                            else
                                f = new Font( "SansSerif", Font.PLAIN, f.getSize() );
                            metrics = c.getFontMetrics( f );
                        }
                        else if ( newItem.format.equalsIgnoreCase( "SERIF" ) ) {
                            if ( f.isBold() && f.isItalic() )
                                f = new Font( "Serif", Font.ITALIC | Font.BOLD, f.getSize() );
                            else if ( f.isBold() )
                                f = new Font( "Serif", Font.BOLD, f.getSize() );
                            else if ( f.isItalic() )
                                f = new Font( "Serif", Font.ITALIC, f.getSize() );
                            else
                                f = new Font( "Serif", Font.PLAIN, f.getSize() );
                            metrics = c.getFontMetrics( f );
                        }
                        else if ( newItem.format.substring( 0, 4 ).equalsIgnoreCase( "SIZE" ) ) {
                            //  Parse out the size value...
                            String sizeString = newItem.format.substring( newItem.format.indexOf( '=' ) + 1 ).trim();
                            int newSize = f.getSize();
                            if ( sizeString.contains( "." ) )
                                newSize = (int)( Double.parseDouble( sizeString ) * (double)newSize );
                            else
                                newSize = Integer.parseInt( sizeString );
                            if ( f.isBold() && f.isItalic() )
                                f = new Font( f.getFontName(), Font.ITALIC | Font.BOLD, newSize );
                            else if ( f.isBold() )
                                f = new Font( f.getFontName(), Font.BOLD, newSize );
                            else if ( f.isItalic() )
                                f = new Font( f.getFontName(), Font.ITALIC, newSize );
                            else
                                f = new Font( f.getFontName(), Font.PLAIN, newSize );
                            metrics = c.getFontMetrics( f );
                        }
                        else if ( newItem.format.substring( 0, 4 ).equalsIgnoreCase( "LINE" ) ) {
                            String sizeString = newItem.format.substring( newItem.format.indexOf( '=' ) + 1 ).trim();
                            if ( sizeString.contains( "." ) ) {
                                iLine = null;
                                dLine = Double.parseDouble( sizeString );
                            }
                            else {
                                dLine = null;
                                iLine = Integer.parseInt( sizeString );
                            }
                        }
                        else if ( newItem.format.substring( 0, 1 ).equalsIgnoreCase( "X" ) ) {
                            String sizeString = newItem.format.substring( newItem.format.indexOf( '=' ) + 1 ).trim();
                            int offSet = 0;
                            if ( sizeString.contains( "." ) )
                                offSet = (int)( Double.parseDouble( sizeString ) * (double)f.getSize() );
                            else
                                offSet = Integer.parseInt( sizeString );
                            x += offSet;
                        }
                        else if ( newItem.format.substring( 0, 1 ).equalsIgnoreCase( "Y" ) ) {
                            String sizeString = newItem.format.substring( newItem.format.indexOf( '=' ) + 1 ).trim();
                            int offSet = 0;
                            if ( sizeString.contains( "." ) )
                                offSet = (int)( Double.parseDouble( sizeString ) * (double)f.getSize() );
                            else
                                offSet = Integer.parseInt( sizeString );
                            if ( y == null )
                                y = metrics.getHeight();
                            y += offSet;
                        }
                        else if ( newItem.format.substring( 0, 2 ).equalsIgnoreCase( "BG" ) ) {
                            //  We need three numbers...
                            String colorString = newItem.format.substring( newItem.format.indexOf( '=' ) + 1 ).trim();
                            String str = colorString.substring( 0, colorString.indexOf( ',' ) ).trim();
                            float R;
                            if ( str.contains( "." ) )
                                R = Float.parseFloat( str );
                            else
                                R = (float)(Integer.parseInt( str ))/(float)256.0;
                            colorString = colorString.substring( colorString.indexOf( ',' ) + 1 ).trim();
                            str = colorString.substring( 0, colorString.indexOf( ',' ) ).trim();
                            float G;
                            if ( str.contains( "." ) )
                                G = Float.parseFloat( str );
                            else
                                G = (float)(Integer.parseInt( str ))/(float)256.0;
                            str = colorString.substring( colorString.indexOf( ',' ) + 1 ).trim();
                            float B;
                            if ( str.contains( "." ) )
                                B = Float.parseFloat( str );
                            else
                                B = (float)(Integer.parseInt( str ))/(float)256.0;
                            backgroundColor = new Color( R, G, B );
                        }
                        else if ( newItem.format.substring( 0, 2 ).equalsIgnoreCase( "FG" ) ) {
                            //  We need three numbers...
                            String colorString = newItem.format.substring( newItem.format.indexOf( '=' ) + 1 ).trim();
                            String str = colorString.substring( 0, colorString.indexOf( ',' ) ).trim();
                            float R;
                            if ( str.contains( "." ) )
                                R = Float.parseFloat( str );
                            else
                                R = (float)(Integer.parseInt( str ))/(float)256.0;
                            colorString = colorString.substring( colorString.indexOf( ',' ) + 1 ).trim();
                            str = colorString.substring( 0, colorString.indexOf( ',' ) ).trim();
                            float G;
                            if ( str.contains( "." ) )
                                G = Float.parseFloat( str );
                            else
                                G = (float)(Integer.parseInt( str ))/(float)256.0;
                            str = colorString.substring( colorString.indexOf( ',' ) + 1 ).trim();
                            float B;
                            if ( str.contains( "." ) )
                                B = Float.parseFloat( str );
                            else
                                B = (float)(Integer.parseInt( str ))/(float)256.0;
                            foregroundColor = new Color( R, G, B );
                        }
                        else if ( newItem.format.substring( 0, 4 ).equalsIgnoreCase( "LINK" ) )
                            link = newItem.format.substring( newItem.format.indexOf( '=' ) + 1 ).trim();
                        else if ( newItem.format.equalsIgnoreCase( "/LINK" ) )
                            link = null;
                    }
                }
                //  Next must be a newline.
                else {
                    TextItem newItem = new TextItem( tipText.substring( ptr, newline ) );
                    newItem.x = x;
                    int metricHeight = metrics.getHeight();
                    if ( dLine != null )
                        metricHeight = (int)( dLine * (double)metricHeight );
                    else if ( iLine != null )
                        metricHeight = iLine;
                    if ( y == null )
                        y = metricHeight;
                    newItem.y = y;
                    newItem.format = null;
                    _textItems.add( newItem );
                    if ( link != null ) {
                        LinkItem newLink = new LinkItem( link );
                        newLink.x = x;
                        newLink.y = y - metrics.getHeight();
                        newLink.y2 = y;
                        newLink.x2 = x + SwingUtilities.computeStringWidth( metrics, tipText.substring( ptr, newline ) );
                        _linkItems.add( newLink );
                    }
                    x += SwingUtilities.computeStringWidth( metrics, tipText.substring( ptr, newline ) );
                    maxWidth = ( maxWidth < x ) ? x : maxWidth;
                    ptr = newline + 1;
                    x = 3;
                    y += metricHeight;
                }
            }
            return new Dimension( maxWidth + 3, y + 4 );
        }
    }
    
    /*
     * Check a mouse click position against any links that we have.
     */
    void checkClick( int x, int y ) {
        if ( _linkItems != null ) {
            for ( Iterator<LinkItem> iter = _linkItems.descendingIterator(); iter.hasNext(); ) {
                LinkItem link = iter.next();
                System.out.println( link.x + "   " + link.y + "   " + link.x2 + "   " + link.y2 );
                if ( x > link.x && y > link.y && x < link.x2 && y < link.y2 ) {
                    if ( _dynamicLinkPath != null )
                        browseURL( _dynamicLinkPath.getText() + link.link );
                    else
                        browseURL( link.link );
                    break;
                }
            }
        }
    }

    /*
     * Try to open the given string in a browser.
     */
    public void browseURL( String url ) {
        java.awt.Desktop desktop = java.awt.Desktop.getDesktop();
        if ( !desktop.isSupported( java.awt.Desktop.Action.BROWSE ) ) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                "Unable to open \"" + url + "\" - browsing not supported??" );
            return;
        }
        try {
            desktop.browse( new java.net.URI( url ) );
        } catch ( Exception e ) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                "Unable to open \"" + url + "\" - " + e.getMessage() );
        }
    }
    
    /*
     * Set a text field that contains a "link path" which will be used to form all
     * links.  Links within a tooltip are specified by a text string.  When the user
     * clicks on a link, the text string is added to the content of this text field
     * to form the complete link (if this text field is null, the text string is used
     * alone).
     * 
     * What is the use of this?  It allows the UI to change the ultimate path of
     * links rather than explicitly setting it when UI components are initialized
     * (which is where tooltip definition is usually done).  I found this a useful
     * feature.
     */
    public void dynamicLinkPath( JTextComponent textField ) {
        _dynamicLinkPath = textField;
    }

    /*
     * This class contains a segment of text and/or a "format" instruction that
     * applies to it.  Format instructions include newline instructions, turning
     * on or off font types, changes in colors, or whatever.  They are implemented
     * FIRST, and the text is drawn afterward at the given x and y positions.
     */
    protected class TextItem {
        public TextItem( String newText ) {
            text = newText;
        }
        public String text;
        public String format;
        public int x;
        public int y;
    }
    
    protected class LinkItem {
        public LinkItem( String newText ) {
            link = newText;
        }
        public String link;
        public int x;
        public int y;
        public int x2;
        public int y2;
    }
    
    protected ArrayDeque<TextItem> _textItems;
    protected ArrayDeque<LinkItem> _linkItems;
    
    protected JTextComponent _dynamicLinkPath;
    
}

