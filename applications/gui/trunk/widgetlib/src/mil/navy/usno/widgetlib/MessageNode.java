/*
 * A message node contains the data for a message, which may have different
 * levels of severity.  It uses the "draw()" function to display itself.
 */
package mil.navy.usno.widgetlib;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Container;

import java.text.SimpleDateFormat;

/**
 *
 * @author jspitzak
 */
public class MessageNode {
    
    public static int ERROR = 0;
    public static int WARNING = 1;
    public static int INFO = 2;
    
    public MessageNode( long time, int severity, String source, String message ) {
        _source = source;
        _message = message;
        _time = time;
        _severity = severity;
        _showTime = true;
        _showDate = false;
        _showSource = true;
        _showErrors = true;
        _showWarnings = true;
        _showMessages = true;
        _showChecked = true;
        _showUnchecked = true;
        SimpleDateFormat sdf = new SimpleDateFormat( "yyyy-MM-dd" );
        _dateString = sdf.format( time );
        sdf = new SimpleDateFormat( "HH:mm:ss.SSS" );
        _timeString = sdf.format( time );
    }
    
    /*
     * Draw this message at the given x and y location.  Highlight any portion
     * of it that is specified.
     */
    public void draw( Graphics g, int x, int y ) {
        _fontMetrics = g.getFontMetrics();
        if ( _highlight ) {
            g.setColor( _highlightColor );
            g.fillRect( _highlightStart, y - _highlightHeight, _highlightEnd - _highlightStart, _highlightHeight );
        }
        if ( _severity < 1 )
            g.setColor( Color.RED );
        else if ( _severity < 2 )
            g.setColor( Color.YELLOW );
        else
            g.setColor( Color.WHITE );
        String text = "";
        if ( _showDate ) {
            text += _dateString + " ";
        }
        if ( _showTime ) {
            text += _timeString + " ";
        }
        if ( _showSource )
            text += "(" + _source + ") ";
        g.drawString( text + _message , x, y );
        _text = ( text + _message ).toCharArray();
    }
    
    /*
     * Return whether this message is older than the given reference time by an
     * interval measured in days, hours, minutes, and seconds, all integers).
     * The time associated with each message is in milliseconds.  The time used
     * for "now" is passed as an argument (so it can be anything you want).
     */
    public boolean isOlderThan( long nowTime, int days, int hours, int minutes, int seconds ) {
        long diffTime = 1000 * ( seconds + 60 * ( minutes + 60 * ( hours + 24 * days ) ) );
        if ( _time + diffTime < nowTime )
            return true;
        else
            return false;
    }
    
    /*
     * Return whether this message is older than a specific time (basically the above
     * function but with all of the math done elsewhere).
     */
    public boolean isOlderThan( long refTime ) {
        if ( _time < refTime ) 
            return true;
        else
            return false;
    }
    
    public int severity() {
        return _severity;
    }
    public void severity( int newVal ) {
        _severity = newVal;
    }
    
    public boolean showDate() {
        return _showDate;
    }
    public void showDate( boolean newVal ) {
        _showDate = newVal;
    }
    
    public boolean showTime() {
        return _showTime;
    }
    public void showTime( boolean newVal ) {
        _showTime = newVal;
    }
    
    public boolean showSource() {
        return _showSource;
    }
    public void showSource( boolean newVal ) {
        _showSource = newVal;
    }
    
    public boolean showErrors() {
        return _showErrors;
    }
    public void showErrors( boolean newVal ) {
        _showErrors = newVal;
        showIt();
    }
    
    public boolean showWarnings() {
        return _showWarnings;
    }
    public void showWarnings( boolean newVal ) {
        _showWarnings = newVal;
        showIt();
    }
    
    public boolean showMessages() {
        return _showMessages;
    }
    public void showMessages( boolean newVal ) {
        _showMessages = newVal;
        showIt();
    }
    
    public void applyFilter( boolean filterSource, boolean filterMessage, String filter ) {
        _filterSource = filterSource;
        _filterMessage = filterMessage;
        //  Special case - if the filter is "*", we save null as an indication
        //  that there really is no filter.
        if ( filter == null )
            _filter = null;
        else if ( filter.equals( "*" ) )
            _filter = null;
        else
            _filter = filter;
        showIt();
    }
    
    /*
     * Figure out whether we want to show this item based on the above settings.
     */
    public void showIt() {
        if ( _severity == ERROR ) {
            _showThis = _showErrors;            
        }
        else if ( _severity == WARNING ) {
            _showThis = _showWarnings;
        }
        else if ( _severity == INFO ) {
            _showThis = _showMessages;
        }
        if ( _filter != null ) {
            if ( _filterSource ) {
                if ( _source.indexOf( _filter ) > -1 )
                    _showThis &= true;
                else
                    _showThis = false;
            }
            if ( _filterMessage ) {
                if ( _message.indexOf( _filter ) > -1 )
                    _showThis &= true;
                else
                    _showThis = false;
            }
        }
    }
    
    public boolean showThis() { return _showThis; }
    
    /*
     * Highlight text on this line between the start and end x positions.  The
     * positions we are given are mouse positions - internally we convert them
     * to character positions by measuring actual character widths.  In this
     * function we are trying to determine two things - the characters from this
     * line that are highlighted (returned as a string) and the region that should
     * be colored with the highlight color when this text is drawn (in draw()).
     */
    public String highlight( boolean on, boolean fromStart, boolean toEnd, int start, int end, int h, Color c ) {
        _highlight = on;
        //  If we aren't highlighting at all, we could care less about the rest
        //  of this stuff.
        if ( !_highlight )
            return null;
        //  Occasionally the text can be empty - presumably when a message is being
        //  created.
        if ( _text == null )
            return null;
        _highlightHeight = h;
        _highlightColor = c;
        //  Find inter-character positions that correspond (as close as possible) to
        //  the start and end.  Starting character first.  It may be the beginning of the line.
        _highlightStart = 0;
        int startChar = 0;
        if ( fromStart )
            _highlightStart = 0;
        else {
            for ( int i = 0; i < _text.length; ++i ) {
                int len = _fontMetrics.charWidth( _text[i] );
                if ( _highlightStart < start ) {
                    _highlightStart += len;
                    startChar += 1;
                }
            }
        }
        //  Find the end character.  It might be that the highlighting should go to the
        //  end of the line (and include all characters).
        _highlightEnd = 0;
        int endChar = startChar;
        if ( toEnd ) {
            _highlightEnd = end;
            endChar = _text.length;
        }
        else {
            _highlightEnd = _highlightStart;
            for ( int i = startChar; i < _text.length; ++i ) {
                int len = _fontMetrics.charWidth( _text[i] );
                if ( _highlightEnd < end ) {
                    _highlightEnd += len;
                    endChar += 1;
                }
            }
        }
        //  Now produce a substring of the text between the start and end characters.
        String ret = "";
        for ( int i = startChar; i < endChar; ++i )
            if ( _text[i] != '\n' )
                ret += _text[i];
        ret += '\n';
        return ret;
    }
    
    protected String _source;
    protected String _message;
    protected long _time;
    protected int _severity;
    protected boolean _showTime;
    protected boolean _showDate;
    protected boolean _showSource;
    protected boolean _showErrors;
    protected boolean _showWarnings;
    protected boolean _showMessages;
    protected boolean _showChecked;
    protected boolean _showUnchecked;
    protected String _dateString;
    protected String _timeString;
    protected boolean _filterSource;
    protected boolean _filterMessage;
    protected String _filter;
    protected boolean _showThis;
    protected boolean _highlight;
    protected int _highlightStart;
    protected int _highlightEnd;
    protected int _highlightHeight;
    protected Color _highlightColor;
    protected FontMetrics _fontMetrics;
    protected char[] _text;
    
}
