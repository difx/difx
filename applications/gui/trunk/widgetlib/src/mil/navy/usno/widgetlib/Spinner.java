package mil.navy.usno.widgetlib;

/*
 * Little spinner thing.
 */


import javax.swing.JPanel;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.Dimension;
import java.awt.Color;

public class Spinner extends JPanel {

    public Spinner() {
        super();
        _value = 0;
        _this = this;
        _timeThread = new TimeThread();
        _timeThread.start();
        _colors = new int[12];
        for ( int i = 0; i < 12; ++i )
            _colors[i] = 255;
        _errorCondition = false;
    }

    @Override
    public void paintComponent( Graphics g ) {
        super.paintComponent( g );
        Graphics2D g2 = (Graphics2D)g;
        g2.setRenderingHint( RenderingHints.KEY_ANTIALIASING,
                     RenderingHints.VALUE_ANTIALIAS_ON );
        Dimension d = getSize();
        int startAngle = 359;
        int endAngle = 15;
        _colors[_value] = 0;
        for ( int i = 0; i < 12; ++i ) {
            if ( _errorCondition ) {
                g2.setColor( Color.RED );
            }
            else {
                g2.setColor( new Color( _colors[i], 255, _colors[i] ) );
            }
            g2.fillArc( 0, 0, d.width, d.height, startAngle, endAngle );
            startAngle -= 30;
            _colors[i] += 25;
            if ( _colors[i] > 255 )
                _colors[i] = 255;
        }
    }

    protected class TimeThread extends Thread {

        public TimeThread() {
            super();
            _keepGoing = true;
        }

        public void run() {
            _value = 10;
            _threadRunning = true;
            while ( _keepGoing ) {
                try { Thread.sleep( 166 );
                } catch ( Exception e ) {
                }
                _value += 1;
                if ( _value == 12 )
                    _value = 0;
                _this.updateUI();
            }
            _threadRunning = false;
        }
        protected boolean _keepGoing;
        public void keepGoing( boolean newVal ) { _keepGoing = newVal; }
        protected boolean _threadRunning;
        public boolean threadRunning() { return _threadRunning; }
    }

    public void stop() { 
        _timeThread.keepGoing( false );
        int counter = 20;
        while ( _timeThread.threadRunning() && counter != 0 ) {
            --counter;
            _timeThread.keepGoing( false );
            try { Thread.sleep( 250 ); } catch ( Exception e ) {}
        }
    }
    public void error() { _errorCondition = true; }
    public void ok() { _errorCondition = false; }

    protected int _value;
    protected TimeThread _timeThread;
    protected boolean _errorCondition;
    protected Spinner _this;
    protected int[] _colors;
}

