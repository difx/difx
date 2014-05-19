/*
 * This is the top-level of the DiFX GUI.  It launches a single window containing
 * multiple panels for DifX control.
 */
package edu.nrao.difx.difxview;

import mil.navy.usno.widgetlib.MessageDisplayPanel;
import mil.navy.usno.widgetlib.ZMenuItem;
import mil.navy.usno.widgetlib.NodeBrowserScrollPane;

import java.awt.event.WindowListener;
import java.awt.event.WindowEvent;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import edu.nrao.difx.difxcontroller.MulticastMonitor;
import edu.nrao.difx.difxcontroller.DiFXMessageProcessor;

import java.awt.event.ComponentEvent;

import javax.swing.JSplitPane;
import javax.swing.UIManager;
import javax.swing.JMenuBar;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JSeparator;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;

import java.awt.Point;

import javax.swing.plaf.FontUIResource;

import java.awt.Font;
import java.awt.Color;
import javax.swing.*;
import mil.navy.usno.widgetlib.ActivityMonitorLight;

/**
 *
 * @author  jspitzak (USNO)
 * @version 1.0
 */
public class DiFXUI extends JFrame implements WindowListener {

    /*
     * This is a static method for setting the default font for all components
     * that I swiped from the web.
     * 
     * setUIFont( new javax.swing.plaf.FontUIResource( "Serif", Font.ITALIC, 12 ) );
     */
    public static void setUIFont( FontUIResource f ) {
        java.util.Enumeration keys = UIManager.getDefaults().keys();
        while ( keys.hasMoreElements() ) {
            Object key = keys.nextElement();
            Object value = UIManager.get( key );
            if ( value instanceof javax.swing.plaf.FontUIResource )
            UIManager.put( key, f );
        }
    }

    public DiFXUI( String settingsFile ) {
        
        setUIFont( new javax.swing.plaf.FontUIResource( "Sans", Font.PLAIN, 12 ) );
        //  This goofiness makes the text on progress bars black.
        UIManager.put( "ProgressBar.selectionForeground", Color.DARK_GRAY );
        UIManager.put( "ProgressBar.selectionBackground", Color.DARK_GRAY );
        
        //  Produce system settings using the settings file that came from command
        //  line arguments (which might be null, indicating we should use default
        //  values).
        _systemSettings = new SystemSettings( this, settingsFile );
        _systemSettings.setLookAndFeel();
        
        //  This function builds all of the GUI components.
        initComponents();
        
        //  Set the message center to absorb "logging" messages.  The
        //  "global" string tells it to capture everything (you can specify more
        //  restrictive names if you are perverse enough to delve into the Java
        //  logging system).
        _messageCenter.captureLogging( "global" );
        _systemSettings.messageCenter( _messageCenter );
        
        //  Start the threads the read and process outside messages (either from the
        //  guiServer or from the multicast network).  The read thread just reads and
        //  queues stuff as fast as possible.  The process thread works its way through
        //  the queue dealing with messages in order.
        _multicastMonitor = new MulticastMonitor( _systemSettings );
        _difxMessageProcessor = new DiFXMessageProcessor( _systemSettings );
        _systemSettings.difxMessageProcessor( _difxMessageProcessor );
        _multicastMonitor.difxMessageProcessor( _difxMessageProcessor );
        _difxMessageProcessor.start();
        _multicastMonitor.start();

        _queueBrowser.difxMessageProcessor( _difxMessageProcessor );
        _hardwareMonitor.difxMessageProcessor( _difxMessageProcessor );

        this.setLocation( _systemSettings.windowConfiguration().mainX, _systemSettings.windowConfiguration().mainY );

        // Set the locations of dividers between panels.  For some reason these
        // settings seem to work better here.  The "resize weight" determines how
        // resize events are allotted to component panel sizes.
        _topSplitPane.setDividerLocation( _systemSettings.windowConfiguration().topDividerLocation );

        //  This is a bit messy - because the top pane has not been drawn yet, its
        //  size is basically 0 by 0.  We have to guess what size it will be to set
        //  the location of its divider as an absolute pixel value
        _mainSplitPane.setDividerLocation( _systemSettings.windowConfiguration().mainDividerLocation );
        _topSplitPane.setResizeWeight( 0.5 );

        // Set ourselves up to intercept window operations (close, iconize, etc).
        addWindowListener( this );
        
        // This stuff is used to trap resize events.
        _this = this;
		this.addComponentListener(new java.awt.event.ComponentAdapter() 
		{
			public void componentResized(ComponentEvent e)
			{
				_this.newSize();
			}
			public void componentMoved(ComponentEvent e)
			{
				_this.newSize();
			}
		});
        
        //  Implement the "tear off" state of panels.
        if ( _systemSettings.windowConfiguration().hardwareMonitorTearOff ) {
            _hardwareMonitor.tearOffState( _systemSettings.windowConfiguration().hardwareMonitorTearOff );
            _hardwareMonitor.setSize( _systemSettings.windowConfiguration().hardwareMonitorW,
                    _systemSettings.windowConfiguration().hardwareMonitorH );
            _hardwareMonitor.tearOffEvent();
        }
        if ( _systemSettings.windowConfiguration().queueBrowserTearOff ) {
            System.out.println( _systemSettings.windowConfiguration().queueBrowserX + "  " +
                    _systemSettings.windowConfiguration().queueBrowserY );
            _queueBrowser.tearOffState( _systemSettings.windowConfiguration().queueBrowserTearOff );
            _queueBrowser.setSize( _systemSettings.windowConfiguration().queueBrowserW,
                    _systemSettings.windowConfiguration().queueBrowserH );
            _queueBrowser.tearOffEvent();
        }
    }
    
    /*
     * Window event methods - we need each of these, even though we are only
     * interested in the "Closing" method.
     */
    @Override
    public void windowOpened(WindowEvent e) {
    }

    @Override
    public void windowClosed(WindowEvent e) {
    }

    @Override
    public void windowClosing(WindowEvent e) {
        exitOperation();
    }

    @Override
    public void windowActivated(WindowEvent e) {
    }

    @Override
    public void windowDeactivated(WindowEvent e) {
    }

    @Override
    public void windowIconified(WindowEvent e) {
    }

    @Override
    public void windowDeiconified(WindowEvent e) {
    }

    @Override
    public void setBounds( int x, int y, int w, int h ) {
        super.setBounds( x, y, w, h );
        newSize();
    }
    
    public void newSize() {
        int w = this.getWidth();
        int h = this.getHeight();
        if ( _mainSplitPane != null ) {
            _mainSplitPane.setBounds( 0, 25, w, h - 47 );
            //  Maintain the relative sizes of the subwindows
            if ( _topSplitPane != null && _messageCenter != null ) {
                double wFraction = (double)( _topSplitPane.getHeight() ) / 
                        ( (double)( _topSplitPane.getHeight() ) + (double)( _messageCenter.getHeight() ) );
                _mainSplitPane.setDividerLocation( wFraction );
            }
            if ( _queueBrowser != null && _hardwareMonitor != null ) {
                double wFraction = (double)( _queueBrowser.getWidth() ) /
                        ( (double)( _queueBrowser.getWidth() ) + (double)( _hardwareMonitor.getWidth() ) );
            }
            _mainSplitPane.updateUI();
        }
        if ( _menuBar != null )
            _menuBar.setBounds( 0, 0, w, 25 );
    }

    private void initComponents() {
        
        this.setLayout( null );
        
        //  These two widget types have static threads that run in them to do periodic
        //  updates and animations.  The reason for this is to allow each instance of
        //  the widgets to share a single thread, as there is a limit to threads and
        //  there can be hundreds of these guys.
        NodeBrowserScrollPane.initializeStatics();
        ActivityMonitorLight.initializeStatics();

        _mainSplitPane = new javax.swing.JSplitPane();
        _topSplitPane = new javax.swing.JSplitPane();
        _messageCenter = new mil.navy.usno.widgetlib.MessageDisplayPanel();
        _queueBrowser = new edu.nrao.difx.difxview.QueueBrowserPanel( _systemSettings );
        _queueBrowser.addTearOffListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                queueBrowserTearOffEvent();
            }
        } );

        setDefaultCloseOperation( javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE );
        setName( "DiFXUI" );
        setSize( new java.awt.Dimension( _systemSettings.windowConfiguration().mainW, 
                _systemSettings.windowConfiguration().mainH ) );
        
        _hardwareMonitor = new HardwareMonitorPanel( _systemSettings );
        _hardwareMonitor.addTearOffListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                hardwareMonitorTearOffEvent();
            }
        } );

        _mainSplitPane.setBorder( javax.swing.BorderFactory.createEmptyBorder( 1, 1, 1, 1 ) );
        _mainSplitPane.setDividerSize(3);
        _mainSplitPane.setOrientation( JSplitPane.VERTICAL_SPLIT );

        _topSplitPane.setDividerSize(3);
        _topSplitPane.setRightComponent( _hardwareMonitor );
        _topSplitPane.setLeftComponent( _queueBrowser );
        if ( _systemSettings.windowConfiguration().verticalPanels )
            _topSplitPane.setOrientation( JSplitPane.VERTICAL_SPLIT );

        _mainSplitPane.setLeftComponent( _topSplitPane );
        _mainSplitPane.setRightComponent( _messageCenter );

        //  Menu bar and components
        _menuBar = new JMenuBar();
        JMenu fileMenu = new JMenu( "File" );
        fileMenu.add( new JSeparator() );
        JMenuItem quitItem = new JMenuItem( "Quit" );
        quitItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                exitOperation();
            }
        } );
        fileMenu.add( quitItem );
        _menuBar.add( fileMenu );
        _settingsMenu = new JMenu( "Settings" );
        JMenuItem showSettingsItem = new JMenuItem( "Show Settings" );
        showSettingsItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                Point np = _settingsMenu.getLocationOnScreen();
                showSettingsAction( np.x, np.y );
            }
        } );
        _settingsMenu.add( showSettingsItem );
        _menuBar.add( _settingsMenu );
        JMenu windowMenu = new JMenu( "Window" );
        JMenu arrangeMenu = new JMenu( "Monitor Arrangement" );
        _horizontalItem = new JCheckBoxMenuItem( "Horizontal" );
        _horizontalItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                arrangeHorizontal();
            }
        } );
        if ( _systemSettings.windowConfiguration().verticalPanels )
            _horizontalItem.setSelected( false );
        else
            _horizontalItem.setSelected( true );
        arrangeMenu.add( _horizontalItem );
        _verticalItem = new JCheckBoxMenuItem( "Vertical" );
        _verticalItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                arrangeVertical();
            }
        } );
        if ( _systemSettings.windowConfiguration().verticalPanels )
            _verticalItem.setSelected( true );
        else
            _verticalItem.setSelected( false );
        arrangeMenu.add( _verticalItem );
        windowMenu.add( arrangeMenu );
        windowMenu.add( new JSeparator() );
        ZMenuItem forceHeaders = new ZMenuItem( "Display Hardware Headers" );
        forceHeaders.toolTip( "Force the display of header lines for both processors and\n"
                + "Mark V machines.  Normally these headers will not appear until\n"
                + "messages are received from <<italic>>mk5daemon<</italic>> instances.  If you\n"
                + "are not running <<italic>>mk5daemon<</italic>> anywhere, the header will let\n"
                + "you add machines by hand.", null );
        forceHeaders.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _hardwareMonitor.forceHeaders();
            }
        } );
        windowMenu.add( forceHeaders );
        _menuBar.add( windowMenu );
        JMenu helpMenu = new JMenu( "Help" );
        JMenuItem aboutItem = new JMenuItem( "Version, etc." );
        aboutItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                aboutAction( e );
            }
        } );
        helpMenu.add( aboutItem );
        helpMenu.add( new JSeparator() );
        JMenuItem helpIndexItem = new JMenuItem( "GUI Documentation" );
        helpIndexItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _systemSettings.launchGUIHelp( "intro.html" );
            }
        } );
        helpMenu.add( helpIndexItem );
        JMenuItem launchUsersGroupItem = new JMenuItem( "DiFX Users Group" );
        launchUsersGroupItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _systemSettings.launchDiFXUsersGroup();
            }
        } );
        helpMenu.add( launchUsersGroupItem );
        JMenuItem launchWikiItem = new JMenuItem( "DiFX Wiki" );
        launchWikiItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _systemSettings.launchDiFXWiki();
            }
        } );
        helpMenu.add( launchWikiItem );
        JMenuItem launchSVNItem = new JMenuItem( "DiFX svn" );
        launchSVNItem.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _systemSettings.launchDiFXSVN();
            }
        } );
        helpMenu.add( launchSVNItem );
        _menuBar.add( helpMenu );
        this.add( _menuBar );
        this.add( _mainSplitPane );
        this.newSize();
    }                  

    /*
     * On exit we over-write the "default" settings file.  This saves our current
     * configuration so it will appear the next time we run.  Before doing so we
     * need to make sure it is up to date on a bunch of settings.
     */
    private void exitOperation() {
        _multicastMonitor.shutDown();
        _difxMessageProcessor.shutDown();
        _systemSettings.windowConfiguration().mainX = this.getLocation().x;
        _systemSettings.windowConfiguration().mainY = this.getLocation().y;
        _systemSettings.windowConfiguration().mainW = this.getSize().width;
        _systemSettings.windowConfiguration().mainH = this.getSize().height;
        _systemSettings.windowConfiguration().hardwareMonitorTearOff = _hardwareMonitor.tearOffState();
        if ( _hardwareMonitorWindow != null ) {
            _systemSettings.windowConfiguration().hardwareMonitorX = _hardwareMonitorWindow.getLocation().x;
            _systemSettings.windowConfiguration().hardwareMonitorY = _hardwareMonitorWindow.getLocation().y;
        }
        _systemSettings.windowConfiguration().hardwareMonitorW = _hardwareMonitor.getSize().width;
        _systemSettings.windowConfiguration().hardwareMonitorH = _hardwareMonitor.getSize().height;
        _systemSettings.windowConfiguration().queueBrowserTearOff = _queueBrowser.tearOffState();
        if ( _queueBrowserWindow != null ) {
            _systemSettings.windowConfiguration().queueBrowserX = _queueBrowserWindow.getLocation().x;
            _systemSettings.windowConfiguration().queueBrowserY = _queueBrowserWindow.getLocation().y;
        }
        _systemSettings.windowConfiguration().queueBrowserW = _queueBrowser.getSize().width;
        _systemSettings.windowConfiguration().queueBrowserH = _queueBrowser.getSize().height;
        _systemSettings.windowConfiguration().mainDividerLocation = _mainSplitPane.getDividerLocation();
        _systemSettings.windowConfiguration().topDividerLocation = _topSplitPane.getDividerLocation();
        if ( _systemSettings.saveSettingsToFile( _systemSettings.defaultSettingsFile() ) )
            System.exit( 0 );
        else {
            Object[] options = { "Exit", "Cancel" };
            int ans = JOptionPane.showOptionDialog( this, 
                    "Unable to write default settings file \"" + _systemSettings.defaultSettingsFile() + "\"\nExit Anyway?",
                    "Settings File Write Error",
                    JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE, null, options, options[0] );
            if ( ans == 0 )
                System.exit( 0 );
        }
    }
    
    /*
     * Bring up a window containing settings for the GUI.
     */
    private void showSettingsAction( int x, int y ) {
        //  Put the settings window just below the menu button only if it hasn't
        //  be repositioned by the user.
        if ( _systemSettings.getX() == 0 )
            _systemSettings.setBounds( x, y + 25, _systemSettings.getWidth(), _systemSettings.getHeight() );
        _systemSettings.setVisible( true );
    }
    
    /*
     * This is called when the "Horizontal" arrangement button is pushed.
     */
    protected void arrangeHorizontal() {
        //  You can't "unselect" these buttons (they act like radio buttons).
        if ( !_horizontalItem.isSelected() ) {
            _horizontalItem.setSelected( true );
            return;
        }
        //  Turn off the other button.
        _verticalItem.setSelected( false );
        double wFraction = (double)( _queueBrowser.getWidth() ) /
                           ( (double)( _queueBrowser.getWidth() ) + (double)( _hardwareMonitor.getWidth() ) );
        _topSplitPane.setOrientation( JSplitPane.HORIZONTAL_SPLIT );
        _topSplitPane.setDividerLocation( wFraction );
        _systemSettings.windowConfiguration().verticalPanels = false;
    }
    
    /*
     * Similar function for the "Vertical" arrangement button.
     */
    protected void arrangeVertical() {
        //  You can't "unselect" these buttons (they act like radio buttons).
        if ( !_verticalItem.isSelected() ) {
            _verticalItem.setSelected( true );
            return;
        }
        //  Turn off the other button.
        _horizontalItem.setSelected( false );
        double wFraction = (double)( _queueBrowser.getWidth() ) /
                           ( (double)( _queueBrowser.getWidth() ) + (double)( _hardwareMonitor.getWidth() ) );
        _topSplitPane.setOrientation( JSplitPane.VERTICAL_SPLIT );
        _topSplitPane.setDividerLocation( wFraction );
        _systemSettings.windowConfiguration().verticalPanels = true;
    }
    
    /*
     * Called when the "tear off" button on the hardware monitor is pushed.  This
     * can happen when the button is pushed to "re-attach" as well.
     */
    protected void hardwareMonitorTearOffEvent() {
        if ( _hardwareMonitor.tearOffState() ) {
            if ( _hardwareMonitorWindow == null ) {
                _hardwareMonitorWindow = new JFrame();
                _hardwareMonitorWindow.setDefaultCloseOperation( javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE );
            }
            _hardwareMonitorWindow.setSize( _hardwareMonitor.getWidth(), _hardwareMonitor.getHeight() );
            _hardwareMonitorWindow.setLocation( _systemSettings.windowConfiguration().hardwareMonitorX,
                    _systemSettings.windowConfiguration().hardwareMonitorY );
            _hardwareMonitorWindow.add( _hardwareMonitor );
            _hardwareMonitorWindow.setVisible( true );
            if ( _queueBrowser.tearOffState() ) {
                _dividerLocation = (double)_mainSplitPane.getDividerLocation() / (double)this.getHeight();
                _mainSplitPane.remove( _topSplitPane );
            }
        }
        else {
            if ( _queueBrowser.tearOffState() ) {
                _mainSplitPane.setLeftComponent( _topSplitPane );
                _mainSplitPane.setDividerLocation( _dividerLocation );
            }
            _topSplitPane.setRightComponent( _hardwareMonitor );
            _hardwareMonitorWindow.setVisible( false );
        }
    }
    
    /*
     * Called when the "tear off" button on the queue browser is pushed.  This
     * can happen when the button is pushed to "re-attach" as well.
     */
    protected void queueBrowserTearOffEvent() {
        if ( _queueBrowser.tearOffState() ) {
            if ( _queueBrowserWindow == null ) {
                _queueBrowserWindow = new JFrame();
                _queueBrowserWindow.setDefaultCloseOperation( javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE );
            }
            _queueBrowserWindow.setSize( _queueBrowser.getWidth(), _queueBrowser.getHeight() );
            _queueBrowserWindow.setLocation( _systemSettings.windowConfiguration().queueBrowserX,
                    _systemSettings.windowConfiguration().queueBrowserY );
            _queueBrowserWindow.add( _queueBrowser );
            _queueBrowserWindow.setVisible( true );
            if ( _hardwareMonitor.tearOffState() ) {
                _dividerLocation = (double)_mainSplitPane.getDividerLocation() / (double)this.getHeight();
                _mainSplitPane.remove( _topSplitPane );
            }
        }
        else {
            if ( _hardwareMonitor.tearOffState() ) {
                _mainSplitPane.setLeftComponent( _topSplitPane );
                _mainSplitPane.setDividerLocation( _dividerLocation );
            }
            _topSplitPane.setLeftComponent( _queueBrowser );
            _queueBrowserWindow.setVisible( false );
        }
    }

    /*
     * Pop up a window containing information about this software.  The event is
     * passed so that the window pops up near the mouse.
     */
    protected void aboutAction( ActionEvent e ) {
        if ( _aboutWindow == null )
            _aboutWindow = new VersionWindow();
        _aboutWindow.setBounds( 150, 20, 300, 100 );
        _aboutWindow.setVisible( true );
    }
    
    /**
     * This is the main entry point of the GUI
     * 
     * @param args the command line arguments
     */
    public static void main( final String args[] ) {
        
        // Create manager UI using the first command line argument as a system settings
        // file.
        String settingsFile = null;
        if ( args.length > 0 )
            settingsFile = args[0];
        DiFXUI view = new DiFXUI( settingsFile );
        view.setVisible(true);

    }
                    
    protected HardwareMonitorPanel _hardwareMonitor;
    protected JSplitPane _mainSplitPane;
    protected MessageDisplayPanel _messageCenter;
    protected QueueBrowserPanel _queueBrowser;
    protected JSplitPane _topSplitPane;
    protected DiFXUI _this;
    protected JMenuBar _menuBar;
    protected JCheckBoxMenuItem _horizontalItem;
    protected JCheckBoxMenuItem _verticalItem;
    protected JFrame _hardwareMonitorWindow;
    protected JFrame _queueBrowserWindow;
    protected double _dividerLocation;
    protected VersionWindow _aboutWindow;
    protected JMenu _settingsMenu;
    
    protected SystemSettings _systemSettings;

    protected MulticastMonitor _multicastMonitor;
    protected DiFXMessageProcessor _difxMessageProcessor;
}