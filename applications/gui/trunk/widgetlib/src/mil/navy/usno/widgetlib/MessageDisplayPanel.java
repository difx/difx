/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package mil.navy.usno.widgetlib;

import mil.navy.usno.widgetlib.MessageScrollPane;
import mil.navy.usno.widgetlib.MessageNode;
import java.awt.Color;
import java.util.Calendar;
import java.util.Iterator;
import java.util.ArrayDeque;

import javax.swing.JPanel;
import javax.swing.JButton;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenuBar;
import javax.swing.Box;
import javax.swing.JSeparator;
import javax.swing.JOptionPane;
import javax.swing.JCheckBox;
import javax.swing.JLabel;

/**
 *
 * @author jspitzak
 */
public class MessageDisplayPanel extends JPanel {
    
    public MessageDisplayPanel() {
        setLayout( null );
        _messageBrowser = new MessageScrollPane();
        this.add( _messageBrowser );
        _menuBar = new JMenuBar();
        this.add( _menuBar );
        _showMenu = new JMenu( "  Show  " );
        _menuBar.add( _showMenu );
        _includeMenu = new JMenu( "  Include  " );
        _menuBar.add( _includeMenu );
        _filterMenu = new JMenu( "  Filter  " );
        _menuBar.add( _filterMenu );
        _menuBar.add( Box.createHorizontalGlue() );
        _navigateMenu = new JMenu( "  Navigate  " );
        _menuBar.add( _navigateMenu );
        _clearMenu = new JMenu( "  Clear  " );
        _menuBar.add( _clearMenu );
        
        //  Show menu items
        _showErrors = new JCheckBoxMenuItem( "Errors", true );
        _showErrors.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                showErrorsAction( e );
            }
        });
        _showMenu.add( _showErrors );
        _showWarnings = new JCheckBoxMenuItem( "Warnings", true );
        _showWarnings.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                showWarningsAction( e );
            }
        });
        _showMenu.add( _showWarnings );
        _showMessages = new JCheckBoxMenuItem( "Messages", true );
        _showMessages.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                showMessagesAction( e );
            }
        });
        _showMenu.add( _showMessages );

        //  Include menu items
        _showDate = new JCheckBoxMenuItem( "Date", false );
        _showDate.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                showDateAction( e );
            }
        });
        _includeMenu.add( _showDate );
        _showTime = new JCheckBoxMenuItem( "Time", true );
        _showTime.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                showTimeAction( e );
            }
        });
        _includeMenu.add( _showTime );
        _showSource = new JCheckBoxMenuItem( "Source", true );
        _showSource.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                showSourceAction( e );
            }
        });
        _includeMenu.add( _showSource );

        //  Filter menu items
        _currentFilter = null;
        _filterSource = new JCheckBoxMenuItem( "Filter Source", false );
        _filterSource.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                runFilter();
            }
        });
        _filterMenu.add( _filterSource );
        _filterMessage = new JCheckBoxMenuItem( "Filter Message", true );
        _filterMessage.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                runFilter();
            }
        });
        _filterMenu.add( _filterMessage );
        _filterMenu.add( new JSeparator() );
        _containingMenu = new JMenu( "Search for String..." );
        _filterMenu.add( _containingMenu );
        _filters = new ArrayDeque<String>();
        _filters.add( "*" );
        buildFilterMenu();

        //  Navigate menu items
        _navigateTop = new JMenuItem( "Top" );
        _navigateTop.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                navigateTopAction( e );
            }
        });
        _navigateMenu.add( _navigateTop );
        _navigateBottom = new JMenuItem( "Bottom" );
        _navigateBottom.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                navigateBottomAction( e );
            }
        });
        _navigateMenu.add( _navigateBottom );
        _previousError = new JMenuItem( "Previous Error" );
        _previousError.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                previousErrorAction( e );
            }
        });
        _navigateMenu.add( _previousError );
        _nextError = new JMenuItem( "Next Error" );
        _nextError.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                nextErrorAction( e );
            }
        });
        _navigateMenu.add( _nextError );
        _previousWarning = new JMenuItem( "Previous Warning or Error" );
        _previousWarning.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                previousWarningAction( e );
            }
        });
        _navigateMenu.add( _previousWarning );
        _nextWarning = new JMenuItem( "Next Warning or Error" );
        _nextWarning.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                nextWarningAction( e );
            }
        });
        _navigateMenu.add( _nextWarning );

        //  Clear menu items
        _clearAll = new JMenuItem( "Errors, Warnings and Messages" );
        _clearAll.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                clearAllAction();
            }
        });
        _clearMenu.add( _clearAll );
        _clearWarningsAndMessages = new JMenuItem( "Warnings and Messages" );
        _clearWarningsAndMessages.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                clearWarningsAndMessagesAction( e );
           }
        });
        _clearMenu.add( _clearWarningsAndMessages );
        _clearMessages = new JMenuItem( "Messages" );
        _clearMessages.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                clearMessagesAction( e );
            }
        });
        _clearMenu.add( _clearMessages );
        _maxMessagesItem = new JPanel();
        _maxMessagesItem.setLayout( new java.awt.GridLayout() );
        _clearMenu.add( _maxMessagesItem );
        _maxMessagesCheck = new JCheckBox( "Keep Last " );
        _maxMessagesCheck.setToolTipText( "Keep only a set number of messages in the message buffer." );
        _maxMessagesItem.add( _maxMessagesCheck );
        _maxMessagesCheck.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                if ( _maxMessagesCheck.isSelected() )
                    _messageBrowser.maxMessages( _maxMessagesNum.intValue() );
                else
                    _messageBrowser.maxMessagesOff();
            }
        });
        _maxMessagesNum = new NumberBox();
        _maxMessagesNum.minimum( 0 );
        _maxMessagesNum.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                if ( _maxMessagesCheck.isSelected() )
                    _messageBrowser.maxMessages( _maxMessagesNum.intValue() );
            }
        });
        _maxMessagesItem.add( _maxMessagesNum );
        _maxMessagesCheck.setSelected( true );
        _maxMessagesNum.intValue( 1000 );
        _messageBrowser.maxMessages( 1000 );
        _oldMessagesItem = new JPanel();
        _oldMessagesItem.setLayout( new java.awt.GridLayout() );
        _clearMenu.add( _oldMessagesItem );
        _oldMessagesCheck = new JCheckBox( "Expire (min) " );
        _oldMessagesCheck.setToolTipText( "Throw away messages older than a set number of minutes." );
        _oldMessagesItem.add( _oldMessagesCheck );
        _oldMessagesCheck.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                if ( _oldMessagesCheck.isSelected() )
                    _messageBrowser.clearOlderThan( 0, 0, _oldMessagesMin.intValue(), 0 );
                else
                    _messageBrowser.clearOlderThanOff();
            }
        });
        _oldMessagesHour = new NumberBox();
        _oldMessagesHour.intValue( 0 );
        _oldMessagesHour.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                if ( _oldMessagesCheck.isSelected() )
                    _messageBrowser.clearOlderThan( 0, _oldMessagesHour.intValue(),_oldMessagesMin.intValue(), 0 );
            }
        });
        //_oldMessagesItem.add( _oldMessagesHour );
        JLabel hourLabel = new JLabel( "hrs" );
        hourLabel.setHorizontalAlignment( JLabel.LEFT );
        //_oldMessagesItem.add( hourLabel );
        _oldMessagesMin = new NumberBox();
        _oldMessagesMin.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                if ( _oldMessagesCheck.isSelected() )
                    _messageBrowser.clearOlderThan( 0, 0, _oldMessagesMin.intValue(), 0 );
            }
        });
        _oldMessagesItem.add( _oldMessagesMin );
        JLabel minLabel = new JLabel( "min" );
        minLabel.setHorizontalAlignment( JLabel.LEFT );
        //_oldMessagesItem.add( minLabel );
        _oldMessagesCheck.setSelected( false );
        _oldMessagesHour.minimum( 0 );
        _oldMessagesHour.intValue( 24 );
        _oldMessagesMin.intValue( 0 );
        _oldMessagesMin.minimum( 0 );
        _messageBrowser.maxMessages( 1000 );
    
        _messageBrowser.setBackground( Color.BLACK );

    }
    
    public void close() {
        _messageBrowser.close();
    }
    
    @Override
    public void setBounds( int x, int y, int w, int h ) {
        if ( _hideMenuBar ) {
            _messageBrowser.setBounds( 0, 0, w, h );
        }
        else {
            _messageBrowser.setBounds( 0, 20, w, h - 20 );
            _menuBar.setBounds( 0, 0, w, 20 );
        }
        super.setBounds( x, y, w, h );
    }
    
    protected void clearAllAction() {
        _messageBrowser.clear();
        this.updateUI();
    }
    
    /*
     * This can be used as soon as the display is formed to cause 
     */
    public void showSource( boolean newVal ) {
        _showSource.setSelected( newVal );
    }
    
    /*
     * Search through all messages and eliminate those that are warnings or
     * informational.
     */
    protected void clearWarningsAndMessagesAction( java.awt.event.ActionEvent e ) {
        for ( Iterator<MessageNode> iter = _messageBrowser.messageList().iterator(); iter.hasNext(); ) {
            MessageNode thisMessage = (MessageNode)( iter.next() );
            if ( thisMessage.severity() != MessageNode.ERROR )
                _messageBrowser.removeMessage( thisMessage );
        }
        _messageBrowser.listChange();
        this.updateUI();
    }
    
    protected void clearMessagesAction( java.awt.event.ActionEvent e ) {
        for ( Iterator<MessageNode> iter = _messageBrowser.messageList().iterator(); iter.hasNext(); ) {
            MessageNode thisMessage = (MessageNode)( iter.next() );
            if ( thisMessage.severity() == MessageNode.INFO )
                _messageBrowser.removeMessage( thisMessage );
        }
        _messageBrowser.listChange();
        this.updateUI();
    }
    
    protected void showDateAction( java.awt.event.ActionEvent e ) {
        for ( Iterator<MessageNode> iter = _messageBrowser.messageList().iterator(); iter.hasNext(); ) {
            MessageNode thisMessage = (MessageNode)( iter.next() );
            thisMessage.showDate( _showDate.isSelected() );
        }
        this.updateUI();
    }
    
    protected void showTimeAction( java.awt.event.ActionEvent e ) {
        for ( Iterator<MessageNode> iter = _messageBrowser.messageList().iterator(); iter.hasNext(); ) {
            MessageNode thisMessage = (MessageNode)( iter.next() );
            thisMessage.showTime( _showTime.isSelected() );
        }
        this.updateUI();
    }
    
    protected void showSourceAction( java.awt.event.ActionEvent e ) {
        for ( Iterator<MessageNode> iter = _messageBrowser.messageList().iterator(); iter.hasNext(); ) {
            MessageNode thisMessage = (MessageNode)( iter.next() );
            thisMessage.showSource( _showSource.isSelected() );
        }
        this.updateUI();
    }
    
    protected void showErrorsAction( java.awt.event.ActionEvent e ) {
        for ( Iterator<MessageNode> iter = _messageBrowser.messageList().iterator(); iter.hasNext(); ) {
            MessageNode thisMessage = (MessageNode)( iter.next() );
            thisMessage.showErrors( _showErrors.isSelected() );
        }
        _messageBrowser.listChange();
    }
    
    protected void showWarningsAction( java.awt.event.ActionEvent e ) {
        for ( Iterator<MessageNode> iter = _messageBrowser.messageList().iterator(); iter.hasNext(); ) {
            MessageNode thisMessage = (MessageNode)( iter.next() );
            thisMessage.showWarnings( _showWarnings.isSelected() );
        }
        _messageBrowser.listChange();
    }
    
    protected void showMessagesAction( java.awt.event.ActionEvent e ) {
        for ( Iterator<MessageNode> iter = _messageBrowser.messageList().iterator(); iter.hasNext(); ) {
            MessageNode thisMessage = (MessageNode)( iter.next() );
            thisMessage.showMessages( _showMessages.isSelected() );
        }
        _messageBrowser.listChange();
    }
    
    protected void buildFilterMenu() {
        _containingMenu.removeAll();
        for ( Iterator<String> iter = _filters.iterator(); iter.hasNext(); ) {
            final String thisFilter = new String( iter.next() );
            JCheckBoxMenuItem newFilter = new JCheckBoxMenuItem( thisFilter );
            newFilter.addActionListener( new java.awt.event.ActionListener() {
                public void actionPerformed( java.awt.event.ActionEvent e ) {
                    setFilterAction( e, thisFilter );
                }
            });
            if ( thisFilter.equals( _currentFilter ) ) {
                newFilter.setSelected( true );
            }
            else {
                newFilter.setSelected( false );
            }
            _containingMenu.add( newFilter );
        }
        _containingMenu.add( new JSeparator() );
        _newFilter = new JMenuItem( "Add New Search String" );
        _newFilter.addActionListener( new java.awt.event.ActionListener() {
            public void actionPerformed( java.awt.event.ActionEvent e ) {
                addNewFilterAction( e );
            }
        });
        _containingMenu.add( _newFilter );
    }
    
    protected void addNewFilterAction( java.awt.event.ActionEvent e ) {
        String newFilter = JOptionPane.showInputDialog( this, "New Filter:" );
        if ( newFilter != null ) {
            _filters.add( newFilter );
            _currentFilter = newFilter;
            //  The menu of filter options needs to be rewitten to include this
            //  item.  We do this (instead of just adding items to the end) so we
            //  can have "New Filter" at the end, where it belongs.
            this.buildFilterMenu();
            this.runFilter();
        }
    }
    
    protected void setFilterAction( java.awt.event.ActionEvent e, String newFilter ) {
        _currentFilter = newFilter;
        this.changeFilterMenu();
        this.runFilter();
    }
    
    protected void changeFilterMenu() {
        for ( int i = 0; i < _containingMenu.getItemCount() - 2; ++i ) {
            if ( _containingMenu.getItem( i ).getText().equals( _currentFilter ) )
                ((JCheckBoxMenuItem)(_containingMenu.getItem( i ))).setSelected( true );
            else
                ((JCheckBoxMenuItem)(_containingMenu.getItem( i ))).setSelected( false );
        }
    }
    
    /*
     * Run the current filter instructions on the existing browser items and
     * display the results.
     */
    protected void runFilter() {
        for ( Iterator<MessageNode> iter = _messageBrowser.messageList().iterator(); iter.hasNext(); ) {
            MessageNode thisMessage = (MessageNode)( iter.next() );
            thisMessage.applyFilter( _filterSource.isSelected(), _filterMessage.isSelected(),
                    _currentFilter );
        }
        _messageBrowser.listChange();
    }
    
        public void navigateTopAction( java.awt.event.ActionEvent e ) {
            _messageBrowser.scrollToTop();
        }
        
        public void navigateBottomAction( java.awt.event.ActionEvent e ) {
            _messageBrowser.scrollToEnd();
        }
        
        /*
         * Locate the first error that is above the current location, and move
         * the browser to it.
         */
        public void previousErrorAction( java.awt.event.ActionEvent e ) {
            int yOffset = _messageBrowser.getYOffset();
            MessageNode goToNode = null;
            int newOffset = 0;
            int nodeOffset = 0;
            for ( Iterator<MessageNode> iter = _messageBrowser.messageList().iterator(); iter.hasNext(); ) {
                MessageNode thisMessage = (MessageNode)( iter.next() );
                //  Measure the vertical offset of each message.  Make sure each
                //  is visible before we consider it.
                if ( thisMessage.showThis() ) {
                    //  Now see if the message is of the correct type - error in
                    //  this case.
                    if ( thisMessage.severity() == MessageNode.ERROR ) {
                        if ( -nodeOffset > yOffset ) {
                            goToNode = thisMessage;
                            newOffset = -nodeOffset;
                        }
                    }
                    nodeOffset += _messageBrowser.messageHeight();
                }
            }
            if ( goToNode != null )
                _messageBrowser.setYOffset( newOffset );
        }
        
        /*
         * Locate the first error below the current location and move the browser
         * to it.
         */
        public void nextErrorAction( java.awt.event.ActionEvent e ) {
            int yOffset = _messageBrowser.getYOffset();
            MessageNode goToNode = null;
            int newOffset = 0;
            int nodeOffset = 0;
            for ( Iterator<MessageNode> iter = _messageBrowser.messageList().iterator(); iter.hasNext(); ) {
                MessageNode thisMessage = (MessageNode)( iter.next() );
                //  Measure the vertical offset of each message.  Make sure each
                //  is visible before we consider it.
                if ( thisMessage._showThis ) {
                    //  Now see if the message is of the correct type - error in
                    //  this case.
                    if ( thisMessage.severity() == MessageNode.ERROR ) {
                        if ( goToNode == null && -nodeOffset < yOffset ) {
                            goToNode = thisMessage;
                            newOffset = -nodeOffset;
                        }
                    }
                    nodeOffset += _messageBrowser.messageHeight();
                }
            }
            if ( goToNode != null )
                _messageBrowser.setYOffset( newOffset );
        }
        
        public void previousWarningAction( java.awt.event.ActionEvent e ) {
            int yOffset = _messageBrowser.getYOffset();
            MessageNode goToNode = null;
            int newOffset = 0;
            int nodeOffset = 0;
            for ( Iterator<MessageNode> iter = _messageBrowser.messageList().iterator(); iter.hasNext(); ) {
                MessageNode thisMessage = (MessageNode)( iter.next() );
                //  Measure the vertical offset of each message.  Make sure each
                //  is visible before we consider it.
                if ( thisMessage._showThis ) {
                    //  Now see if the message is of the correct type - error in
                    //  this case.
                    if ( thisMessage.severity() == MessageNode.ERROR ||
                         thisMessage.severity() == MessageNode.WARNING ) {
                        if ( -nodeOffset > yOffset ) {
                            goToNode = thisMessage;
                            newOffset = -nodeOffset;
                        }
                    }
                    nodeOffset += _messageBrowser.messageHeight();
                }
            }
            if ( goToNode != null )
                _messageBrowser.setYOffset( newOffset );
        }
        
        public void nextWarningAction( java.awt.event.ActionEvent e ) {
            int yOffset = _messageBrowser.getYOffset();
            MessageNode goToNode = null;
            int newOffset = 0;
            int nodeOffset = 0;
            for ( Iterator<MessageNode> iter = _messageBrowser.messageList().iterator(); iter.hasNext(); ) {
                MessageNode thisMessage = (MessageNode)( iter.next() );
                //  Measure the vertical offset of each message.  Make sure each
                //  is visible before we consider it.
                if ( thisMessage._showThis ) {
                    //  Now see if the message is of the correct type - error in
                    //  this case.
                    if ( thisMessage.severity() == MessageNode.ERROR ||
                         thisMessage.severity() == MessageNode.WARNING ) {
                        if ( goToNode == null && -nodeOffset < yOffset ) {
                            goToNode = thisMessage;
                            newOffset = -nodeOffset;
                        }
                    }
                    nodeOffset += _messageBrowser.messageHeight();
                }
            }
            if ( goToNode != null )
                _messageBrowser.setYOffset( newOffset );
        }
                
    /*
     * Set a "handler" to sponge up log messages and display them on this
     * message panel.  Logging can be broken into individual logs, which are
     * named by the given string.  You can use "global" as the string to grab
     * everything (which is generally what I do).
     */
    public void captureLogging( String logName ) {
        internalLoggingHandler = new InternalLoggingHandler( this );
        java.util.logging.Logger.getLogger( logName ).addHandler( internalLoggingHandler );
    }
    
    public void message( long time, String source, String newText ) {
        if ( time == 0 ) {
            Calendar cal = Calendar.getInstance();
            time = cal.getTimeInMillis();
        }
        if ( source == null ) {
            source = "GUI Internal";
        }
        MessageNode node = new MessageNode( time, MessageNode.INFO, source, newText );
        node.showDate( _showDate.isSelected() );
        node.showTime( _showTime.isSelected() );
        node.showSource( _showSource.isSelected() );
        node.showErrors( _showErrors.isSelected() );
        node.showWarnings( _showWarnings.isSelected() );
        node.showMessages( _showMessages.isSelected() );
        node.applyFilter( _filterSource.isSelected(), _filterMessage.isSelected(), _currentFilter );
        _messageBrowser.addMessage( node );
    }

    public void warning( long time, String source, String newText ) {
        if ( time == 0 ) {
            Calendar cal = Calendar.getInstance();
            time = cal.getTimeInMillis();
        }
        if ( source == null ) {
            source = "GUI Internal";
        }
        MessageNode node = new MessageNode( time, MessageNode.WARNING, source, newText );
        node.showDate( _showDate.isSelected() );
        node.showTime( _showTime.isSelected() );
        node.showSource( _showSource.isSelected() );
        node.showErrors( _showErrors.isSelected() );
        node.showWarnings( _showWarnings.isSelected() );
        node.showMessages( _showMessages.isSelected() );
        node.applyFilter( _filterSource.isSelected(), _filterMessage.isSelected(), _currentFilter );
        _messageBrowser.addMessage( node );
    }

    public void error( long time, String source, String newText ) {
        if ( time == 0 ) {
            Calendar cal = Calendar.getInstance();
            time = cal.getTimeInMillis();
        }
        if ( source == null ) {
            source = "GUI Internal";
        }
        MessageNode node = new MessageNode( time, MessageNode.ERROR, source, newText );
        node.showDate( _showDate.isSelected() );
        node.showTime( _showTime.isSelected() );
        node.showSource( _showSource.isSelected() );
        node.showErrors( _showErrors.isSelected() );
        node.showWarnings( _showWarnings.isSelected() );
        node.showMessages( _showMessages.isSelected() );
        node.applyFilter( _filterSource.isSelected(), _filterMessage.isSelected(), _currentFilter );
        _messageBrowser.addMessage( node );
    }
    
    private InternalLoggingHandler internalLoggingHandler;
    /*
     * This class allows the message panel to sponge up logging messages.  Logging
     * is a bit complex and messy in Java, but for our purposes, you can dump
     * something like the following any place in the code:
     * 
     *      java.util.logging.Logger.getLogger("global").log(java.util.logging.Level.SEVERE, "this is a message" );
     * 
     * This will cause the message to appear in the output console as well as to
     * be captured by this logging handler (and from there printed to the panel).
     * 
     * Logging messages are given a "level", which can be SEVERE, WARNING, INFO,
     * CONFIG, FINE, FINER, or FINEST.  The logging system can be adjusted to
     * ignore levels.  We don't use this system to its full extent - SEVERE log
     * messages are treated as "errors", WARNINGS are treated as "warnings" and
     * everything else is treated as "messages".
     * 
     * You can add a "Throwable" as a third parameter to a "log()" call if you
     * wish.  The "publish()" method below knows what to do with those.  You can
     * also add Objects, but publish() ignores those.
     */
    private class InternalLoggingHandler extends java.util.logging.Handler {
        public InternalLoggingHandler( MessageDisplayPanel newOutput ) {
            output = newOutput;
            //includeTime = true;
            //includeSource = true;
            //includeDate = false;
        }
        //public void includeDate( boolean newVal ) {
        //    includeDate = newVal;
        //}
        //public void includeTime( boolean newVal ) {
        //    includeTime = newVal;
        //}
        //public void includeSource( boolean newVal ) {
        //    includeSource = newVal;
        //}
        @Override
        public void flush() {}        
        @Override
        public void close() {}        
        @Override
        public void publish( java.util.logging.LogRecord newRecord ) {
            //  Build a new message string including components as specified.
            String outText = new String( "" );
            //  Print out the string version of a Throwable, if one is included.
            //  These are sometimes big, but useful to see.
            if ( newRecord.getThrown() != null ) {
                outText += newRecord.getThrown().toString() + " ";
            }
            if ( newRecord.getMessage() != null ) {
                outText += newRecord.getMessage();
            }
            long time = newRecord.getMillis();
            String source = "GUI:" + newRecord.getSourceClassName().substring( newRecord.getSourceClassName().lastIndexOf(".") + 1 ) + 
                        "." + newRecord.getSourceMethodName() + "(): ";
            if ( newRecord.getLevel() == java.util.logging.Level.SEVERE )
                output.error( time, source, outText + "\n" );
            else if ( newRecord.getLevel() == java.util.logging.Level.WARNING )
                output.warning( time, source, outText + "\n" );
            else
                output.message( time, source, outText + "\n" );
        }       
        private MessageDisplayPanel output;
    }
    
    public void hideMenuBar( boolean newVal ) { 
        _hideMenuBar = newVal;
        _menuBar.setVisible( !_hideMenuBar );
    }
    
    protected MessageScrollPane _messageBrowser;
    protected JButton _clearButton;
    protected JMenuBar _menuBar;
    protected JMenu _clearMenu;
    protected JMenuItem _clearAll;
    protected JMenuItem _clearWarningsAndMessages;
    protected JMenuItem _clearMessages;
    protected JMenuItem _clearChecked;
    protected JMenuItem _clearUnchecked;
    protected JCheckBoxMenuItem _showDate;
    protected JCheckBoxMenuItem _showTime;
    protected JCheckBoxMenuItem _showSource;
    protected JCheckBoxMenuItem _showErrors;
    protected JCheckBoxMenuItem _showWarnings;
    protected JCheckBoxMenuItem _showMessages;
    protected JCheckBoxMenuItem _showChecked;
    protected JCheckBoxMenuItem _showUnchecked;
    protected JMenuItem _newFilter;
    protected JCheckBoxMenuItem _filterSource;
    protected JCheckBoxMenuItem _filterMessage;
    protected JMenu _containingMenu;
    protected JMenu _showMenu;
    protected JMenu _includeMenu;
    protected JMenu _filterMenu;
    protected JMenu _navigateMenu;
    protected JMenuItem _navigateTop;
    protected JMenuItem _navigateBottom;
    protected JMenuItem _previousError;
    protected JMenuItem _nextError;
    protected JMenuItem _previousWarning;
    protected JMenuItem _nextWarning;
    protected JMenuItem _previousChecked;
    protected JMenuItem _nextChecked;
    protected JPanel _maxMessagesItem;
    protected JCheckBox _maxMessagesCheck;
    protected NumberBox _maxMessagesNum;
    protected JPanel _oldMessagesItem;
    protected JCheckBox _oldMessagesCheck;
    protected NumberBox _oldMessagesHour;
    protected NumberBox _oldMessagesMin;
    protected ArrayDeque<String> _filters;
    protected String _currentFilter;
    protected boolean _hideMenuBar;

}
