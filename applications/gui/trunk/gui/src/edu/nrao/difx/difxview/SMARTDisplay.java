/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxview;

import javax.swing.JFrame;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.table.DefaultTableModel;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JComponent;

import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.awt.Component;
import java.awt.Color;

import java.awt.event.ComponentEvent;

import java.util.Date;
import java.util.Iterator;

import java.text.SimpleDateFormat;

import edu.nrao.difx.difxutilities.SMARTMonitor;

public class SMARTDisplay extends JFrame {
    
    public SMARTDisplay( int x, int y, SMARTMonitor smartMonitor, SystemSettings settings, String host, String vsn ) {
        _settings = settings;
        _settings.setLookAndFeel();
        _smartMonitor = smartMonitor;
        this.setLayout( null );
        this.setBounds( x, y, _settings.windowConfiguration().smartDisplayW,
                _settings.windowConfiguration().smartDisplayH );
        this.getContentPane().setLayout( null );
        _this = this;
	this.addComponentListener( new java.awt.event.ComponentAdapter() {
            public void componentResized( ComponentEvent e ) {
                _settings.windowConfiguration().smartDisplayW = _this.getWidth();
                _settings.windowConfiguration().smartDisplayH = _this.getHeight();
                newSize();
            }
        });
        this.setTitle( "S.M.A.R.T. Display for " + vsn + " on " + host );
        _vsn = vsn;
        _host = host;
        _smartMonitor.addNewDataListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                locateData();
            }
        } );
        //  The "refresh" button is used to generate new data.  If the user doesn't hit
        //  this button whatever SMART data we have for this module/host combination is
        //  used.
        _refreshButton = new JButton( "Refresh" );
        _refreshButton.setToolTipText( "Generate new S.M.A.R.T. data for this host/VSN combination" );
        _refreshButton.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                _smartMonitor.generate( _host );
            }
        } );
        this.add( _refreshButton );
        _dateLabel = new JLabel( "no data" );
        _dateLabel.setBounds( 20, 20, 200, 25 );
        this.add( _dateLabel );
        _tableModel = new DefaultTableModel();
        _table = new JTable( _tableModel );
        _scrollPane = new JScrollPane( _table );
        this.add( _scrollPane );
        _tableModel.addColumn( "ID" );        
        _tableModel.addColumn( "Attribute Name" );        
        _tableModel.addColumn( "Better" );
        _tableModel.addColumn( "Slot 0" );
        _tableModel.addColumn( "Slot 1" );
        _tableModel.addColumn( "Slot 2" );
        _tableModel.addColumn( "Slot 3" );
        _tableModel.addColumn( "Slot 4" );
        _tableModel.addColumn( "Slot 5" );
        _tableModel.addColumn( "Slot 6" );
        _tableModel.addColumn( "Slot 7" );
        //  Build our own table cell renderer.  This allows us to change drawing aspects
        //  on each cell.  
        _renderer = new LocalTableCellRenderer( 50, 11 );
        _table.setDefaultRenderer( _table.getColumnClass( 3 ), _renderer );
        newSize();
        getData();
    }
    
    @Override
    public void setBounds( int x, int y, int w, int h ) {
        super.setBounds( x, y, w, h );
        newSize();
    }
    
    public void newSize() {
        if ( _scrollPane != null ) {
            int w = this.getContentPane().getSize().width;
            int h = this.getContentPane().getSize().height;
            _refreshButton.setBounds( w - 160, 20, 150, 25 );
            _scrollPane.setBounds( 0, 60, w, h - 60 );
            _dateLabel.setBounds( 20, 20, w - 180, 25 );
        }
    }

    public String host() { return _host; }
    public String vsn() { return _vsn; }
    
    /*
     * See if data exist for this VSN/host combination.  If there are none,
     * trigger the host to generate new data.
     */
    public void getData() {
        if ( _smartMonitor.data( _host, _vsn, 0 ) == null )
            _smartMonitor.generate( _host );
        else
            locateData();
    }
    
    /*
     * Poll the SMARTMonitor for data matching our current host and VSN for each slot
     * number we are interested in.
     */
    public void locateData() {
        for ( int slot = 0; slot < 8; ++slot ) {
            SMARTMonitor.SMARTData smartData = _smartMonitor.data( _host, _vsn, slot );
            if ( smartData != null && smartData.host.contentEquals( _host ) && smartData.vsn.contentEquals( _vsn ) ) {
                //  Change the date of the most recent update...if it is later than the one we have.
                if ( _dateLabelDate == null || _dateLabelDate.before( smartData.date ) ) {
                    _dateLabel.setText( "Last Update: " + new SimpleDateFormat( "MMMMM dd, yyyy hh:mm:ss z" ).format( smartData.date ) );
                    _dateLabelDate = smartData.date;
                }
                //  Loop through each item in these data.  Add a row to the table for the data ID if necessary.
                for ( Iterator<SMARTMonitor.SMARTdatum> iter = smartData.data.iterator(); iter.hasNext(); ) {
                    SMARTMonitor.SMARTdatum datum = iter.next();
                    //  Get attribute information from the system settings for this ID.  There may be
                    //  none...
                    SystemSettings.SMARTAttribute att = _settings.smartAttribute( datum.id );
                    String name = "";
                    String better = "";
                    boolean highIsGood = true;
                    int badValue = 0;
                    if ( att != null ) {
                        if ( att.name != null )
                            name = att.name;
                        if ( att.highIsGood != null ) {
                            if ( att.highIsGood ) {
                                highIsGood = true;
                                better = "\u25b4";
                            }
                            else {
                                highIsGood = false;
                                better = "\u25be";
                            }
                        }
                        if ( att.badValue != null )
                            badValue = att.badValue;
                    }
                    //  See if the row exists...we identify rows by ID number, which is in the 0th column
                    int row = -1;
                    Integer ID = (Integer)datum.id;
                    for ( int i = 0; i < _tableModel.getRowCount() && row == -1; ++i ) {
                        if ( ID.compareTo( (Integer)_tableModel.getValueAt( i, 0 ) ) == 0 )
                            row = i;
                        //  This is here in case our IDs come out of order...which they probably do not.
                        else if ( ID.compareTo( (Integer)_tableModel.getValueAt( i, 0 ) ) < 0 ) {
                            _tableModel.insertRow( i, new Object[]{ datum.id, name, better, "", "", "", "", "", "", "", "" } );
                            row = i;
                        }
                    }
                    //  If the row didn't exist, create a new one.
                    if ( row == -1 ) {
                        _tableModel.insertRow( _tableModel.getRowCount(), new Object[]{ datum.id, name, better, "", "", "", "", "", "", "", "" } );
                        row = _tableModel.getRowCount() - 1;
                    }
                    //  Change the data in the table for this column and row.
                    _tableModel.setValueAt( datum.value, row, slot + 3 );
                    //  Check the value against any limit.  How the limit is interpreted is based
                    //  on whether "high is good".
                    boolean badCell = false;
                    if ( att.highIsGood != null && att.badValue != null ) {
                        if ( ( att.highIsGood && datum.value < att.badValue ) ||
                             ( !att.highIsGood && datum.value > att.badValue ) ) {
                            badCell = true;
                        }
                    }
                    if ( badCell ) {
                        _renderer.background[row][slot + 3] = Color.RED;
                        _renderer.foreground[row][slot + 3] = Color.YELLOW;
                        _renderer.tooltip[row][slot + 3] = att.toolTip;
                    }
                    else {
                        _renderer.background[row][slot + 3] = Color.WHITE;
                        _renderer.foreground[row][slot + 3] = Color.BLACK;
                        _renderer.tooltip[row][slot + 3] = null;
                    }
                }
            }
        }
    }
    
    /*
     * This ridiculous thing allows us the change the attributes of different cells in
     * the table.  
     */
    public class LocalTableCellRenderer extends DefaultTableCellRenderer {
        
        public LocalTableCellRenderer( int rows, int columns ) {
            //  Set default colors
            background = new Color[rows][columns];
            foreground = new Color[rows][columns];
            tooltip = new String[rows][columns];
            for ( int i = 0; i < rows; ++i )
                for ( int j = 0; j < columns; ++j ) {
                    background[i][j] = Color.WHITE;
                    foreground[i][j] = Color.BLACK;
                    tooltip[i][j] = null;
                }
        }

        public Component getTableCellRendererComponent(JTable table,
                Object value,
                boolean isSelected,
                boolean hasFocus,
                int row,
                int column) {
            Component c = super.getTableCellRendererComponent(table, value,
                    isSelected, hasFocus,
                    row, column);

            if ( column == 2 )
                this.setHorizontalAlignment( DefaultTableCellRenderer.CENTER );
            else
                this.setHorizontalAlignment( DefaultTableCellRenderer.LEFT );
//            if ( row == 5 && column == 5 ) {
//                //c.setFont(/* special font*/);
//                // you may want to address isSelected here too
//                //c.setForeground(/*special foreground color*/);
//                c.setBackground( Color.BLUE );
//            }
//            else
//                c.setBackground( Color.WHITE );
            c.setBackground( background[row][column] );
            c.setForeground( foreground[row][column] );
            if ( tooltip[row][column] == null )
                ( (JComponent)c ).setToolTipText( "" );
            else
                ( (JComponent)c ).setToolTipText( tooltip[row][column] );
            return c;
        }
        
        public Color background[][];
        public Color foreground[][];
        public String tooltip[][];
        
    }

    protected SystemSettings _settings;
    protected SMARTMonitor _smartMonitor;
    protected SMARTDisplay _this;
    protected String _host;
    protected String _vsn;
    protected JButton _refreshButton;
    protected JScrollPane _scrollPane;
    protected JTable _table;
    protected JLabel _dateLabel;
    protected DefaultTableModel _tableModel;
    protected Date _dateLabelDate;
    protected LocalTableCellRenderer _renderer;
            
}
