/***************************************************************************
 *   Copyright (C) 2016 by John Spitzak                                    *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
/*
 * PlotterUI.java
 * 
 * This class provides a simple user interface for testing the PlotWindow class.
 * It allows the user to draw a simple plot and add stuff to it.
 *
 * Created on Jun 13, 2011, 5:08:45 PM
 */
package mil.navy.usno.plotlib;

import javax.swing.JColorChooser;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JSeparator;
import java.awt.Color;
import java.awt.event.WindowListener;
import java.awt.event.WindowEvent;
import javax.imageio.ImageIO;
import javax.swing.UIManager;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.Random;

import java.util.ArrayDeque;

/**
 *
 * @author jspitzak
 */
public class PlotterUI extends javax.swing.JFrame implements WindowListener {

    /** Creates new form PlotterUI */
    public PlotterUI() {
        //  This puts the menu bar on the top of the screen - apple-ish.  Doesn't work.
        //System.setProperty("apple.laf.useScreenMenuBar", "true");
        //  This is supposed to change the application name on the top of the screen.
        //  Doesn't work as far as I can tell.
        //System.setProperty("com.apple.mrj.application.apple.menu.about.name", "Plotter");
        //  This is supposed to set the look and feel.  Using the "cross platform"
        //  look and feel makes the menu look the same everywhere.
        try {
            UIManager.setLookAndFeel( UIManager.getCrossPlatformLookAndFeelClassName() );
        }
        catch ( Exception e ) {
            
        }
        
        //  Build all components.
        initComponents();
        
        //  Initialize and ArrayDeque to hold a list of editor windows.
        _plotEditors = new ArrayDeque<PlotEditor>();
        plotWindow.plotEditors( _plotEditors );
        
        //  This is the editor window.
        _plotEditorWindow = new PlotEditorWindow();
        
        //  This is used to make the "close" button run through our exit checks
        //  before closing the window.
        addWindowListener( this );
        
        //  Build the data menu using the current list of data.
        
        //  If other plots have not been created, make a "default" plot.
        createNewPlot( "default" );
        //  The above function makes the editor window visible, which we don't want by
        //  default.
        _plotEditorWindow.setVisible( false ); 
        
        //  this is temporary test junk
        Random random = new Random();
//        double x[] = new double[1001];
//        double y[] = new double[1001];
//        for ( int i = 0; i < 1001; ++i ) {
//            x[i] = (double)i * 0.001;
//            y[i] = random.nextDouble();
//        }
//        getPlot( "default" ).addCurve( x, y );
        Track2D newTrack = new Track2D();
        plotWindow.getPlot( "default" ).addTrack( newTrack );
        for ( int i = 0; i < 1001; ++i ) {
            newTrack.add( (double)i * 0.001, random.nextDouble() );
        }
        plotWindow.getPlot( "default" ).addGrid( Plot2DObject.X_AXIS, 0.1, Color.LIGHT_GRAY );
        plotWindow.getPlot( "default" ).addGrid( Plot2DObject.Y_AXIS, 0.1, Color.LIGHT_GRAY );
        plotWindow.getPlot( "default" ).title( "<size=1.2>Some Random Numbers" );
//    public void addLabels( int axis, Double start, Double stop, double step, 
//            Color color, String format, double ticSize, double gapSize, 
//            int justification, Double crossValue, boolean drawScale, 
//            boolean plotAlways ) {
        plotWindow.getPlot( "default" ).addLabels( Plot2DObject.X_AXIS, .25 );
        plotWindow.getPlot( "default" ).addLabels( Plot2DObject.Y_AXIS, .25 );
        
        //BLAT  These three lines can be used to demonstrate an annoying error which boils
        //      down to, basically, the problem that Java can't add.  
        //double foo = 0.1;
        //plotWindow.getPlot( "default" ).addLabels( Plot2DObject.Y_AXIS, foo / 4.0, "%.3f" );
        //plotWindow.getPlot( "default" ).limits( 0.0, 10.0, 0.0, foo );
        
        
        //plotWindow.getPlot( "default" ).addLabels( Plot2DObject.X_AXIS, null, null, .25, Color.GREEN,
         //       null, .05, 10.0, DrawObject.RIGHT_JUSTIFY, null, true, false );
//        plotWindow.getPlot( "default" ).addLabels( Plot2DObject.Y_AXIS, null, null, .25, Color.RED,
//                null, 5.0, 10.0, DrawObject.CENTER_JUSTIFY, 0.6, false, false );
        //plotWindow.getPlot( "default" ).labelColor( Color.RED ); 
        
        //  Build the "plots" menu using the current list of plots.
        buildPlotMenu();
        
        //  Set menu options - these eventually should be moved to plot editing
        //  tabs.
        drawFrameCheckBox.setState( plotWindow.getPlot( "default" ).drawFrame() );
        fillPlotCheckBox.setState( plotWindow.getPlot( "default" ).drawBackground() );
        setClipCheckBox.setState( plotWindow.getPlot( "default" ).clip() );
        
        //  This is needed so the editor will show all of the above changes.
        locateEditor( plotWindow.getPlot( "default" ) ).dataChange();
        
        //  Make sure any changes we've made take place on the displayed plot window.
        plotWindow.updateUI();

    }
    
    public void buildPlotMenu() {
        //  Clear anything existing in the plot menu.
        plotMenu.removeAll();
        //  First put the "new plot" option in place.  This is a simple item
        //  with a callback.
        JMenuItem newPlotItem = new JMenuItem();
        newPlotItem.setText( "Create New" );
        plotMenu.add( newPlotItem );
        plotMenu.add( new JSeparator() );
        newPlotItem.addActionListener(new java.awt.event.ActionListener() {
            //protected int _plotCounter;
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                //  Add the new plot to the plot list - give it a unique (and maybe
                //  temporary) name.
                ++_plotCounter;
                createNewPlot( "Plot " + _plotCounter );
            }
        });
        //  Then add a small menu for each existing plot item.
        for ( Iterator<DrawObject> iter = plotWindow.plotList().iterator(); iter.hasNext(); ) {
            final Plot2DObject thisPlot = (Plot2DObject)( iter.next() );
            JMenu newMenu = new JMenu();
            newMenu.setText( thisPlot.name() );
            plotMenu.add( newMenu );
            //  Each plot has edit and delete options.  The callback functions
            //  are passed pointers to the plots themselves as opposed to their
            //  names (which was the original design).  This *should* allow the
            //  user to make multiple plots with the same name...should they be
            //  so silly as to do so.
            JMenuItem newEdit = new JMenuItem();
            newEdit.setText( "Edit Properties" );
            newEdit.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    editPlot( thisPlot );
                }
            });
            newMenu.add( newEdit );
            JMenuItem newDelete = new JMenuItem();
            newDelete.setText( "Delete" );
            newDelete.addActionListener(new java.awt.event.ActionListener() {
                public void actionPerformed(java.awt.event.ActionEvent evt) {
                    deletePlot( thisPlot );
                }
            });
            newMenu.add( newDelete );
        }

    }
    
    /*
     * Add a new plot.  This both puts a plot in the PlotWindow list and brings
     * up the plot properties editor with a new tab for this plot.
     */
    public void createNewPlot( String name ) {
        Plot2DObject thisPlot = new Plot2DObject();
        thisPlot.name( name );
        //  These values determine where this plot will be drawn.  Fractional
        //  values will cause the plot to scale with this window.  Values larger
        //  than 1.0 will make the plot not scale.
        thisPlot.frame( 0.15, 0.1, 0.75, 0.75 );
        //  Add the plot to the list of plots being represented in this window.
        plotWindow.add2DPlot( thisPlot );
        //  Add it to the editor.
        PlotEditor newEditor = new PlotEditor( thisPlot );
        //  Make the editor properly reflect the parameters of this plot.
        newEditor.dataChange();
        //  Add the editor to our list of editors.
        _plotEditors.add( newEditor );
        //  This gives the editor a link back to the main GUI so it can order a
        //  change in the plot menu.
        newEditor.plotterUI( this );
        //  And this so it can do redraws.
        newEditor.plotWindow( plotWindow );
        _plotEditorWindow.addEditor( name, newEditor );
        //  Make the editor visible
        _plotEditorWindow.setVisible( true ); 
        //  Recreate the plot menu
        buildPlotMenu();
        //  Redraw the plots with this new plot included.
        plotWindow.updateUI();
    }
    
    /*
     * Edit this properties of this plot.  To do so, we make the plot properties
     * window visible and make the tab corresponding to this plot be on top.
     */
    public void editPlot( Plot2DObject thisPlot ) {
        if ( _plotEditorWindow == null )
            _plotEditorWindow = new PlotEditorWindow();
        _plotEditorWindow.edit( locateEditor( thisPlot ) );
        _plotEditorWindow.setVisible( true );
    }
    
    /*
     * Delete an existing plot.
     */
    public void deletePlot( Plot2DObject thisPlot ) {
        plotWindow.remove2DPlot( thisPlot );
        _plotEditorWindow.removeEditor( locateEditor( thisPlot ) );
        buildPlotMenu();
        plotWindow.updateUI();
    }
    
    /*
     * Locate the editor window that is linked to the given plot.
     */
    public PlotEditor locateEditor( Plot2DObject thisPlot ) {
        PlotEditor theEditor = null;
        for ( Iterator<PlotEditor> iter = _plotEditors.iterator(); iter.hasNext(); ) {
            PlotEditor thisEditor = iter.next();
            if ( thisEditor.plot() == thisPlot )
                theEditor = thisEditor;
        }
        return theEditor;
    }
    
    /*
     * Window event methods - we need each of these, even though we are only
     * interested in the "Closing" method.
     */
    @Override
    public void windowOpened( WindowEvent e ) {
    }
    @Override
    public void windowClosed( WindowEvent e ) {
    }
    @Override
    public void windowClosing( WindowEvent e ) {
        exitOperation();
    }
    @Override
    public void windowActivated( WindowEvent e ) { 
    }
    @Override
    public void windowDeactivated( WindowEvent e ) {     
    }
    @Override
    public void windowIconified( WindowEvent e ) { 
    }
    @Override
    public void windowDeiconified( WindowEvent e ) {     
    }
    
    /*
     * This method is called from within the constructor to initialize the form.
     * It was created originally by NetBeans, but I've adjusted it some.  Things
     * appear to work properly with this configuration, but I can't claim I
     * fully understand what it is doing with all of the layout business.
     */
    private void initComponents() {
        
        
        plotWindow = new PlotWindow();
        menuBar = new javax.swing.JMenuBar();
        fileMenu = new javax.swing.JMenu();
        newMenuItem = new javax.swing.JMenuItem();
        openMenuItem = new javax.swing.JMenuItem();
        saveMenuItem = new javax.swing.JMenuItem();
        exportMenuItem5 = new javax.swing.JMenuItem();
        jSeparator1 = new javax.swing.JPopupMenu.Separator();
        quitMenuItem = new javax.swing.JMenuItem();
        dataMenu = new javax.swing.JMenu();
        plotMenu = new javax.swing.JMenu();
        exportMenuItem7 = new javax.swing.JMenuItem();
        settingsMenu = new javax.swing.JMenu();
        exportMenuItem1 = new javax.swing.JMenuItem();
        exportMenuItem8 = new javax.swing.JMenuItem();
        frameDetailsMenu = new javax.swing.JMenu();
        drawFrameCheckBox = new javax.swing.JCheckBoxMenuItem();
        exportMenuItem11 = new javax.swing.JMenuItem();
        fillPlotCheckBox = new javax.swing.JCheckBoxMenuItem();
        exportMenuItem10 = new javax.swing.JMenuItem();
        setClipCheckBox = new javax.swing.JCheckBoxMenuItem();
        exportMenuItem9 = new javax.swing.JMenuItem();

        setTitle("Trivial Plotter");
        
        org.jdesktop.layout.GroupLayout plotWindowLayout = new org.jdesktop.layout.GroupLayout(plotWindow);
        plotWindow.setLayout(plotWindowLayout);
        plotWindowLayout.setHorizontalGroup(
            plotWindowLayout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(0, 800, Short.MAX_VALUE)
        );
        plotWindowLayout.setVerticalGroup(
            plotWindowLayout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(0, 478, Short.MAX_VALUE)
        );

        //  File menu, details and components...
        fileMenu.setText(" File ");

        newMenuItem.setText("New");
        newMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                NewButtonAction(evt);
            }
        });
        fileMenu.add(newMenuItem);

        openMenuItem.setText("Open...");
        fileMenu.add(openMenuItem);

        saveMenuItem.setText("Save");
        fileMenu.add(saveMenuItem);

        exportMenuItem5.setText("Export...");
        fileMenu.add(exportMenuItem5);
        fileMenu.add(jSeparator1);

        quitMenuItem.setText("Quit");
        quitMenuItem.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                QuitButtonAction(evt);
            }
        });
        fileMenu.add(quitMenuItem);

        menuBar.add(fileMenu);

        //  The Data menu...this is a place holder for now - details will be
        //  filled in dynamically.
        dataMenu.setText(" Data ");
        menuBar.add( dataMenu );

        //  The Plots menu....like the Data menu, details will be filled in
        //  dynamically (see buildPlotMenu()).
        plotMenu.setText( " Plots " );
        menuBar.add( plotMenu );

        //  The "Add Ons" menu allows the user to add addition DrawObjects to
        //  the plots.
        addOnsMenu = new javax.swing.JMenu();
        addOnsMenu.setText( " Add Ons " );
        menuBar.add( addOnsMenu );

        exportMenuItem7.setText("Hello World");
        exportMenuItem7.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                HelloWorldAction(evt);
            }
        });
        addOnsMenu.add(exportMenuItem7);

        settingsMenu.setText(" Settings ");

        exportMenuItem1.setText("Background Color");
        exportMenuItem1.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                BackgroundColorButtonAction(evt);
            }
        });
        settingsMenu.add(exportMenuItem1);

        exportMenuItem8.setText("Foreground Color");
        exportMenuItem8.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                ForegroundColorButtonAction(evt);
            }
        });
        settingsMenu.add(exportMenuItem8);

        frameDetailsMenu.setText("Frame Details");

        drawFrameCheckBox.setSelected(true);
        drawFrameCheckBox.setText("Draw Frame");
        drawFrameCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                DrawFrameButtonAction(evt);
            }
        });
        frameDetailsMenu.add(drawFrameCheckBox);

        exportMenuItem11.setText("Frame Color");
        exportMenuItem11.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                FrameColorButtonAction(evt);
            }
        });
        frameDetailsMenu.add(exportMenuItem11);

        fillPlotCheckBox.setSelected(true);
        fillPlotCheckBox.setText("Fill Plot Area");
        fillPlotCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                FillPlotAreaButtonAction(evt);
            }
        });
        frameDetailsMenu.add(fillPlotCheckBox);

        exportMenuItem10.setText("Plot Area Color");
        exportMenuItem10.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                PlotAreaColorButtonAction(evt);
            }
        });
        frameDetailsMenu.add(exportMenuItem10);

        setClipCheckBox.setSelected(true);
        setClipCheckBox.setText("Clip Data To Frame");
        setClipCheckBox.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                clipFrameAction(evt);
            }
        });
        frameDetailsMenu.add(setClipCheckBox);

        settingsMenu.add(frameDetailsMenu);

        exportMenuItem9.setText("exportMenuItem9");
        settingsMenu.add(exportMenuItem9);

        menuBar.add(settingsMenu);

        setJMenuBar(menuBar);
        
        org.jdesktop.layout.GroupLayout layout = new org.jdesktop.layout.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(plotWindow, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
            .add(plotWindow, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, org.jdesktop.layout.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );

        pack();
    }

    private void BackgroundColorButtonAction(java.awt.event.ActionEvent evt) {                                             
        Color newColor = JColorChooser.showDialog( this, "Background Color", plotWindow.backgroundColor() );
        if ( newColor != null ) {
            plotWindow.backgroundColor( newColor );
            plotWindow.updateUI();
        }
    }                                            

    private void NewButtonAction(java.awt.event.ActionEvent evt) {                                 
        // TODO add your handling code here:
    }                                

    private void QuitButtonAction(java.awt.event.ActionEvent evt) {                                  
        exitOperation();
    }                                 

    public double count;
    public DrawObject sillyObject;
    
    private void HelloWorldAction(java.awt.event.ActionEvent evt) {                                  
        if ( count < .9 ) {
            sillyObject = new DrawObject();
            sillyObject.color( Color.BLACK );
            plotWindow.drawObject( sillyObject );
        }
        DrawObject newObject = new DrawObject();
        newObject.text( "Hello World", 100.0, (count + 1.0) * 100.0 );
        DrawObject comObject = new DrawObject();
        String stuff = new String( "<color = 0xff0000 >Hello<bold>World</bold></color>" );
        comObject.complexText( DrawObject.CENTER_JUSTIFY, 100.0, (count + 1.0) * 100.0 + 30.0, stuff );
        //DrawObject junkObject = new DrawObject();
        //junkObject.text( "Hello World", 0.0, 0.0 );
        //comObject.add( junkObject );
        DrawObject nextObject = new DrawObject();
        nextObject.drawline( 200.0, (count + 1) * 50.0, 400, (count + 1) * 100.0 );
        nextObject.lineWidth( (count + 1) * 1.0 );
        nextObject.lineStyle( DrawObject.LINE_DASHDOT );
        sillyObject.add( nextObject );
        count = count + 1.0;
        sillyObject.add( newObject );
        sillyObject.add( comObject );
        DrawObject imObject = new DrawObject();
        try {
            BufferedImage newImage = ImageIO.read( new File( "/Users/jspitzak/Desktop/thumbnail.aspx.jpeg" ) );
            imObject.image( newImage, (count) * 30.0, 0.0 );
            imObject.translate( 300.0, (count + 1 ) * 30.0 );
            //imObject.rotate( (count + 1) * 30.0 );
            //imObject.scale( (count) * 1.0, count * 2.0 );
        }
        catch ( IOException e ) {
            System.out.println( "trouble with the damned image file: " + e.getMessage() );
        }
        sillyObject.add( imObject );
        plotWindow.repaint();
    }                                 

    private void ForegroundColorButtonAction(java.awt.event.ActionEvent evt) {                                             
        //  Set the default color for foreground drawing.
        Color newColor = JColorChooser.showDialog( this, "Foreground Color", plotWindow.foregroundColor() );
        if ( newColor != null ) {
            plotWindow.foregroundColor( newColor );
            plotWindow.updateUI();
        }
        
    }                                            

    private void DrawFrameButtonAction(java.awt.event.ActionEvent evt) {                                       
        plotWindow.getPlot( "default" ).drawFrame( drawFrameCheckBox.getState() );
        plotWindow.updateUI();
    }                                      

    private void PlotAreaColorButtonAction(java.awt.event.ActionEvent evt) {                                           
        //  Set the color for the "plot area" - the box holding the plotting data.
        //  This looks nice if it is different from the background color.
        Color newColor = JColorChooser.showDialog( this, "Plot Area Color", plotWindow.getPlot( "default" ).backgroundColor() );
        if ( newColor != null ) {
            plotWindow.getPlot( "default" ).backgroundColor( newColor );
            plotWindow.updateUI();
        }
        // TODO add your handling code here:
    }                                          

    private void FrameColorButtonAction(java.awt.event.ActionEvent evt) {                                        
        //  Set the default color for foreground drawing.
        Color newColor = JColorChooser.showDialog( this, "Frame Color", plotWindow.getPlot( "default" ).frameColor() );
        if ( newColor != null ) {
            plotWindow.getPlot( "default" ).frameColor( newColor );
            plotWindow.updateUI();
        }
        // TODO add your handling code here:
    }                                       

    private void FillPlotAreaButtonAction(java.awt.event.ActionEvent evt) {                                          
        plotWindow.getPlot( "default" ).drawBackground( fillPlotCheckBox.getState() );
        plotWindow.updateUI();
    }                                         

    private void clipFrameAction(java.awt.event.ActionEvent evt) {                                 
        plotWindow.getPlot( "default" ).clip( setClipCheckBox.getState() );
        plotWindow.updateUI();
    }                                

    private void exitOperation() {
        // Exit the program after a check for unsaved changes.
        System.out.println( "run exit checks" );
        System.exit( 0 );
    }
    /**
     * @param args the command line arguments
     */
    public static void main(String args[]) {
        java.awt.EventQueue.invokeLater(new Runnable() {

            public void run() {
                new PlotterUI().setVisible(true);
            }
        });
    }
    // Variables declaration - do not modify
    private javax.swing.JCheckBoxMenuItem drawFrameCheckBox;
    private javax.swing.JCheckBoxMenuItem fillPlotCheckBox;
    private javax.swing.JMenu frameDetailsMenu;
    private javax.swing.JMenu fileMenu;
    private javax.swing.JMenu dataMenu;
    private javax.swing.JMenu plotMenu;
    private javax.swing.JMenu addOnsMenu;
    private javax.swing.JMenu settingsMenu;
    private javax.swing.JMenuBar menuBar;
    private javax.swing.JMenuItem exportMenuItem1;
    private javax.swing.JMenuItem exportMenuItem10;
    private javax.swing.JMenuItem exportMenuItem11;
    private javax.swing.JMenuItem newMenuItem;
    private javax.swing.JMenuItem openMenuItem;
    private javax.swing.JMenuItem saveMenuItem;
    private javax.swing.JMenuItem exportMenuItem5;
    private javax.swing.JMenuItem quitMenuItem;
    private javax.swing.JMenuItem exportMenuItem7;
    private javax.swing.JMenuItem exportMenuItem8;
    private javax.swing.JMenuItem exportMenuItem9;
    private javax.swing.JPopupMenu.Separator jSeparator1;
    private javax.swing.JCheckBoxMenuItem setClipCheckBox;
    private PlotWindow plotWindow;
    
    protected PlotEditorWindow _plotEditorWindow;
    protected int _plotCounter;
    protected ArrayDeque<PlotEditor> _plotEditors;

}
