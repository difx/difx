/*
 * This window contains editors for all plots in a tab-ified list.
 */
package mil.navy.usno.plotlib;

import javax.swing.JFrame;
import javax.swing.JTabbedPane;

import java.awt.Dimension;
/**
 *
 * @author jspitzak
 */
public class PlotEditorWindow extends JFrame {
    
    public PlotEditorWindow() {
        super( "Plot Editor" );
        this.setBounds( 500, 100, 500, 500 );
        _tabbedPane = new JTabbedPane();
        Dimension d = this.getSize();
        _tabbedPane.setBounds( 0, 0, d.width, d.height );
        this.add( _tabbedPane );
    }
    
    /*
     * Add a new editor pane for a plot with this name.  The name will be used
     * as the tab title.
     */
    public void addEditor( String name, PlotEditor thisEditor ) {
        _tabbedPane.addTab( name, thisEditor );
        //  The editor needs a pointer to the tabbed pane so it can change the
        //  name associated with the tab.
        thisEditor.tabbedPane( _tabbedPane );
        this.edit( name );
    }
    
    /*
     * Remove a tab from this editor.  Don't use this function!  Names are not
     * unique!
     */
    public void removeEditor( String name ) {
        _tabbedPane.remove( _tabbedPane.indexOfTab( name ) );
    }
    
    /*
     * Remove the tab associated with the given plot editor.
     */
    public void removeEditor( PlotEditor thisEditor ) {
        _tabbedPane.remove( thisEditor );
    }
    
    /*
     * Display the tab associated with this name.  Names may not be unique, so
     * this isn't the desired function to use for this.
     */
    public void edit( String name ) {
        _tabbedPane.setSelectedIndex( _tabbedPane.indexOfTab( name ) );
    }
    
    /*
     * Display the tab for the given PlotEditor object.
     */
    public void edit( PlotEditor thisEditor ) {
        _tabbedPane.setSelectedIndex( _tabbedPane.indexOfComponent( thisEditor ) );
    }
    
    protected JTabbedPane _tabbedPane;
    
}
