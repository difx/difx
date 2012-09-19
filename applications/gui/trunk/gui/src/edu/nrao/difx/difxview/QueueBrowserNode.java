/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxview;

import mil.navy.usno.widgetlib.BrowserNode;

/**
 *
 * @author jspitzak
 */
public class QueueBrowserNode extends BrowserNode {
    
    public QueueBrowserNode( String name ) {
        super( name );
    }

    public boolean found() { return _found; }
    public void found( boolean newVal ) { _found = newVal; }
        
    public void id( int newVal ) { _id = newVal; }
    public Integer id() { return _id; }
    
    public void inDatabase( boolean newVal ) { _inDataBase = newVal; }
    public boolean inDatabase() { return _inDataBase; }
    
    protected boolean _persist;
    protected boolean _found;
    protected Integer _id;
    protected boolean _inDataBase;
    
}
