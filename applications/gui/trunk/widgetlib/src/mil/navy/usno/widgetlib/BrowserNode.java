/*
 * This is a generic browser node which can be used as part of a browser display
 * hierarchy (the NodeBrowserPane is designed to contain a hierarchy of BrowserNodes).
 * This class can contain a list of "child" BrowserNodes, and be part of a list
 * under a "parent".  A BrowserNode can be "open" or "closed", which determines 
 * whether is children are displayed or not.
 * 
 * What makes this class different from normal browser content is that because it
 * is a JPanel it is not limited to text, nor limited in size and shape for that
 * matter.  It can contain other widgets - buttons, drawings, etc.
 * 
 * By default this class has a number of controls included.  There is a little
 * arrow drawn on the left side to indicate that it has children and whether it
 * is open or not.  There is a popup menu that can be accessed by the right mouse
 * button or by clicking an button with a little down arrow (this button is not
 * visible by default, but can be made so using the visiblePopupButton() function).
 * These necessitate the capturing of mouse motion and click events, the functions
 * for which can be overridden.
 * 
 * Each BrowserNode is indented by an multiple of its level in the parent/child
 * hierarchy.  This can be changed using the levelOffset() function.  The default
 * offset is 30 pixels, which works pretty well with the open/close arrow.
 */
package mil.navy.usno.widgetlib;

import java.awt.Color;
import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.JButton;
import javax.swing.JPopupMenu;

import java.awt.Graphics;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Container;
import java.awt.Insets;

import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseEvent;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import java.util.ArrayDeque;
import java.util.Vector;
import java.util.Iterator;
import java.util.Collections;
import java.util.Comparator;

import javax.swing.event.EventListenerList;

/**
 *
 * @author jspitzak
 */
public class BrowserNode extends JPanel implements MouseListener, MouseMotionListener {
    
    public BrowserNode( String name ) {
        _name = name;
        _open = true;
        _inBounds = true;
        _showThis = true;
        _ySize = 20;
        _resizeTopBarSize = 20;
        _xSize = 500;
        _children = new Vector<BrowserNode>();
        setBounds( 0, 0, _xSize, _ySize );
        setLayout( null );
        addMouseListener( this );
        addMouseMotionListener( this );
        _mouseIn = false;
        _label = new JLabel( name );
        _label.setFont( new Font( _label.getFont().getFamily(), Font.BOLD, _label.getFont().getSize() ) );
        _labelWidth = 150;
        this.add( _label );
        _popupButton = new JButton( "\u25be" );  //  this is a little down arrow
        _popupButton.setMargin( new Insets( 0, 0, 2, 0 ) );
        _popupButton.setVisible( false );
        _popupButton.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                if ( _popup != null )
                    generatePopupMenu();
                    _popup.show( _popupButton, 0, 0 );
            }
        });
        this.add( _popupButton );
        createAdditionalItems();
        setLevel( 0 );
        _highlightColor = Color.LIGHT_GRAY;
        _backgroundColor = Color.WHITE;
        _arrowColor = Color.GRAY;
        _levelOffset = 30;
        _selected = false;
    }
    
    /*
     * This method is meant to be inherited - it adds items to the node (buttons
     * and popup menus, etc).
     */
    public void createAdditionalItems() {
    }
    
    /*
     * Also meant to be inherited - create the popup menu on the fly (when the button
     * that raises it is pushed).  This is optional, basically.  If you have a static
     * popup menu it can be created once in "createAdditionalItems()".  Note that if
     * you create the popup dynamically the first thing you should do is "clear()"
     * it.
     */
    public void generatePopupMenu() {
    }
    
    /*
     * Add a "selection" button to this browser node.  The selection button has 
     * "selected" and "unselected" String labels (specified in the command line).  When
     * a browser item is "selected" the selected label will be drawn, otherwise the
     * unselected label will be drawn.  Selected and unselected items also have colors
     * associated with them.  The displayed string is the ONLY part of the button that
     * is visible (i.e. no outline).  Clicking on the button selects or deselects the
     * browser node.  Functions do this too.  Other functions can be used to determine
     * whether the node has been selected.
     */
    public void addSelectionButton( String unselectedLabel, String selectedLabel ) {
        if ( unselectedLabel == null )
            _unselectedLabel = "\u2606";  //  empty star
        else
            _unselectedLabel = unselectedLabel;
        if ( selectedLabel == null )
            _selectedLabel = "\u2605";  //  filled star
        else
            _selectedLabel = selectedLabel;
        _selectedButton = new JButton( _unselectedLabel );
        _selectedButton.setBorderPainted( false );
        _selectedButton.setContentAreaFilled( false );
        _selectedButton.setMargin( new Insets( 0, 0, 2, 0 ) );
        _selectedButton.setForeground( Color.BLACK );
        _selectedButton.setFont( new Font( "Dialog", Font.BOLD, 14 ) );
        _selectedButton.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                selectionButtonAction();
            }
        });
        this.add( _selectedButton );
        _selectedColor = new Color( 200, 100, 0 );  //  default selection color - kind of brownish orange
        _unselectedColor = Color.BLACK;
        //  By default the button is located at the upper right corner of the browser - it can
        //  be repositioned anywhere of course.
        _selectedButton.setBounds( 0, 2, 20, 20 );
    }
    
    /*
     * Set the bounds for the selected button.  Slightly messy way to do this.
     */
    public void selectionButtonBounds( int x, int y, int w, int h ) {
        _selectedButton.setBounds( x, y, w, h );
    }
    
    /*
     * Set the colors for selected, unselected.
     */
    public void selectedColor( Color newColor ) { _selectedColor = newColor; }
    public void unselectedColor( Color newColor ) { _unselectedColor = newColor; }
    
    /*
     * Select or unselect this item.  This is the callback for the button.
     */
    public void selectionButtonAction() {
        _selected = !_selected;
        checkSelectionSetting();
    }
    
    public void checkSelectionSetting() {
        if ( _selected ) {
            _selectedButton.setText( _selectedLabel );
            _selectedButton.setForeground( _selectedColor );
        }
        else {
            _selectedButton.setText( _unselectedLabel );
            _selectedButton.setForeground( _unselectedColor );
        }
    }
    
    /*
     * Set or get the selected value from the outside.
     */
    public boolean selected() { return _selected; }
    public void selected( boolean newVal ) {
        _selected = newVal;
        checkSelectionSetting();
    }
    
    /*
     * Figure out the y position where this object will be drawn, as well
     * as whether it is drawn at all.  This information is also figured out for
     * any child objects.  The y position depends on the size of the parent of
     * this object, as well as any preceding siblings.
     */
    public int setDrawConditions( int yOffset, boolean open ) {
        int height = 0;
        int width = 0;
        //  Add the height of this object if its parent object is "open" (which
        //  indicates it should be drawn).
        if ( open ) {
            Dimension d = getSize();
            height += _ySize;
            width = d.width;
            if ( _showThis )
                this.setVisible( true );
            else
                this.setVisible( false );
        }
        else {
            this.setVisible( false );
        }
        //  Get the heights of all children, which are added to the height of this
        //  object.  These heights will be zero if this object isn't open, but
        //  we need to do this anyway to make the children not visible in that
        //  instance.
        for ( Iterator<BrowserNode> iter = _children.iterator(); iter.hasNext(); ) {
            height += iter.next().setDrawConditions( height, _open && open );
        }
        //  Set the bounds of this object such that it contains all of its
        //  children.  We only bother doing this if the parent object is open, 
        //  thus making this object visible.
        if ( open ) {
            this.setBounds( 0, yOffset, width, height );
        }
        if ( !_showThis )
            height = 0;
        if ( yOffset + height < 0 )
            inBounds( false );
        else
            inBounds( true );
        return height;
    }
    
    /*
     * Set the "level" of this object.  This determines whether it has a button
     * for opening and closing children, and where it is.  It also positions
     * other items in the node.
     */
    public void setLevel( int newLevel ) {
        _level = newLevel;
        positionItems();
    }
    
    /*
     * Adjust the position of items to correspond to the current level.  This
     * method should be overridden by any inheriting classes that add new items.
     */
    public void positionItems() {
        _label.setBounds( _level * _levelOffset + _xOffset, 0, _labelWidth, _ySize );
    }
    
    /*
     * Set the height of this object.
     */
    public void setHeight( int newVal ) {
        _ySize = newVal;
        Dimension d = this.getSize();
        this.setSize( d.width, _ySize );
        positionItems();
    }
    
    /*
     * Set the width of the object.
     */
    public void setWidth( int newVal ) {
        _xSize = newVal;
        Dimension d = this.getSize();
        this.setSize( _xSize, d.height );
        positionItems();
        //  Do we really want to do this?  This makes all child nodes the same
        //  width....which seems like the right thing to do for my specific purposes
        //  right now, but maybe isn't really.
        for ( Iterator<BrowserNode> iter = _children.iterator(); iter.hasNext(); ) {
            iter.next().setWidth( _xSize );
        }
    }
    
    /*
     * Add a new child object to this object.  This is both "added" to the panel
     * and added to the list of children.
     */
    public void addChild( BrowserNode newChild ) {
        newChild.setLevel( _level + 1 );
        _children.add( newChild );
        this.add( newChild );
        newChild.addResizeEventListener(new ActionListener() {
            public void actionPerformed( ActionEvent e ) {
                respondToResizeEvent();
            }
        });
    }
    
    /*
     * Remove a child.
     */
    public void removeChild( BrowserNode thisChild ) {
        _children.remove( thisChild );
        this.remove( thisChild );
    }
    
    /*
     * Clear all children.
     */
    public void clearChildren() {
        if ( _children.size() > 0 ) {
            //  Clear the children of the children first...
            try {
                for ( Iterator<BrowserNode> iter = _children.iterator(); iter.hasNext(); ) {
                    BrowserNode shoot = iter.next();
                    shoot.clearChildren();
                    this.remove( shoot );
                    iter.remove();
                }
            } catch ( java.util.ConcurrentModificationException e ) {}
        }
    }
    
    /*
     * This function responds to a resize event in a child.  It triggers a redraw
     * of this item and kicks the resize event again.
     */
    public void respondToResizeEvent() {
        this.updateUI();
        dispatchResizeEvent();
        this.updateUI();
    }
    
    public void backgroundColor( Color newColor ) {
        _backgroundColor = newColor;
    }
    public Color backgroundColor() { return _backgroundColor; }
    
    public void highlightColor( Color newColor ) {
        _highlightColor = newColor;
    }
    public Color highlightColor() { return _highlightColor; }
    
    public void arrowColor( Color newColor ) {
        _arrowColor = newColor;
    }
    public Color arrowColor() { return _arrowColor; }
    
    //public ArrayDeque<BrowserNode> children() {
    public Vector<BrowserNode> children() {
        return _children;
    }
    
    public Iterator<BrowserNode> childrenIterator() {
        return _children.iterator();
    }
    
    /*
     * Sort the items in the vector based on their name.
     */
    public void sortByName() {
        if ( _nameSort == null )
            _nameSort = new NameSort();
        Collections.sort( _children, _nameSort );
    }
    
    /*
     * The "comparator" used to do the above sorting.
     */
    class NameSort implements Comparator {
        
        public int compare( Object o1, Object o2 ) {
            return ( (BrowserNode)o1 ).name().compareTo( ( (BrowserNode)o2 ).name() );
        }
        
    }
    
    public NameSort _nameSort;

    @Override
    public void paintComponent( Graphics g ) {
        if ( !_inBounds )
            return;
        super.paintComponent( g );
        Dimension d = this.getSize();
        if ( _mouseIn )
            g.setColor( _highlightColor );
        else
            g.setColor( _backgroundColor );
        g.fillRect( 0, 0, d.width, d.height );
        //  Draw the open/close arrow if there are child nodes.
        if ( _children.size() > 0 ) {
            int xpts[] = new int[3];
            int ypts[] = new int[3];
            int levelOffset = ( _level - 1 ) * _levelOffset + _xOffset;
            int levelOffsetY = 0;
            if ( _yLevelOffset != null ) {
                levelOffsetY = _yLevelOffset;
            }
            if ( _open ) {
                //  "Open" objects have an arrow that points down.
                xpts[0] = levelOffset + 10;
                xpts[1] = levelOffset + 22;
                xpts[2] = levelOffset + 16;
                ypts[0] = levelOffsetY + 5;
                ypts[1] = levelOffsetY + 5;
                ypts[2] = levelOffsetY + 15;
            } else {
                xpts[0] = levelOffset + 11;
                xpts[1] = levelOffset + 11;
                xpts[2] = levelOffset + 21;
                ypts[0] = levelOffsetY + 4;
                ypts[1] = levelOffsetY + 16;
                ypts[2] = levelOffsetY + 10;
            }
            g.setColor( _arrowColor );
            g.fillPolygon( xpts, ypts, 3 );
        }
    }
    
    @Override
    public void mouseClicked( MouseEvent e ) {
        int levelOffset = ( _level - 1 ) * _levelOffset + _xOffset;
        if ( ( e.getX() > levelOffset + 5 && e.getX() < levelOffset + 25 ) || 
                ( _resizeOnTopBar && e.getY() < _resizeTopBarSize ) ) {
            _open = !_open;
            labelCheck();
            this.updateUI();
            dispatchResizeEvent();
        }
        else {
            Container foo = this.getParent();
            foo.dispatchEvent( e );
        }
   }
    
    @Override
    public void mouseEntered( MouseEvent e ) {
        _mouseIn = true;
        this.updateUI();
    }
    
    @Override
    public void mouseExited( MouseEvent e ) {
        _mouseIn = false;
        this.updateUI();
    }
    
    @Override
    public void mousePressed( MouseEvent e ) {
        if ( e.getButton() == MouseEvent.BUTTON3 && _popup != null ) {
            generatePopupMenu();
            _popup.show( e.getComponent(), e.getX(), e.getY() );
        }
        else {
            Container foo = this.getParent();
            foo.dispatchEvent( e );
            mousePressEvent( e );
        }
    }
    
    /*
     * Allow overriding functions to use this event.
     */
    public void mousePressEvent( MouseEvent e ) {}
    
    @Override
    public void mouseReleased( MouseEvent e ) {
        Container foo = this.getParent();
        foo.dispatchEvent( e );
    }
    
    @Override
    public void mouseMoved( MouseEvent e ) {
        if ( !mouseMoveEvent( e ) ) {
            Container foo = this.getParent();
            foo.dispatchEvent( e );
        }
    }
    
    /*
     * Allow overriding functions to use this event.
     */
    public boolean mouseMoveEvent( MouseEvent e ) {
        return false;
    }
    
    @Override
    public void mouseDragged( MouseEvent e ) {
        if ( !mouseDragEvent( e ) ) {
            Container foo = this.getParent();
            foo.dispatchEvent( e );
        }
    }
    
    /*
     * Allow overriding functions to use this event.
     */
    public boolean mouseDragEvent( MouseEvent e ) {
        return false;
    }
    
    public String name() {
        return _name;
    }
    
    public void name( String newName ) {
        _name = newName;
        labelCheck();
        this.updateUI();
    }
    
    /*
     * Occasionally the width of the label needs to be set.
     */
    public void labelWidth( int newVal ) { _labelWidth = newVal; }
    
    /*
     * Set whether this object is "in bounds", i.e. whether it can be displayed.
     */
    public void inBounds( boolean newVal ) {
        _inBounds = newVal;
    }
    
    public void levelOffset( int newVal ) { _levelOffset = newVal; }
    public int levelOffset() { return _levelOffset; }
    public void visiblePopupButton( boolean newVal ) { _popupButton.setVisible( newVal ); }
    
    public void addResizeEventListener( ActionListener a ) {
        if ( _resizeEventListeners == null )
            _resizeEventListeners = new EventListenerList();
        _resizeEventListeners.add( ActionListener.class, a );
    }

    protected void dispatchResizeEvent() {
        if ( _resizeEventListeners == null )
            return;
        Object[] listeners = _resizeEventListeners.getListenerList();
        // loop through each listener and pass on the event if needed
        int numListeners = listeners.length;
        for ( int i = 0; i < numListeners; i+=2 ) {
            if ( listeners[i] == ActionListener.class )
                ((ActionListener)listeners[i+1]).actionPerformed( null );
        }
    }
    
    public boolean showThis() { return _showThis; }
    public void showThis( boolean newVal ) { _showThis = newVal; }
    
    public void open( boolean newVal ) { 
        _open = newVal;
        labelCheck();
    }
    public boolean open() { return _open; }
    
    public void yLevelOffset( int newVal ) { _yLevelOffset = new Integer( newVal ); }
    
    /*
     * This sets the whole top of the BrowserNode to be sensitive to open/close
     * operations (not just the resize arrow).
     */
    public void resizeOnTopBar( boolean newVal ) { 
        _resizeOnTopBar = newVal;
    }
    
    public void resizeTopBarSize( int newSize ) {
        _resizeTopBarSize = newSize;
    }
    
    public int xOffset() { return _xOffset; }
    public void xOffset( int newVal ) { _xOffset = newVal; }
    
    /*
     * Add the number of child objects in parenthesis to the name when this object
     * is closed.
     */
    public void addCountWhenClosed( boolean newVal ) { _addCountWhenClosed = newVal; }
    
    /*
     * Always show the count of child objects.
     */
    public void addCountAlways( boolean newVal ) { _addCountAlways = newVal; }
    
    /*
     * Change the label to reflect the name and other things.
     */
    public void labelCheck() {
        if ( _open ) {
                if ( _addCountAlways )
                _label.setText( _name + " (" + _children.size() + ")" );
            else
                _label.setText( _name );
        }
        else {
            if ( _addCountWhenClosed || _addCountAlways )
                _label.setText( _name + " (" + _children.size() + ")" );
            else
                _label.setText( _name );
        }
    }

    
    protected boolean _open;
    protected boolean _showThis;
    protected boolean _inBounds;
    protected int _ySize;
    protected int _xSize;
    protected int _level;
//    protected ArrayDeque<BrowserNode> _children;
    protected Vector<BrowserNode> _children;
    protected boolean _mouseIn;
    protected Color _backgroundColor;
    protected Color _highlightColor;
    protected Color _arrowColor;
    protected JLabel _label;
    protected JPopupMenu _popup;
    protected JButton _popupButton;
    protected int _levelOffset;
    protected int _labelWidth;
    protected EventListenerList _resizeEventListeners;
    protected Integer _yLevelOffset;
    protected boolean _resizeOnTopBar;
    protected int _resizeTopBarSize;
    protected int _xOffset;
    protected String _name;
    protected boolean _addCountWhenClosed;
    protected boolean _addCountAlways;
    
    protected boolean _selected;
    protected JButton _selectedButton;
    protected String _selectedLabel;
    protected String _unselectedLabel;
    protected Color _selectedColor;
    protected Color _unselectedColor;

}
