/*
 * This class is used to produce a text editor widget that does basic things -
 * insert, delete, copy, paste, cut, etc. all using control keys.  Most of this
 * is handled by the JTextArea, but some things have to be added.
 */
package mil.navy.usno.widgetlib;

import java.awt.Font;
import javax.swing.JTextArea;
import javax.swing.JScrollPane;

/**
 *
 * @author jspitzak
 */
public class SimpleTextEditor extends JScrollPane {
    
    public SimpleTextEditor() {
        super();
        _textArea = new JTextArea();
        _textArea.setFont( new Font( "Courier", Font.PLAIN, 14 ) );//_textArea.getFont().getSize() ) );
        this.setViewportView( _textArea );
    }
    
    public JTextArea textArea() { return _textArea; }
    
    public void text( String newText ) {
        _textArea.setText( newText );
    }
    public void top() {
        _textArea.setSelectionStart( 0 );
        _textArea.setSelectionEnd( 0 );
    }
    public String text() { 
        String ret = _textArea.getText();
        return ret;
    }
    public void addText( String newText ) {
        _textArea.append( newText );
    }
    
    protected JTextArea _textArea;
    
}
