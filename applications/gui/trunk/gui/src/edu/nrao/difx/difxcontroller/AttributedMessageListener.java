/*
 * This is a simple message listener structure that allows us to include a
 * DifxMessage as data.
 */
package edu.nrao.difx.difxcontroller;

import java.util.EventListener;
import edu.nrao.difx.xmllib.difxmessage.DifxMessage;

/**
 *
 * @author jspitzak
 */
public interface AttributedMessageListener extends EventListener {
    
    public void update( DifxMessage data );
    
}
