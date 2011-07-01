/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.simcontroller;

import edu.nrao.difx.difxsim.*;

import edu.nrao.difx.xmllib.difxmessage.*;

import javax.swing.*;

/**
 *
 * @author mguerra
 */
public class SimController {

    // Allow only one instance of thread manager and view, and the simulator 
    // does not have a data model
    ThreadManager threadMgr;
    DiFXSimGUI    simView;
    int           mMsgRxCount = 0;
    
    public SimController() 
    {
        // thread manager, set the view in the Initialize method
        threadMgr = new ThreadManager();
    }
    
    public void Initialize(DiFXSimGUI view)
    {
        // initialize the view, there is not a datamodel for the simulator.
        // intitialization of view can be done in constructor, the view passes
        // itself to contructor upon instantiation of controller.
        simView = view;        
    }
    
    public void StartController() throws InterruptedException
    {
        if (simView != null)
        {
           threadMgr.GetMessageQueue().SetController(this);
           threadMgr.startThreads();
        }
    }    
    
    public void StopController()
    {
        threadMgr.stopThreads();        
    }    
    
    public void Update()
    {        
        // enable/disable control on view, service calls to model is not required
        // System.out.println("**************** Sim Controller update. \n");
    }
    
    public void ProcessMessage(DifxMessage message)
    {        
        // message queue call back, so controller can act upon message
        System.out.println("**************** Sim Controller process message. \n");
        
        // update the view
        mMsgRxCount++;
        Header header = message.getHeader();
        Body   body   = message.getBody();
        String tempStr = Integer.toString(mMsgRxCount) + ": " + 
                           header.getType() + " " + header.getIdentifier() + " " + 
                           header.getMpiProcessId() + " " + header.getFrom() + " " +
                           header.getTo() + " " + "\n";
        
        simView.UpdateRxTextArea(tempStr.toString());
        //simView.UpdateRxTextArea(message.toString());

        // if DiFXCommand message, return response
        
        // maybe add a message to generate some status or something
    }

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) 
    {
        // TODO code application logic here
        
    }

}
