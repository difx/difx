/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxutilities;

/**
 *
 * @author mguerra
 */
import edu.nrao.difx.difxdatamodel.DiFXSystemStatus;
import edu.nrao.difx.difxdatamodel.Job;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

public class DiFXDialog extends JDialog implements ActionListener {

   private Job currJob = null;

   public DiFXDialog(JFrame parent, String title, String message, Job job)
   {
      super(parent, title, false);
      if (parent != null)
      {
         Dimension parentSize = parent.getSize();
         Point p = parent.getLocation();
         setLocation(p.x + parentSize.width / 4, p.y + parentSize.height / 4);
      }

      // position on top
      super.setLocationRelativeTo(parent);

      // set job
      currJob = job;

      JPanel messagePane = new JPanel();
      JLabel label = new JLabel(message);
      label.setHorizontalAlignment(JLabel.LEFT);
      messagePane.add(label);
      getContentPane().add(messagePane);
      JPanel buttonPane = new JPanel();
      JButton buttonComplete = new JButton("Complete");
      JButton buttonFail     = new JButton("Fail");
      JButton buttonReady    = new JButton("Ready");
      JButton buttonUnknown  = new JButton("Unknown");
      buttonPane.add(buttonComplete);
      buttonPane.add(buttonFail);
      buttonPane.add(buttonReady);
      buttonPane.add(buttonUnknown);
      buttonComplete.addActionListener(new ActionListener() {
         @Override
         public void actionPerformed(ActionEvent e)
         {
            setJobState(1);
            setVisible(false);
            dispose();
         }
      });      
      buttonFail.addActionListener(new ActionListener() {
         @Override
         public void actionPerformed(ActionEvent e)
         {
            setJobState(2);
            setVisible(false);
            dispose();
         }
      });
      buttonReady.addActionListener(new ActionListener() {
         @Override
         public void actionPerformed(ActionEvent e)
         {
            setJobState(3);
            setVisible(false);
            dispose();
         }
      });
      buttonUnknown.addActionListener(new ActionListener() {
         @Override
         public void actionPerformed(ActionEvent e)
         {
            setJobState(4);
            setVisible(false);
            dispose();
         }
      });

      getContentPane().add(buttonPane, BorderLayout.SOUTH);
      setDefaultCloseOperation(DISPOSE_ON_CLOSE);
      pack();
      setVisible(true);
   }

   @Override
   public void actionPerformed(ActionEvent e)
   {
      setVisible(false);
      dispose();
   }

   public void setJobState(int opt)
   {
      if (currJob != null)
      {
         // process option
         switch (opt)
         {
            case 1: // complete
            {
               // change current job state to COMPLETE, run next job
               currJob.setState(DiFXSystemStatus.JobStates.COMPLETE);
               break;
            }
            case 2: // failed
            {
               // change current job state to failed
               currJob.setState(DiFXSystemStatus.JobStates.FAILED);
               break;
            }
            case 3: // ready
            {
               // change queue to idle and current job state to ready
               currJob.setState(DiFXSystemStatus.JobStates.READY);
               break;
            }
            case 4: // unknown
            {
               // change queue to idle and current job state to ready
               currJob.setState(DiFXSystemStatus.JobStates.UNKNOWN);
               break;
            }
            default: // unknown
            {
               // leave in unknown and change queue to error state
               currJob.setState(DiFXSystemStatus.JobStates.UNKNOWN);
               break;
            }

         } // switch (opt)
      }

   }

   public static void main(String[] a)
   {
      DiFXDialog dlg = new DiFXDialog(new JFrame(), "title", "message", null);
   }


}