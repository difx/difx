/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxutilities;

import java.awt.Adjustable;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import javax.swing.JTextArea;

/**
 *
 * @author mguerra
 */
public class DiFXAdjustmentListener implements AdjustmentListener
{

   private JTextArea textArea;

   public void setTextArea(JTextArea area)
   {
      textArea = area;
   }

   @Override
   public void adjustmentValueChanged(AdjustmentEvent evt)
   {
      Adjustable source = evt.getAdjustable();
      if (evt.getValueIsAdjusting())
      {
         return;
      }

      if (evt.getAdjustable().getMaximum() < evt.getAdjustable().getValue()+200)
      {
         //System.out.println("Hit max - value of vertical scroll bar: " + evt.getValue() + ":" + source.getMinimum()+ ":" + source.getMaximum());
         //textArea.setCaretPosition( textArea.getDocument().getLength() );
      }
      
   }

}
