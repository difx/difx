package edu.nrao.difx.difxutilities;

import javax.swing.JTextArea;
import javax.swing.SwingUtilities;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
/**
 *
 * @author mguerra
 */
public class TextAreaFIFO extends JTextArea implements DocumentListener
{
   private int maxLines;

   public TextAreaFIFO(int lines)
   {
      maxLines = lines;
      getDocument().addDocumentListener(this);
   }

   public void insertUpdate(DocumentEvent e)
   {
      SwingUtilities.invokeLater( new Runnable()
      {
         public void run()
         {
            removeLines();

         }

      });
      //throw new UnsupportedOperationException("Not supported yet.");
   }

   public void removeUpdate(DocumentEvent e)
   {
      //throw new UnsupportedOperationException("Not supported yet.");
   }

   public void changedUpdate(DocumentEvent e)
   {
      //throw new UnsupportedOperationException("Not supported yet.");
   }

   public void removeLines()
   {
      Element root = getDocument().getDefaultRootElement();
      while (root.getElementCount() > maxLines)
      {
         Element firstLine = root.getElement(0);

         try
         {
            getDocument().remove(0, firstLine.getEndOffset());
         }
         catch (BadLocationException ble)
         {
            System.out.println(ble);
         }
      }
   }
}
