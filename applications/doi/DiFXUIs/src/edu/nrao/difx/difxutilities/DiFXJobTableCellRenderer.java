/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxutilities;

import java.awt.Color;
import java.awt.Component;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;

/**
 *
 * @author mguerra
 */
public class DiFXJobTableCellRenderer extends DefaultTableCellRenderer{

   // This method is called each time a sell in a column using this renderer 
   // needs to be rendered.
   @Override
   public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, 
                                                  boolean hasFocus, int row, int column)
   {
      Component comp = null;
      try
      {
         int i = table.convertRowIndexToModel(row);
         comp = super.getTableCellRendererComponent(table,  value, isSelected,
                                                    hasFocus, i, column);

         String s =  table.getModel().getValueAt(i, column ).toString();
         if (s.equalsIgnoreCase("NOT QUEUED"))
         {
            comp.setBackground(Color.white);
            comp.setForeground(Color.black);
         }
         else if (s.equalsIgnoreCase("NOTREADY"))
         {
            comp.setBackground(Color.orange);
            comp.setForeground(Color.black);
         }
         else if (s.equalsIgnoreCase("READY"))
         {
            comp.setBackground(Color.blue);
            comp.setForeground(Color.white);
         }
         else if (s.equalsIgnoreCase("CONFLICT"))
         {
            comp.setBackground(Color.yellow);
            comp.setForeground(Color.black);
         }
         else if (s.equalsIgnoreCase("WAITING"))
         {
            comp.setBackground(Color.yellow);
            comp.setForeground(Color.black);
         }
         else if (s.equalsIgnoreCase("RUNNING"))
         {
            comp.setBackground(Color.green);
            comp.setForeground(Color.black);
         }
         else if (s.equalsIgnoreCase("KILLED"))
         {
            comp.setBackground(Color.red);
            comp.setForeground(Color.white);
         }
         else if (s.equalsIgnoreCase("COMPLETE"))
         {
            comp.setBackground(Color.white);
            comp.setForeground(Color.black);
         }
         else if (s.equalsIgnoreCase("UNKNOWN"))
         {
            comp.setBackground(Color.red);
            comp.setForeground(Color.white);
         }
         else if (s.equalsIgnoreCase("FAILED"))
         {
            comp.setBackground(Color.red);
            comp.setForeground(Color.white);
         }
         else
         {
            comp.setBackground(Color.white);
            comp.setForeground(Color.black);
         }

      }
      catch (ArrayIndexOutOfBoundsException ex)
      {
         // do nothing
         System.out.print( ex.toString() );
      }

      return( comp );

   }

   @Override
   public void validate()   {}
   @Override
   public void revalidate() {}
   
   @Override
   protected void firePropertyChange(String propertyName, Object oldValue, Object newValue)   {}
   @Override
   public    void firePropertyChange(String propertyName, boolean oldValue, boolean newValue) {}
   
}
