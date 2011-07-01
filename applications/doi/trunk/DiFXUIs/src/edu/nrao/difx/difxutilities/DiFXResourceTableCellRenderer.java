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
public class DiFXResourceTableCellRenderer extends DefaultTableCellRenderer{

   // This method is called each time a sell in a column using this renderer 
   // needs to be rendered.
   @Override
   public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, 
                                                  boolean hasFocus, int row, int column)
   {
      int i = table.convertRowIndexToModel(row);
      Component comp = super.getTableCellRendererComponent(table,  value, isSelected,
                                                             hasFocus, i, column);

      String s =  table.getModel().getValueAt(i, column ).toString();
      if (s.equalsIgnoreCase("Idle"))
      {
         comp.setBackground(Color.white);
         comp.setForeground(Color.black);
      }
      else if (s.equalsIgnoreCase("Condition"))
      {
         comp.setBackground(Color.yellow);
         comp.setForeground(Color.black);
      }
      else if (s.equalsIgnoreCase("Busy"))
      {
         comp.setBackground(Color.orange);
         comp.setForeground(Color.black);
      }
      else if (s.equalsIgnoreCase("Play"))
      {
         comp.setBackground(Color.green);
         comp.setForeground(Color.black);
      }
      else if (s.equalsIgnoreCase("Online"))
      {
         comp.setBackground(Color.green);
         comp.setForeground(Color.black);
      }
      else if (s.equalsIgnoreCase("Lost"))
      {
         comp.setBackground(Color.red);
         comp.setForeground(Color.white);
      }
      else if (s.equalsIgnoreCase("Error"))
      {
         comp.setBackground(Color.red);
         comp.setForeground(Color.white);
      }
      else
      {
         comp.setBackground(Color.white);
         comp.setForeground(Color.black);
      }

      return( comp );

      // throw new UnsupportedOperationException("Not supported yet.");
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
