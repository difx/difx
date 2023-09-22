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
public class DiFXTableCellRenderer extends DefaultTableCellRenderer{

   // This method is called each time a sell in a column using this renderer 
   // needs to be rendered.
   @Override
   public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, 
                                                  boolean hasFocus, int row, int column)
   {
        Component comp = super.getTableCellRendererComponent(table,  value, isSelected,
                                                             hasFocus, row, column);
        //String s =  table.getModel().getValueAt(row, 1 ).toString();
        String s =  table.getModel().getValueAt(row, column ).toString();
        if (column == 3 && s.equalsIgnoreCase("Idle"))
        {
            comp.setBackground(Color.blue);
            comp.setForeground(Color.red);
        }
        else if (column == 3 && s.equalsIgnoreCase("Condition"))
        {
            comp.setBackground(Color.red);
            comp.setForeground(Color.blue);
        }
        else if (column == 3 && s.equalsIgnoreCase("Busy"))
        {
            comp.setBackground(Color.orange);
            comp.setForeground(Color.black);
        }
        else if (s.equalsIgnoreCase("Running"))
        {
            comp.setBackground(Color.green);
            comp.setForeground(Color.white);
        }
        else if (s.equalsIgnoreCase("Aborting"))
        {
            comp.setBackground(Color.red);
            comp.setForeground(Color.white);
        }
        else if (s.equalsIgnoreCase("Terminated"))
        {
            comp.setBackground(Color.red);
            comp.setForeground(Color.white);
        }
        else if (s.equalsIgnoreCase("Done"))
        {
            comp.setBackground(Color.orange);
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
