package edu.nrao.difx.difxutilities;

import java.awt.Color;
import java.awt.Component;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;

/**
 * Customization of DefaultTableCellRenderer used for rendering the DiFX module table
 * @author mguerra (NRAO)
 * @author Helge Rottmann (MPIfR)
 */
public class DiFXModuleTableCellRenderer extends DefaultTableCellRenderer{

   // This method is called each time a sell in a column using this renderer 
   // needs to be rendered.
   @Override
   public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, 
                                                  boolean hasFocus, int row, int column)
   {
      if (value != null)
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
         else if (s.equalsIgnoreCase("PlayInvalid"))
         {
            comp.setBackground(Color.yellow);
            comp.setForeground(Color.black);
         }
         else if (s.equalsIgnoreCase("NoMoreData"))
         {
            comp.setBackground(Color.orange);
            comp.setForeground(Color.black);
         }
         else if (s.equalsIgnoreCase("NoData"))
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
      }
      else
      {
         return null;
      }
      
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
