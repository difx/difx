package edu.nrao.difx.difxutilities;

import java.awt.Color;
import java.awt.Component;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;

/**
 * Customization of DefaultTableCellRenderer for rendering the DiFX queue table.
 * @author mguerra (NRAO)
 * @author Helge Rottmann (MPIfR)
 */
public class DiFXQueueTableCellRenderer extends DefaultTableCellRenderer{

   // This method is called each time a sell in a column using this renderer 
   // needs to be rendered.
   @Override
   public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, 
                                                  boolean hasFocus, int row, int column)
   {
        Component comp = super.getTableCellRendererComponent(table,  value, isSelected,
                                                             hasFocus, row, column);

        String s =  table.getModel().getValueAt(row, column).toString();
        if (s.equalsIgnoreCase("Idle"))
        {
            comp.setBackground(Color.white);
            comp.setForeground(Color.black);
        }
        else if (s.equalsIgnoreCase("Spawning"))
        {
            comp.setBackground(Color.white);
            comp.setForeground(Color.black);
        }
        else if (s.equalsIgnoreCase("Starting"))
        {
            comp.setBackground(Color.white);
            comp.setForeground(Color.black);
        }
        else if (s.equalsIgnoreCase("Running"))
        {
            comp.setBackground(Color.green);
            comp.setForeground(Color.white);
        }
        else if (s.equalsIgnoreCase("Ending"))
        {
            comp.setBackground(Color.white);
            comp.setForeground(Color.black);
        }
        else if (s.equalsIgnoreCase("Aborting"))
        {
            comp.setBackground(Color.red);
            comp.setForeground(Color.white);
        }
        else if (s.equalsIgnoreCase("Terminating"))
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
            comp.setBackground(Color.green);
            comp.setForeground(Color.white);
        }
        else if (s.equalsIgnoreCase("MpiDone"))
        {
            comp.setBackground(Color.green);
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
