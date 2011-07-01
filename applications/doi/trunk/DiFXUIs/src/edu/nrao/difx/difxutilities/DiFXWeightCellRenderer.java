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
public class DiFXWeightCellRenderer extends DefaultTableCellRenderer{

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
				 // Weights above 0.8 should be white.
				 // Between 0.01 and 0.8 should be yellow (or some other warning color),
				 // and below 0.01 should be red (or some other error color)
         float  weight =  Float.parseFloat(table.getModel().getValueAt(i, column).toString());
				 String state  = "";

				 // must check for null value!
				 if (table.getModel().getValueAt(i, 5) != null)
				 {
				    state = table.getModel().getValueAt(i, 5).toString();
				 }
				 
				 if ( state.equalsIgnoreCase("Play") )
				 {
					 if ( weight > 0.8f )
					 {
							comp.setBackground(Color.white);
							comp.setForeground(Color.black);
					 }
					 else if ( (weight >= 0.1f) && (weight <= 0.8f) )
					 {
							comp.setBackground(Color.yellow);
							comp.setForeground(Color.black);
					 }
					 else if (weight < 0.1f)
					 {
							comp.setBackground(Color.red);
							comp.setForeground(Color.white);
					 }
					 else // -- should never occur
					 {
							comp.setBackground(Color.red);
							comp.setForeground(Color.white);
					 }
				 }
				 else // -- not playing colors
				 {
				    comp.setBackground(Color.white);
						comp.setForeground(Color.black);

				 } // if ( state.equalsIgnoreCase("Play") )

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
