/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxutilities;

import java.awt.Component;
import javax.swing.AbstractCellEditor;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.table.TableCellEditor;

/**
 *
 * @author mguerra
 */
public class DiFXTableCellEditor extends AbstractCellEditor implements TableCellEditor{

   // This is the component that will handle the editing of the cell value
   JComponent component = new JCheckBox();
   Boolean currCheckBoxValue;
   Boolean checkBoxValue;
   
   // This method is called when editing is completed. It must return the new 
   // value to be stored in the cell
   @Override
   public Object getCellEditorValue()
   {
      return currCheckBoxValue;
      //throw new UnsupportedOperationException("Not supported yet.");
   }

   // This method is called when a cell is edited by the user.
   @Override
   public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column)
   {
      // 'value' is the cell located at (row, col)
      String s = table.getValueAt(row, column).toString();
      currCheckBoxValue = (Boolean)value;

      //if (isSelected)
      //{
         // cell (and perhaps other cells) are selected
         // String s = table.getValueAt(row, column).toString();
         //checkBoxValue = table.getValueAt(row, column).toString();
         //component.setEnabled(true);
      //   checkBoxValue = true;
      //}
      
      // configure the component with the specified value
      // ((JTextField)component).setText((String)value);
      
      // Return the configured component
      component.setAlignmentX(JComponent.CENTER_ALIGNMENT);
      component.setAlignmentY(JComponent.CENTER_ALIGNMENT);
      return (component);
      
      // throw new UnsupportedOperationException("Not supported yet.");
   }

}
