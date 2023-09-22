/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxutilities;

import javax.swing.JLabel;
import javax.swing.table.TableCellRenderer;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.table.*;

/**
 *
 * @author mguerra
 */
public class DiFXColorRenderer extends JLabel implements TableCellRenderer
{
   // Color each row in a JTable

   private String columnName;

   public DiFXColorRenderer(String column)
   {
      this.columnName = column;
      setOpaque(true);
   }

   @Override
   public Component getTableCellRendererComponent(JTable  table,    Object value, boolean isSelected,
                                                  boolean hasFocus, int    row,   int     column)
      {
      Object columnValue=table.getValueAt(row,table.getColumnModel().getColumnIndex(columnName));

      if (value != null) setText(value.toString());
      if(isSelected)
          {
          setBackground(table.getSelectionBackground());
          setForeground(table.getSelectionForeground());
      }
      else
      {
          setBackground(table.getBackground());
          setForeground(table.getForeground());
          if (columnValue.equals("1")) setBackground(java.awt.Color.pink);
          if (columnValue.equals("2")) setBackground(java.awt.Color.green);
          if (columnValue.equals("3")) setBackground(java.awt.Color.red);
          if (columnValue.equals("4")) setBackground(java.awt.Color.blue);

      }
      return this;
   }
}

