/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxutilities;

import java.io.*;

/**
 *
 * @author mguerra
 */
public class InputFileFilter implements FilenameFilter 
{
   public boolean accept(File dir, String name) 
   {
      if ( name.endsWith(".input") )
      {
         return true;
      }
      else 
      {   
         return false;
      }
 
    }
}
