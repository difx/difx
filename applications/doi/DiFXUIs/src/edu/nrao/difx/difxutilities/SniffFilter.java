/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxutilities;

import java.io.File;
import javax.swing.*;
import javax.swing.filechooser.*;

/**
 *
 * @author mguerra
 */
public class SniffFilter extends FileFilter {

    // Accept all directories and .input files
    public boolean accept(File f) 
    {
        if (f.isDirectory()) 
        {
            return true;
        }

        String extension = Utils.getExtension(f);
        if (extension != null) 
        {
            if ( extension.equals(Utils.input) ||
                 extension.equals(Utils.calc ) ) 
            {
              return true;
            } 
            else 
            {
              return false;
            }
        }

        return false;
    }

    //The description of this filter
    public String getDescription() 
    {
        return "Project Files";
    }

}
