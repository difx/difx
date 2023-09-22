/*
* To change this template, choose Tools | Templates
* and open the template in the editor.
*/

package edu.nrao.difx.difxutilities;

import java.io.*;
import java.util.Scanner;


/**
*
* @author mguerra
*/
public final class ReadWithScanner 
{

  // PRIVATE //
  private final File fFile;
  
  /**
  * @param aFileName full name of an existing, readable file.
  */
  public ReadWithScanner(String aFileName)
  {
    fFile = new File(aFileName);  
  }
  
  /** Template method that calls {@link #processLine(String)}.  */
  public final void processLineByLine() throws FileNotFoundException 
  {
    Scanner scanner = new Scanner(fFile);
    try 
    {
      //first use a Scanner to get each line
      while ( scanner.hasNextLine() )
      {
        processLine( scanner.nextLine() );
      }
    }
    finally 
    {
      //ensure the underlying stream is always closed
      scanner.close();
    }
  }
  
  /** 
  * Overridable method for processing lines in different ways.
  *  
  * <P>This simple default implementation expects simple name-value pairs, separated by an 
  * '=' sign. Examples of valid input : 
  * <tt>height = 167cm</tt>
  * <tt>mass =  65kg</tt>
  * <tt>disposition =  "grumpy"</tt>
  * <tt>this is the name = this is the value</tt>
  */
  protected void processLine(String aLine)
  {
    //use a second Scanner to parse the content of each line 
    Scanner scanner = new Scanner(aLine);
    scanner.useDelimiter(":");
    
    if ( scanner.hasNext() )
    {
      String name = scanner.next();
      String value = scanner.next();
      log("Name is : " + quote(name.trim()) + ", and Value is : " + quote(value.trim()) );
    }
    else 
    {
      log("Empty or invalid line. Unable to process.");
    }
    
    //(no need for finally here, since String is source)
    scanner.close();
  }
  
  private static void log(Object aObject)
  {
    System.out.println(String.valueOf(aObject));
  }
  
  private String quote(String aText)
  {
    String QUOTE = "'";
    return QUOTE + aText + QUOTE;
  }
    
   /**
   * @param args the command line arguments
   */
   public static void main(String[] args) throws FileNotFoundException 
   {
      log("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>");

      //ReadWithScanner parser = new ReadWithScanner("C:\\Temp\\test.txt");
      //ReadWithScanner inputParser = new ReadWithScanner
      //   ("/users/mguerra/A007 - DiFX Files/DiFXProjectsJobsFolder/mt826/job420.000.input");       
      //inputParser.processLineByLine();
      
      //ReadWithScanner parser = new ReadWithScanner("C:\\Temp\\test.txt");
      ReadWithScanner scanParser = new ReadWithScanner
         ("/users/mguerra/A007 - DiFX Files/DiFXProjectsJobsFolder/mt826/job420.000.calc");  
      scanParser.processLineByLine();
      
      log("Done.");
   }

}

