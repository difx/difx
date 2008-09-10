package difx;

import java.io.File;
import java.io.IOException;

/**
 * <p>Überschrift: </p>
 *
 * <p>Beschreibung: </p>
 *
 * <p>Copyright: Copyright (c) 2008</p>
 *
 * <p>Organisation: Max-Planck-Institut für Radioastronomie (MPIfR) Bonn, Germany</p>
 *
 * @author Helge Rottmann
 * @version 1.0
 */
public class Auxiliary
{
    /**
     * See description of terminateOnError(String message, int exitCode). A default exit code of 1 is used.
     * @param message String
     */
    public static void terminateOnError(String message)
    {
        terminateOnError(message, 1);

    }
    /**
     * Central exit routine. Use whenever DifXgui needs to be abnormally terminated.
     * @param message String The message string to print on exiting.
     * @param exitCode int The exit code
     */
    public static void terminateOnError(String message, int exitCode)
    {
        System.err.println(message);
        System.exit(exitCode);
    }
    /**
  * Extracts the filename from a filepath consisting of path + filename.
  * @param sFilePath - the input filepath
  * @return the filename stripped of any leading filepath.
  * @author Synaptek Software {@linkPlain http://www.synaptek.de}
  */
 public static String StripFilePath(String sFilePath)
 {

   int nLastSep;
   int nEndIdx;

   // Path contains a trailing '/'
   if (sFilePath.endsWith("/"))
   {
     nLastSep = sFilePath.lastIndexOf('/', sFilePath.length() - 2);
     nEndIdx = sFilePath.length() - 1;
   }
   else
   {
     nLastSep = sFilePath.lastIndexOf('/');
     nEndIdx = sFilePath.length();
   }

   if (nLastSep == -1) // no Path separator
   {
     return (sFilePath);
   }
   else
   {
     return (sFilePath.substring(nLastSep + 1, nEndIdx));
   }
 }

 /**
  * Extracts the path from a filepath consisting of path + filename.
  * @param sFilePath - the input filepath
  * @return the path stripped of any trailing filename.
  * @author Synaptek Software {@linkPlain http://www.synaptek.de}
  */
 public static String StripFileName(String sFilePath)
 {

   int nLastSep;
   int nEndIdx;

   // Path contains a trailing '/'
   if (sFilePath.endsWith("/"))
   {
     nLastSep = sFilePath.lastIndexOf('/', sFilePath.length() - 2);
     nEndIdx = sFilePath.length() - 1;
   }
   else
   {
     nLastSep = sFilePath.lastIndexOf('/');
     nEndIdx = sFilePath.length();
   }

   if (nLastSep == -1) // no Path separator
   {
     return (sFilePath);
   }
   else
   {
     return (sFilePath.substring(0, nLastSep));
   }
 }



    /**
     * Obtains the path of the application.
     * @param mainClass String The class name of the main class of the application
     * @return String the apllication path
     */
    public static String getApplicationPath(Class mainClass)
    {
        String sep = java.io.File.separator;

        java.net.URL location = mainClass.getProtectionDomain().getCodeSource().getLocation();
        String path = location.getPath().trim();

        // check if application was started from a jar or other file
        if (!path.endsWith(sep))
        {
            path = StripFileName(path);
            path += sep;
        }

        return (path);


    }

    /**
     * Checks if a file given by its filename exists.
     * @param sFilename - Filename of the file to check
     * @return <code>true</code> - if the file exists
     * @return <code>false</code> - otherwise
     * @author Synaptek Software {@linkPlain http://www.synaptek.de}
     */
    public static boolean FileExists(String filename)
    {
      File testfile = new File(filename);

      return (testfile.exists());
  }
}
