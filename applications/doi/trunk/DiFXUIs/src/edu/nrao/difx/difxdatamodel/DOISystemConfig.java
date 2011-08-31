package edu.nrao.difx.difxdatamodel;

import java.io.File;

public class DOISystemConfig extends DiFXObject
{
   // Set the default values in this class, change them via XML file
   public static String  DiFXJaxbPackage = "edu.nrao.difx.xmllib.difxmessage";
   public static String  DiFXHome        = "/home/swc/difx";
   public static String  ResourcesFile  = "/cluster/difx/DiFX_trunk_64/conf/resources.difx";

   public static boolean LoggingEnabled = false;
   public static long    StatusValidDuration = 2000l;

   public static String  IpAddress      = "224.2.2.1"; 
   public static int     Port           = 52525; 
   public static int     BufferSize     = 1500;

   // Database config
   public static String DB_HOST            = "c3po.aoc.nrao.edu"; //"quigon.aoc.nrao.edu"
   public static String DB_SID             = "vlbatest";          //"vlba10"
   public static String DB_PWD             = "vlba";              //"chandra1999"
   public static String ORACLE_JDBC_DRIVER = "oracle.jdbc.driver.OracleDriver";
   public static String ORACLE_JDBC_PORT   = "1521";
   public static String DB_URL             = "jdbc:oracle:thin:@"+ DB_HOST + ":" +
                                             ORACLE_JDBC_PORT + ":" + DB_SID;

   // Default report location
   public static String DOIReportLoc       = "/users/difx/Desktop";

   // Default report location
   public static String DOIVersion         = "2.0";
   
    private static String configFile = "";

    /**
     * Gets the path to the main configuration file.
     * By default the configuration file is expected to 
     * be named DOISystemConfig.xml and is assumes to reside
     * in the conf subdirectory under the directory pointed to 
     * by the DIFXROOT environment variable.
     * 
     * @return the full path of the configuration file
     * @throws Exception in case the DIFXROOT environment has not been set
     * @throws Exception in case the configuration file does not exist
     */
    public static String getConfigFile() throws Exception
    {   
        String difxRoot = "";
        
        // look for setup file in the DIFX_ROOT/conf  directory
        if ((difxRoot = System.getenv("DIFXROOT")) != null)
        {
               configFile = difxRoot + "/conf/DOISystemConfig.xml";
               File f = new File( configFile );
               if (!f.exists())
               {
                   throw new Exception (configFile + " does not exist.");
               }
            
        }
        else
        {
            throw new Exception ("Environment variable DIFXROOT not defined");
        }
      
        return configFile;
    }

    /**
     * Sets the path to the main configuration file
     * @param ConfigFile 
     */
    public static void setConfigFile(String ConfigFile)
    {
        DOISystemConfig.configFile = ConfigFile;
    }
}
