package edu.nrao.difx.simcontroller;


public class DiFXSystemConfig
{
   public static String  DiFXJaxbPackage = "edu.nrao.vlba.xmllib.difxmessage";
   
   public static String  DiFXHome       = "/home/swc/difx";
   public static String  SimHome        = "/users/mguerra/A007 - DiFX Files/DiFXXMLFiles";
   public static byte    NumMark5Units  = 24;
   public static byte    NumProcessors  = 10;
   public static byte    NumHeadNodes   = 1;
   public static boolean LoggingEnabled = false;

   public static String  IpAddress      = "224.2.2.4"; //"224.2.2.1", "224.2.2.2" "224.2.2.4", "224.2.2.5"
   public static int     Port           = 50201;       // 50200, 50201
   public static int     BufferSize     = 1024;

   public static String  ResourcesFile  = "/home/swc/difx/resources.difx";

   public static String  CorrelatedOutputString    = "Correlated output already exist for this job.";
   public static String  DoYouWishToContinueString = "Do you wish to continue?";

   public static final String DATE_TIME_FORMAT = "yyyy-MM-dd HH:mm:ss";
   public static final String TIME_FORMAT      = "HH:mm:ss";
   public static final String DATE_FORMAT      = "yyyy-MM-dd";

   // Global queue variables
   public static long  TimeQueueStarted = System.currentTimeMillis();

   // Max number of sends
   public static int  MaxSends = 10000;
   public static int  MaxSpeed = 100;
}

