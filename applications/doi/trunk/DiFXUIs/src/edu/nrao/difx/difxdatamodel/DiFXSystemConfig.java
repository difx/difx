package edu.nrao.difx.difxdatamodel;

public class DiFXSystemConfig
{
   // System constants
   public static byte NumMark5Units  = 24;
   public static byte NumProcessors  = 10;
   public static byte NumHeadNodes   = 1;

   public static String DATE_TIME_FORMAT = "yyyy-MM-dd HH:mm:ss";
   public static String TIME_FORMAT      = "HH:mm:ss";
   public static String DATE_FORMAT      = "yyyy-MM-dd";

   // Global queue variable
   public static long TimeSystemStarted = System.currentTimeMillis();

}

