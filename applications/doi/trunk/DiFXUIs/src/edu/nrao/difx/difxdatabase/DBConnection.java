/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxdatabase;

import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.logging.Logger;

/**
 * Class to handle database realted statements (open, close, insert, update etc.)
 * @author mguerra (NRAO)
 * @author Helge Rottmann (MPIfR)
 */

public class DBConnection
{
   /** Insert statement. */
   private static final String INSERT_STATEMENT = "INSERT INTO ALERTDATA (HOSTNAME, DEVICENAME, MONPOINTNAME, TIMESTAMP, ALERT, LO_ALERT, HI_ALERT, MONPOINTVALUE) VALUES (?, ?, ?, ?, ?, ?, ?, ?)";

   /** Connection to the database. */
   private Connection mConnection = null;

   /** Time last wrote to database. */
   private long lastWrite = 0L;

   /** The statement to be sent to the database to be executed */
   private PreparedStatement mStatement = null;

   private boolean mConnected = false;

   private Logger mLogger;

   /** The database URL. */
   private String mDbUrl;

   /** The database JDBC path. */
   private String mDbJdbc;

   /** The database user. */
   private String mDbUser;

   /** The database passcode. */
   private String mDbPass;

   /**
    * Constructor. Sets the connection pararmeters and intitalizes the logger.
    * @param url
    * @param jdbc
    * @param user Username
    * @param pass Password
    */
   public DBConnection(String url, String jdbc, String user, String pass)
   {
      mDbUrl   = url;
      mDbJdbc  = jdbc;
      mDbUser  = user;
      mDbPass  = pass;
      initLogger();
   }

   /**
     * Initialize the logging facility. This uses the standard Java 1.4
     * logger, logging records to logs/monarch.xml.
     */
   private void initLogger()
   {
       mLogger = Logger.getLogger("vlba-difx-connection");
       //mLogger = Logger.getLogger(this.getClass());
   }   

   /**
    * Loads the JDBC driver.
    * @throws Exception in case the driver could not be loaded
    */
   public void loadJDBC() throws Exception
   {
      // Load the JDBC driver
      try
      {
         Class.forName(mDbJdbc);
      }
      catch (Exception e)
      {
         String message =
                "Failed to load JDBC driver [ " + e.getMessage() + "]";
         mLogger.severe(message);
         throw e;
      }
   }

   /**
    * Closes the database connection
    */
   public void close()
   {
      try
      {
         if (mConnection != null)
         {
            mConnection.close();
            mStatement.close();
            mStatement = null;
            mLogger.info("Closed database connection...");
         }
      }
      catch (Exception e)
      {
         mLogger.info(e.getMessage());
      }
   }

   /**
    * Makes connection to an Oracle database
    * @throws Exception in case the connection could not be established
    */
   public void connectToDB() throws Exception
   {
      mLogger.info("Connect to database...");
//      mLogger.info("Database URL: " + mDbUrl);
//      mLogger.info("JDBC Driver : " + mDbJdbc);
      mConnection = null;
      mStatement  = null;

      try
      {
         Class driverClass = Class.forName(mDbJdbc);

//         mLogger.info("login timeout = " + DriverManager.getLoginTimeout());
         DriverManager.registerDriver((Driver) driverClass.newInstance());

         mConnection = DriverManager.getConnection(mDbUrl, mDbUser, mDbPass);
         mConnection.setAutoCommit(true);
         mStatement = mConnection.prepareStatement(INSERT_STATEMENT);

         mLogger.info("Connected to database!!");
      }
      catch (ClassNotFoundException e)
      {
         String message =
                "Failed to find database driver [" + e.getMessage() + "]";
         mLogger.severe(message);
         throw e;
      }
      catch (Exception e)
      {
         String message =
                "Failed to connect to database [" + e.getMessage() + "]";
         mLogger.severe(message);
         throw e;
      }

   }

   /**
    * Executes an sql query on the active database connection.
    * @param query the SQL query statement
    * @return the ResultSet holding the query result
    * @throws Exception in case there is no open database connection or the query could not be executed
    */
   public ResultSet selectData(String query) throws Exception
   {
      
      try
      {
         Statement stmt = mConnection.createStatement();
         ResultSet rs = stmt.executeQuery(query);
         return rs;
      }
      catch (Exception e)
      {
         String message =
                "Failed to select data from database [ " + e.getMessage() + "]";
         mLogger.severe(message);
         throw e;
      }
   }

   /**
    * Performs an insert into the database.
    * @param statement the SQL statement used to perform the insert
    * @return the number of records inserted
    * @throws Exception in case there is no open database connection or the statement could not be executed
    */
   public int insertData(String statement) throws Exception
   {
      // Create and execute insert statement into jobsTable
      try
      {
         Statement stmt = mConnection.createStatement();
         int insertCount = stmt.executeUpdate(statement);
         return insertCount;
      }
      catch (Exception e)
      {
         String message =
                "Failed to insert data into database [ " + e.getMessage() + "]";
         mLogger.severe(message);
         throw e;
      }
   }

   /**
    * Performs an update statement on the open database.
    * @param statement the SQL statement used to perform the update
    * @return the number of records updated
    * @throws Exception in case there is no open database connection or the statement could not be executed
    */
   public int updateData(String statement) throws Exception
   {
      // Create and execute update statement into jobsTable
      try
      {
         Statement stmt = mConnection.createStatement();

         // execute the insert statement
         int updateCount = stmt.executeUpdate(statement);

         // updateCount contains the number of updated rows.
         return updateCount;
      }
      catch (Exception e)
      {
         String message =
                "Failed to update data into database [ " + e.getMessage() + "]";
         mLogger.severe(message);
         throw e;
      }
   }

   /**
    * Verify the connection.
    *
    * @return true if connection is okay, otherwise false.
    */
   private boolean isConnectionValid()
   {
      boolean result = false;
      String testQuery = "select systimestamp from dual";
      Statement testStatement = null;

      try
      {
         if (mConnection != null)
         {
            testStatement = mConnection.createStatement();
            testStatement.executeQuery(testQuery);
            result = true;
         }

      }
      catch (SQLException e)
      {
         //connection is not valid
         try
         {
            mConnection.close();
            mConnection = null;
         } 
         catch (Exception ee)
         {
                //quit
         }
         result = false;

      }
      finally
      {
         try
         {
            //free up resource kept by the test statement
            if (testStatement != null)
            {
               testStatement.close();
            }
            testStatement = null;
         }
         catch (Exception e)
         {
            //quit
         }
      }

      return result;
    }

}
