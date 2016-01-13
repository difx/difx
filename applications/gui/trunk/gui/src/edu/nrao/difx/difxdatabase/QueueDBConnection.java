/***************************************************************************
 *   Copyright (C) 2016 by John Spitzak                                    *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
/*
 * Use the generic DBConnection class in convenience functions
 * for interacting with the DiFX queue data base.
 * 
 * Exceptions are intercepted here but generally ignored as the parent class
 * produces messages associated with them.
 */
package edu.nrao.difx.difxdatabase;

import edu.nrao.difx.difxview.SystemSettings;

import java.sql.ResultSet;
import java.sql.DriverManager;
import java.sql.Connection;
import java.sql.Statement;
import java.sql.Driver;
import java.sql.PreparedStatement;

/**
 *
 * @author jspitzak
 */
public class QueueDBConnection {
    
    public QueueDBConnection( SystemSettings settings ) {
        _settings = settings;
        //  Don't connect to the database if the user is not using it!
        if ( !_settings.useDatabase() ) {
            _dbConnection = null;
            return;
        }
        try {
            Class driverClass = Class.forName( _settings.dbDriver() );

            DriverManager.registerDriver( (Driver)driverClass.newInstance() );

            _dbConnection = DriverManager.getConnection( _settings.dbURL(), _settings.dbUser(), _settings.dbPwd() );
            _dbConnection.setAutoCommit( true );

        } catch ( java.sql.SQLException e ) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "SQL Exception during connection to database [" + e.getMessage() + "]" );
        } catch (ClassNotFoundException e) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to find database driver [" + e.getMessage() + "]" );
        } catch (Exception e) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to connect to database [" + e.getMessage() + "]" );
        }
    }
    
   /**
    * Closes the database connection
    */
    public void close() {
        try {
            if (_dbConnection != null) {
                _dbConnection.close();
            }
        } catch ( Exception e ) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    e.getMessage() );
        }
    }

    /*
     * Return whether we are actually connected to the database.
     */
    public boolean connected() {
        return _dbConnection != null;
    }
    
    /*
     * Generate a list of all experiments in the data base.
     */
    public ResultSet experimentList() {
        try {
            Statement stmt = _dbConnection.createStatement( ResultSet.TYPE_SCROLL_INSENSITIVE, 
                    ResultSet.CONCUR_UPDATABLE );
            return stmt.executeQuery( "select * from Experiment" );
        } catch (Exception e) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to select data from Experiment database [ " + e.getMessage() + "]" );
            return null;
        }
    }
    
    /*
     * Generate a list of all passes in the data base.
     */
    public ResultSet passList() {
        try {
            Statement stmt = _dbConnection.createStatement( ResultSet.TYPE_SCROLL_INSENSITIVE, 
                    ResultSet.CONCUR_UPDATABLE );
            return stmt.executeQuery( "select * from Pass" );
        } catch (Exception e) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to select data from Pass database [ " + e.getMessage() + "]" );
            return null;
        }
    }
    
    /*
     * Generate a list of all jobs in the data base.
     */
    public ResultSet jobList() {
        try {
            Statement stmt = _dbConnection.createStatement( ResultSet.TYPE_SCROLL_INSENSITIVE, 
                    ResultSet.CONCUR_UPDATABLE );
            return stmt.executeQuery( "select * from Job" );
        } catch (Exception e) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to select data from Job database [ " + e.getMessage() + "]" );
            return null;
        }
    }
    
    /*
     * Generate a list of all pass types in the data base.
     */
    public ResultSet passTypeList() {
        try {
            Statement stmt = _dbConnection.createStatement( ResultSet.TYPE_SCROLL_INSENSITIVE, 
                    ResultSet.CONCUR_UPDATABLE );
            return stmt.executeQuery( "select * from PassType" );
        } catch (Exception e) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to select data from PassType database [ " + e.getMessage() + "]" );
            return null;
        }
    }
    
    /*
     * Generate a list of all job status types in the data base.
     */
    public ResultSet jobStatusList() {
        try {
            Statement stmt = _dbConnection.createStatement( ResultSet.TYPE_SCROLL_INSENSITIVE, 
                    ResultSet.CONCUR_UPDATABLE );
            return stmt.executeQuery( "select * from JobStatus" );
        } catch (Exception e) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to select data from JobStatus database [ " + e.getMessage() + "]" );
            return null;
        }
    }
    
    public ResultSet slotList() {
        try {
            Statement stmt = _dbConnection.createStatement( ResultSet.TYPE_SCROLL_INSENSITIVE, 
                    ResultSet.CONCUR_UPDATABLE );
            return stmt.executeQuery( "select * from Slot" );
        } catch (Exception e) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to select data from Slot database [ " + e.getMessage() + "]" );
            return null;
        }
    }
    
    public ResultSet moduleList() {
        try {
            Statement stmt = _dbConnection.createStatement( ResultSet.TYPE_SCROLL_INSENSITIVE, 
                    ResultSet.CONCUR_UPDATABLE );
            return stmt.executeQuery( "select * from Module" );
        } catch (Exception e) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to select data from Module database [ " + e.getMessage() + "]" );
            return null;
        }
    }
    
    public ResultSet experimentAndModuleList() {
        try {
            Statement stmt = _dbConnection.createStatement( ResultSet.TYPE_SCROLL_INSENSITIVE, 
                    ResultSet.CONCUR_UPDATABLE );
            return stmt.executeQuery( "select * from ExperimentAndModule" );
        } catch (Exception e) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to select data from ExperimentAndModule database [ " + e.getMessage() + "]" );
            return null;
        }
    }
    
    public ResultSet experimentStatusList() {
        try {
            Statement stmt = _dbConnection.createStatement( ResultSet.TYPE_SCROLL_INSENSITIVE, 
                    ResultSet.CONCUR_UPDATABLE );
            return stmt.executeQuery( "select * from ExperimentStatus" );
        } catch (Exception e) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to select data from ExperimentStatus database [ " + e.getMessage() + "]" );
            return null;
        }
    }
    
    public ResultSet versionHistoryList() {
        try {
            Statement stmt = _dbConnection.createStatement( ResultSet.TYPE_SCROLL_INSENSITIVE, 
                    ResultSet.CONCUR_UPDATABLE );
            return stmt.executeQuery( "select * from VersionHistory" );
        } catch (Exception e) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to select data from VersionHistory database [ " + e.getMessage() + "]" );
            return null;
        }
    }
    
    /*
     * Generate a list of all jobs in the data base in the given Pass.
     */
    public ResultSet jobListByPassId( int passId ) {
        try {
            PreparedStatement stmt = _dbConnection.prepareStatement( 
                    "SELECT * FROM Job where passID = ?",
                    ResultSet.TYPE_SCROLL_INSENSITIVE, 
                    ResultSet.CONCUR_UPDATABLE );
            stmt.setLong( 1, passId );
            return stmt.executeQuery();
        } catch (Exception e) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to select data from database [ " + e.getMessage() + "]" );
            return null;
        }
    }
    
    /*
     * Create a new experiment.  The data base will automatically assign a unique
     * ID number, but we give it name, number (used to be called "segment"), 
     * initial status, directory, and vex filename.  Return whether this operation 
     * was successful or not.
     */
    public boolean newExperiment( String name, Integer number, Integer statusId, String directory, String vexFileName ) {
        if ( !this.connected() )
            return false;
        try {
            PreparedStatement stmt = _dbConnection.prepareStatement( 
                    "INSERT INTO Experiment (code, number, statusID, directory, vexfile) VALUES(?, ?, ?, ?, ?)",
                    ResultSet.TYPE_SCROLL_INSENSITIVE, 
                    ResultSet.CONCUR_UPDATABLE );
            stmt.setString( 1, name );
            stmt.setLong( 2, number );
            stmt.setLong( 3, statusId );
            stmt.setString( 4, directory );
            stmt.setString( 5, vexFileName );
            if ( stmt.executeUpdate() > 0 )
                return true;
            else
                return false;
        } catch (Exception e) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to insert new experiment \"" + name + "\" in Experiment database [ " + e.getMessage() + "]" );
            return false;
        }
    }
    
    /*
     * Create a new pass.  The pass is given a name, type, and experiment ID, and
     * (hopefully soon) a directory and vex file name.
     */
    public boolean newPass( String name, Integer typeId, Integer experimentId ) {
        if ( !this.connected() )
            return false;
        try {
            PreparedStatement stmt = _dbConnection.prepareStatement( 
                    "INSERT INTO Pass (experimentID, passName, passTypeID) VALUES(?, ?, ?)",
                    ResultSet.TYPE_SCROLL_INSENSITIVE, 
                    ResultSet.CONCUR_UPDATABLE );
            stmt.setLong( 1, experimentId );
            stmt.setString( 2, name );
            stmt.setLong( 3, typeId );
            if ( stmt.executeUpdate() > 0 )
                return true;
            else
                return false;
        } catch (Exception e) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to insert new pass \"" + name + "\" in Pass database [" + e.getMessage() + "]" );
            return false;
        }
    }
    
    /*
     * Create a new job.
     */
    public boolean newJob( String name, Integer passId, Integer jobNumber, Double jobStart,
            Double jobDuration, String inputFile, String difxVersion, Integer numAntennas,
            Integer numForeign, Integer statusId ) {
        if ( !this.connected() )
            return false;
        try {
            PreparedStatement stmt = _dbConnection.prepareStatement( 
                    "INSERT INTO Job (passID, jobNumber, jobStart, jobDuration, inputFile, difxVersion, numAntennas, numForeign, statusID) VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?)",
                    ResultSet.TYPE_SCROLL_INSENSITIVE, 
                    ResultSet.CONCUR_UPDATABLE );
            stmt.setLong( 1, passId );
            stmt.setLong( 2, jobNumber );
            stmt.setDouble( 3, jobStart );
            stmt.setDouble( 4, jobDuration );
            stmt.setString( 5, inputFile );
            stmt.setString( 6, difxVersion );
            stmt.setLong( 7, numAntennas );
            stmt.setLong( 8, numForeign );
            stmt.setLong( 9, statusId );
            if ( stmt.executeUpdate() > 0 )
                return true;
            else
                return false;
        } catch (Exception e) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to insert new Job \"" + name + "\" in Job database [" + e.getMessage() + "]" );
            return false;
        }
    }
    
    /*
     * Delete the given experiment from the database.  The experiment is identified
     * by its unique ID.
     */
    public void deleteExperiment( Integer id ) {
        //  We can't handle null ID's.
        if ( id == null )
            return;
        try {
            PreparedStatement stmt = _dbConnection.prepareStatement(
                    "DELETE FROM Experiment WHERE id = ?",
                    ResultSet.TYPE_SCROLL_INSENSITIVE, 
                    ResultSet.CONCUR_UPDATABLE );
            stmt.setLong( 1, id );
            stmt.executeUpdate();
        } catch ( Exception e ) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to delete Experiment " + id.toString() + " from Experiment database [" + e.getMessage() + "]" );
        }
    }
    
    /*
     * Delete the given pass from the database.  The pass is identified
     * by its unique ID.
     */
    public void deletePass( Integer id ) {
        //  We can't handle null ID's.
        if ( id == null )
            return;
        try {
            PreparedStatement stmt = _dbConnection.prepareStatement(
                    "DELETE FROM Pass WHERE id = ?",
                    ResultSet.TYPE_SCROLL_INSENSITIVE, 
                    ResultSet.CONCUR_UPDATABLE );
            stmt.setLong( 1, id );
            stmt.executeUpdate();
        } catch ( Exception e ) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to delete Pass " + id.toString() + " from Pass database [" + e.getMessage() + "]" );
        }
    }
    
    /*
     * Delete the given job from the database.  The job is identified
     * by its unique ID.
     */
    public void deleteJob( Integer id ) {
        //  We can't handle null ID's.
        if ( id == null )
            return;
        try {
            PreparedStatement stmt = _dbConnection.prepareStatement(
                    "DELETE FROM Job WHERE id = ?",
                    ResultSet.TYPE_SCROLL_INSENSITIVE, 
                    ResultSet.CONCUR_UPDATABLE );
            stmt.setLong( 1, id );
            stmt.executeUpdate();
        } catch ( Exception e ) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to delete Job " + id.toString() + " from Job database [" + e.getMessage() + "]" );
        }
    }
    
    /*
     * Update an element of a specified experiment (identified by ID).  It will change
     * a specific field to a specific value - both are strings.  Return the number of items
     * updated.
     */
    public int updateExperiment( Integer id, String param, String setting ) {
        if ( id == null )
            return 0;
        try {
            PreparedStatement stmt = _dbConnection.prepareStatement(
                    "UPDATE Experiment SET " + param + " = ? WHERE id = ?",
                    ResultSet.TYPE_SCROLL_INSENSITIVE, 
                    ResultSet.CONCUR_UPDATABLE );
            stmt.setString( 1, setting );
            stmt.setLong( 2, id );
            return stmt.executeUpdate();
        } catch ( Exception e ) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to update Experiment " + id.toString() + " with " + param + " = " + setting + " [" + e.getMessage() + "]" );
            return 0;
        }
    }
    
    /*
     * Update an element of a specified pass (identified by ID).  It will change
     * a specific field to a specific value - both are strings.  Return the number of items
     * updated.
     */
    public int updatePass( Integer id, String param, String setting ) {
        if ( id == null )
            return 0;
        try {
            PreparedStatement stmt = _dbConnection.prepareStatement(
                    "UPDATE Pass SET " + param + " = ? WHERE id = ?",
                    ResultSet.TYPE_SCROLL_INSENSITIVE, 
                    ResultSet.CONCUR_UPDATABLE );
            stmt.setString( 1, setting );
            stmt.setLong( 2, id );
            return stmt.executeUpdate();
        } catch ( Exception e ) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE,
                    "Failed to update Pass " + id.toString() + " with " + param + " = " + setting + " [" + e.getMessage() + "]" );
            return 0;
        }
    }
    
    /*
     * Update an element of a specified job (identified by ID).  It will change
     * a specific field to a specific value - both are strings.  Return the number of items
     * updated.
     */
    public int updateJob( Integer id, String param, String setting ) {
        if ( id == null )
            return 0;
        try {
            PreparedStatement stmt = _dbConnection.prepareStatement(
                    "UPDATE Job SET " + param + " = ? WHERE id = ?",
                    ResultSet.TYPE_SCROLL_INSENSITIVE, 
                    ResultSet.CONCUR_UPDATABLE );
            stmt.setString( 1, setting );
            stmt.setLong( 2, id );
            return stmt.executeUpdate();
        } catch ( Exception e ) {
            java.util.logging.Logger.getLogger( "global" ).log( java.util.logging.Level.SEVERE, 
                    e.getMessage() );
            return 0;
        }
    }
    
    protected Connection _dbConnection;
    protected SystemSettings _settings;
    
}
