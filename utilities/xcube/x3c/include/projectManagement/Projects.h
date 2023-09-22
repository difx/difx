/******************************************************************************
 * Projects.h
 *
 *  Author      : Danni Song
 *  Version     :
 *  Copyright   : (C) Copyright 2011 XCube Research and Development
 *                      All rights reserved.
 *  Description : This class provides user functions to projects, refers to the
 *                access and manipulation of Logger Data Projects related data
 *                on any X3C logger ot DSSC platform.
 *  HISTORY     :
 *          Oct 14, 2011 - initial version
 *
 *****************************************************************************/
#ifndef PROJECTS_H_
#define PROJECTS_H_

#include "xcTypes.h"
#include "CX3cFile.h"
//#include "CMsgHub.h"
#include "xcTaskTools.h"
#include "x3cTaskDefs.h"

#include <string>
#include <vector>
#include <sstream>
using namespace std;

#define MNT_POINT_BASE_NUMBER 0;
#define PROJECTNAME_PREFIX "proj_"  //UTC is appended to this string
#define PROJECT_BASE_PATH "/xcube"
// this is defined in lgrTaskDefs.h
//Identifies the TYPE of task,
//#define TASK_ADDR_LGRMGR              ((TASK_TYPE_LGRMGR << 16)   | 0x00000000)


//! Data structure of stream
/*!
 * defines a single instance of a logger data
 * stream. This is always an .x3c formatted file
 */
typedef struct  _STREAM_BLOCK_ {
    //! path of the parent
    /*! name of the project it belongs to
     * for example, "proj_X3C_LGR030A_2011_10_17_171141"*/
    string strmParent;

    //! path of the stream
    /*! path of the stream on the base mount point,
     * for example, "/mnt/disk0/xcube/proj_X3C_LGR030A_2011_10_17_171141/data/T59_E0_R1_2011_10_17_171150.x3c"*/
    string strmPath;            // Path from project base path

    //! name of the stream
    /*! for example, "T59_E0_R1_2011_10_17_171150.lf1"*/
    string  strmName;

    //! total size of the stream
    /*! sum up all streams on all disks */
    unsigned long long strmSize;

    //! type of the stream
    /*! for example, "59" indicates hrftMgr*/
    unsigned int strmType;

    //! enum of the stream
    /*! for example. "0" */
    unsigned int strmEnum;

    //! run of the stream
    /*! for example. "0" means the first run and "1" means the second run */
    unsigned int strmRun;
}STREAM;

//! Data structure of project
/*!
 * Data structure for a project, It defines a single instance of a project
 */
typedef struct  _PROJECT_BLOCK_
{
    //! name of the project
    /*! string example: proj_LGR00001_2011_10_04_120534*/
    string  prjName;

    //! total size of the project
    /*! total size of recorded data streams, including the streams on all disks*/
    UINT64 prjSize;

    //! project open flag
    /*! TRUE of this project is opened, or FALSE*/
    bool activeFlag;

    //! stream list
    /* a vector containing all stream of this project*/
    vector<STREAM> streamList;
} PROJECT;

//! Projects class
/*!
 * Class of project manager, it provides all functionality to manage projects
 * of the logger.
 */
class Projects
{
 protected:

     //! path of the root of projects,
     /*! for example: "/xcube" completes the path "/mnt/disk#/xcube"*/
    string mRootPath;

    //! path of the base mount point
    /*! for example, "/mnt/disk"*/
    string mMntBasePnt;

    //! number of disks
    /*! total number of disks of the active volumes*/
    UINT32 mDiskCount;

    //! project list
    /*! A vector containing all projects belong to the active volumes*/
    vector<PROJECT> mProjects;

    //! opened project pointer
    /*! points to the opened project if any,
     *  or NULL if there is no project opened*/
    PROJECT* mActiveProject;

    //! current project pointer
    PROJECT* mCurrentProject;

    //! base mount point number
    /*! is initialize to MNT_POINT_BASE_NUMBER, which is 0*/
    static const int MntBasePntNo = MNT_POINT_BASE_NUMBER;


 public:
    //! project created flag
    /*! when a project is created, it will be marked as true,
     * when the project is done, will be marked back as false */
    bool mProjectCreated;

    //! Constructor
    /**
     * Construct an object of project manager
     *
     * @param mntBasePnt        path of the base mount point
     * @param projBasePath      path of the base project
     * @param diskCount         number of disks of the active volume
     * @param mntPtNumAfterBase true if the mount point number should follow the
     *                          projBasePath rather than the mntBasePnt 
     *                             
     */
    Projects(string mntBasePnt = "", string projBasePath = "",
             unsigned diskCount = 0);

    //! De-constructor
    /**
     * De-construct an object of a project manager, clean all fields and memory
     */
    virtual ~ Projects();

    //!Return a list of project on the active volume
    /**
     * First do a projects scan, find out all projects, then return
     * all projects found on the active volume as a project list
     *
     * @return a vector containing all projects
     */
    vector<PROJECT> pmGetProjects();

    //! Delete a project
    /**
     * Delete a project, including all sub directories and data streams,
     * across all disks.
     *
     * @param prjName   - name of the project to delete
     * @return 0            - project deleted
     * @return -1       - project not found
     * @return -2       - forbid to delete current project
     * @return -3       - fail to delete
     */
    int pmDeleteProject(string const& prjName);

    // ! Close project
    /**
     * Close the current project.
     * @return 0            - current project closed
     * @return -1           - fail to close current project
     */
    int pmCloseProject();

    //! Mark a project as the active project.
    /**
     * Only one project may be active at any time.
     * @param prjName       - name of the project to set active
     * @return -1           - there is a project opened
     * @return -2           - project name not found
     * @return 0            - success
     */
    int pmActivateProject(string prjName);

    //! Close an opened project
    /**
     * Close an opened project, if no project opened, return error code
     * @return 0            -success
     * @return -1           - no project opened error
     */
    int pmInactivateProject();

    //! Return the active project currently
    /**
     * Search is there any project opened, if yes, return a pointer to the
     * opened project, or return NULL
     *
     * @return PROJECT*     - pointer to the opened project if any, or NULL
     */
    PROJECT* pmGetActiveProject();

    //! Return all streams belonging to the active project
    /**
     * Search all streams belonging to the opened project and return them
     * @param streamList    - streams belong to the opened project (will be written in)
     *
     * @return 0            - success
     * @return -1           - no project opened
     * @return -2           - fail to open data directory of the project
     */
    int pmGetStreams(vector<STREAM> &streamList);

    //! Indicate whether a project is created and alive now
    /**
     * Boolean function to check whether there is a project created and alive
     * in the system
     *
     * @return true     - there is a project alive now
     * @return false        - no project alive
     */
    bool pmIsProjectAlive() const;

    //! Whether a project is opened
    /**
     * Tell whether there is any project opened there
     * @return true     - there is an opened project
     * @return false    - no project is opened
     */
    bool pmIsProjectActive();

    //! Return a pointer points to the current project
    /**
     * Return the last project descriptor int the project list
     * which is the latest project.
     *
     * @param projectName      - name of current project
     * @return PROJECT*         - pointer to the current project
     */
    PROJECT* pmGetCurrentProject();

    //! Set current project
    /*! @param projectName      -   name of current project
     */
    void pmSetCurrentProject(string projectName);

    //! Return the root path of projects
    /**
     * Get the root path of projects, for example "/xcube"
     *
     * @return string       - path of root of projects.
     */
    string pmGetRootPath() const;

    //! Return the path of the base mount point
    /**
     * For example. "/mnt/disk"
     *
     * @return string       - path of the base mount point
     */
    string pmGetMntBasePnt() const;

    //! Get the number of disks
    /**
     * Return the number of disks detected by the system as usable
     *
     * @return unsigned     - number of disks
     */
    unsigned pmGetDiskCount() const;

    //! Set the number of disks
    /**
     * Set the number of disks
     *
     * @return unsigned     - number of disks
     */
    void pmSetDiskCount(int numOfDisks);

    //! Get the latest project
    /**
     * Return the latest created project
     *
     * @return PROJECT*         - pointer to the latest project or NULL if no project
     */
    PROJECT* pmGetLatestProject();

    /**
     *  Return a collection of stream files for a given STREAM
     * 
     * @param stream The Stream to search for files
     * @param [out] streamFiles a reference to a vector where the stream
     * filenames will be stored.
     *
     * @return 0 if successful, -1 otherwise
     */
    int pmGetStreamFiles(STREAM const& stream, vector<string>& streamFiles);

protected:
    //! Scan all projects on the active volume
    /**
     * Scan all projects created on the active volumes, it will search
     * "/mnt/disk0" for all projects.
     *
     * @return 0            - success
     * @return -1       - fail
     */
    int pmScanProjects();

    // ! Remove a directory
    /**
     * Remove a directory indicated by path, this function is designed for
     * "pmDeleteProject" to delete the project directory on the disks
     *
     * @param path      - path of the directory to remove
     * @return 0            - success
     * @return -1       - fail to delete
     */
    int pmRemoveDirectory(string path);

    //! Whether path is a regular file or directory
    /**
     * Boolean function to check whether path is a regular file or directory
     *
     * @param path      - the path to test
     * @return true     - path is a directory
     * @return false        - path is not a directory
     */
    bool pmIsDirectory(string path);

    //! Return the size of the data file belonging to a project
    /**
     * Get the total size of a project(Go through all disks and sum up
     * the size of data ), report any missing disks
     *
     * @param projName          name of a project
     * @param missingDisks      list of missing disks
     * @return                  total size of the project
     */
    INT64 pmGetProjectDataSize(string const& projName, vector<int> &missingDisks);

    //! Scan all streams of a specific project
    /**
     * Scan all streams of a specific project and fill in the stream list
     * information
     *
     * @param proj      - the project to be scaned
     * @return -1       - project pointer is null
     * @return -2       - fail to open the data directory
     */
    int pmScanStreams(PROJECT &proj);

    //! Parse the name of a stream
    /**
     * Parse the name of stream into stream enum, stream type and stream run
     *
     * @param stream        - the stream to parse
     */
    void pmParseStreamName(STREAM &stream);

    //! search a project based its name
    /**
     * Look for a project in the project list for a project matches the name.
     * Project name is unique.
     *
     * @param prjName       - name of the project to search
     * @return PROJECT*     - NULL if no match or a pointer points to the project found
     */
    PROJECT* pmFindProject(string prjName);

    //! Get stream size
    /**
     * Return total data size of a stream, sum up all data files on all disks
     *
     * @param stream                - the stream to get size
     * @return unsigned long long   - data size
     */
    unsigned long long pmGetStreamSize(STREAM const& stream);

    /**
     * Return total data size of a stream, sum up all data files on all disks
     * A search for all stream files is done starting from the mount point
     *
     * @param stream                - the stream to get size
     * @return unsigned long long   - data size
     */
    unsigned long long pmGetStreamSizeDeepSearch(STREAM const& stream);

    /**
     * Return total data size of all of the streams in a project.
     * A search for the project is done starting from the mount point. Then
     * each project directory is searched for stream files.
     *
     * @param stream                - the stream to get size
     * @return unsigned long long   - data size
     */
    unsigned long long pmGetProjectDataSizeDeepSearch(std::string const& prjName);

    /**
     * Delete a project. The search for the project directories is done
     * starting from the mount point
     *
     * @param prjName A string representing the project name
     * @return 0 if successful, -1 if an error is encountered
     */
    int pmDeleteProjectDeepSearch(std::string const& prjName);
};

#endif /* PROJECTS_H_ */
