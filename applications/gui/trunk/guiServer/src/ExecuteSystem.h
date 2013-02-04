#ifndef GUISERVER_EXECUTESYSTEM_H
#define GUISERVER_EXECUTESYSTEM_H
//=============================================================================
//
//   guiServer::ExecuteSystem Class
//
//!  This class is used to execute a single system call.  It provides functions
//!  for reading stdout and stderr data from the system call separately.
//
//=============================================================================
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <string.h>

namespace guiServer {

    class ExecuteSystem {
    
    public:
    
        static const int MAX_BUFFER_LENGTH = 1024 * 1024;
    
        //---------------------------------------------------------------------
        //!  The constructor is called with a command string that needs to be
        //!  executed.  The command is exectuted (using the bash shell) and
        //!  file descriptors are created to gather its output.
        //---------------------------------------------------------------------
        ExecuteSystem( char* command ) {
            _commandList[0] = "/bin/bash";
            _commandList[1] = "-c";
            _commandList[2] = command;
            _commandList[3] = NULL;
            _pid = popenRWE( _pipe, "/bin/bash", _commandList );
            _outOpen = true;
            _errOpen = true;
            _outN = 0;
            _errN = 0;
            _noErrors = true;
        }
        
        ~ExecuteSystem() {
            pcloseRWE( _pid, _pipe );
        }
        
        //--------------------------------------------------------------------
        //!  Do a select on both outputs (stdout and stderr), returning the
        //!  first that responds with a complete line of something (1 for 
        //!  stdout, 2 for stderr). If both are "closed" (i.e both have 
        //!  reached the end-of-file marks) return 0.
        //!
        //!  The buffer for the output line must be supplied, along with a
        //!  maximum length (the full line of output will still be read - it
        //!  will just be truncated to fit.  In all cases the returned string
        //!  will be NULL terminated.
        //--------------------------------------------------------------------
        int nextOutput( char* buffer, int maxLen ) {
            int fds;
            while ( _outOpen || _errOpen ) {
                FD_ZERO( &_rfds );
                if ( _outOpen ) {
                    FD_SET( _pipe[1], &_rfds );
                    fds = _pipe[1];
                }
                if ( _errOpen ) {
                    FD_SET( _pipe[2], &_rfds );
                    if ( _pipe[2] > fds )
                    fds = _pipe[2];
                }
                ++fds;
                if ( select( fds, &_rfds, NULL, NULL, NULL ) > -1 ) {
                    if ( FD_ISSET( _pipe[1], &_rfds ) ) {
                        //  select returned stdout....
                        //  Read data until we run out or reach a newline character.
                        //  This is read character by character, which might be a bit
                        //  slow.  Also, make sure we are able to read SOMETHING - if
                        //  not, this indicates an end of file.
                        bool somethingRead = false;
                        while ( read( _pipe[1], _outBuffer + _outN, 1 ) > 0 ) {
                            somethingRead = true;
                            if ( _outBuffer[_outN] == '\n' ) {
                                //  Reached the end of a line of output - copy to the user's
                                //  buffer, make sure its NULL terminated, and return.
                                _outBuffer[_outN] = 0;
                                strncpy( buffer, _outBuffer, maxLen );
                                buffer[maxLen-1] = 0;
                                _outN = 0;
                                return 1;
                            }
                            ++_outN;
                        }
                        if ( !somethingRead )
                            _outOpen = false;
                    }
                    if ( FD_ISSET( _pipe[2], &_rfds ) ) {
                        //  select returned stderr...
                        //  See above for stdout - logic is identical.
                        bool somethingRead = false;
                        while ( read( _pipe[2], _errBuffer + _errN, 1 ) > 0 ) {
                            somethingRead = true;
                            if ( _errBuffer[_errN] == '\n' ) {
                                //  Reached the end of a line of output - copy to the user's
                                //  buffer, make sure its NULL terminated, and return.
                                _errBuffer[_errN] = 0;
                                strncpy( buffer, _errBuffer, maxLen );
                                buffer[maxLen-1] = 0;
                                _errN = 0;
                                _noErrors = false;
                                return 2;
                            }
                            ++_errN;
                        }
                        if ( !somethingRead )
                            _errOpen = false;
                    }
                }
                else {
                    // broken pipe or something
                    _outOpen = false;
                    _errOpen = false;
                }
            }
            return 0;
        }
        
        bool noErrors() { return _noErrors; }
        int pid() { return _pid; }

    protected:
    
        int _pipe[3];
        int _pid;
        char* _commandList[4];
        bool _outOpen;
        bool _errOpen;
        fd_set _rfds;
        char _outBuffer[MAX_BUFFER_LENGTH];
        char _errBuffer[MAX_BUFFER_LENGTH];
        int _outN;
        int _errN;
        bool _noErrors;

        //-----------------------------------------------------------------------------
        //  The two following functions borrowed essentially unchanged...citation below.
        /*
         * Copyright 2009-2010 Bart Trojanowski <bart@jukie.net>
         * Licensed under GPLv2, or later, at your choosing.
         *
         * bidirectional popen() call
         *
         * @param rwepipe - int array of size three
         * @param exe - program to run
         * @param argv - argument list
         * @return pid or -1 on error
         *
         * The caller passes in an array of three integers (rwepipe), on successful
         * execution it can then write to element 0 (stdin of exe), and read from
         * element 1 (stdout) and 2 (stderr).
         */
        int popenRWE(int *rwepipe, const char *exe, const char *const argv[])
        {
	        int in[2];
	        int out[2];
	        int err[2];
	        int pid;
	        int rc;

	        rc = pipe(in);
	        if (rc<0)
		        goto error_in;

	        rc = pipe(out);
	        if (rc<0)
		        goto error_out;

	        rc = pipe(err);
	        if (rc<0)
		        goto error_err;

	        pid = fork();
	        if (pid > 0) { // parent
		        close(in[0]);
		        close(out[1]);
		        close(err[1]);
		        rwepipe[0] = in[1];
		        rwepipe[1] = out[0];
		        rwepipe[2] = err[0];
		        return pid;
	        } else if (pid == 0) { // child
		        close(in[1]);
		        close(out[0]);
		        close(err[0]);
		        close(0);
		        dup(in[0]);
		        close(1);
		        dup(out[1]);
		        close(2);
		        dup(err[1]);

		        execvp(exe, (char**)argv);
		        exit(1);
	        } else
		        goto error_fork;

	        return pid;

        error_fork:
	        close(err[0]);
	        close(err[1]);
        error_err:
	        close(out[0]);
	        close(out[1]);
        error_out:
	        close(in[0]);
	        close(in[1]);
        error_in:
	        return -1;
        }

        int pcloseRWE(int pid, int *rwepipe)
        {
	        int rc, status;
	        close(rwepipe[0]);
	        close(rwepipe[1]);
	        close(rwepipe[2]);
	        rc = waitpid(pid, &status, 0);
	        return status;
        }
        
    };

}

#endif

