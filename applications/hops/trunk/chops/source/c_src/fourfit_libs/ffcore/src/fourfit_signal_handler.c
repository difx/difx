/************************************************************************/
/*                                                                      */
/* Removes temporary lock file if it exists and a terminating signal    */
/* is caught                                                            */
/*                                                                      */
/*      Inputs:         integer signal value, global variables of the   */
/*                      lock file name, and its existence indicator     */
/*                      (int) must also be accessible                   */
/*                                                                      */
/*      Output:         non-zero                                        */
/*                                                                      */
/* Created Feb 16 2017 JPB                                              */
/*                                                                      */
/************************************************************************/
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "fourfit_signal_handler.h"
#include "write_lock_mechanism.h"

void
fourfit_signal_handler(int signal_value)
    {
    //removes any existing lock file associated with this process
    if(signal_value == SIGINT)
        {
        remove_lockfile();
        signal(SIGINT, SIG_DFL);
        kill(getpid(), SIGINT);
        }
    if(signal_value == SIGTERM)
        {
        remove_lockfile();
        signal(SIGTERM, SIG_DFL);
        kill(getpid(), SIGTERM);
        }
    if(signal_value == SIGQUIT)
        {
        remove_lockfile();
        signal(SIGQUIT, SIG_DFL);
        kill(getpid(), SIGQUIT);
        }
    if(signal_value == SIGSEGV)
        {
        remove_lockfile();
        signal(SIGSEGV, SIG_DFL);
        kill(getpid(), SIGSEGV);
        }
    if(signal_value == SIGBUS)
        {
        remove_lockfile();
        signal(SIGBUS, SIG_DFL);
        kill(getpid(), SIGBUS);
        }
    if(signal_value == SIGHUP)
        {
        remove_lockfile();
        signal(SIGHUP, SIG_DFL);
        kill(getpid(), SIGHUP);
        }
    if(signal_value == SIGABRT)
        {
        remove_lockfile();
        signal(SIGABRT, SIG_DFL);
        kill(getpid(), SIGABRT);
        }
    }
    
void 
fourfit_register_signal_handler(struct sigaction *handler_action)
    {
    if ( sigaction(SIGTERM, handler_action, NULL) == -1)
        {
        msg ("Could not register SIGTERM", 3);
        exit(1);
        }

    if ( sigaction(SIGINT, handler_action, NULL) == -1)
        {
        msg ("Could not register SIGINT", 3);
        exit(1);
        }
        
    if ( sigaction(SIGQUIT, handler_action, NULL) == -1)
        {
        msg ("Could not register SIGQUIT", 3);
        exit(1);
        }
        
    if ( sigaction(SIGSEGV, handler_action, NULL) == -1)
        {
        msg ("Could not register SIGSEGV", 3);
        exit(1);
        }
        
    if ( sigaction(SIGBUS, handler_action, NULL) == -1)
        {
        msg ("Could not register SIGBUS", 3);
        exit(1);
        }

    if ( sigaction(SIGHUP, handler_action, NULL) == -1)
        {
        msg ("Could not register SIGHUP", 3);
        exit(1);
        }
        
    if ( sigaction(SIGABRT, handler_action, NULL) == -1)
        {
        msg ("Could not register SIGABRT", 3);
        exit(1);
        }  
    }
