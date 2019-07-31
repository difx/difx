/************************************************************************/
/*									*/
/* Tells the user what happened.  Assesses how good or bad it was,	*/
/* and confers or denies approval via return code.			*/
/*									*/
/*	Inputs:		root_specified		by the user		*/
/*			roots_read		by the program		*/
/*			checked			total # of baselines	*/
/*						checked for fringing	*/
/*						requests		*/
/*			tried			total # of baselines in	*/
/*						the root searched for	*/
/*			successes		# of passes successfully*/
/*						fringe-searched		*/
/*			failures		# of bad fringe searches*/
/*									*/
/*	Output:		return value		0 good, !=0 bad		*/
/*									*/
/* Created 30 December 1993 by CJL					*/
/*									*/
/************************************************************************/


int
report_actions (int roots_specified, int roots_read, int checked, int tried, int successes, int failures)
    {
    char str1[20], str2[20], str3[20];
    extern int test_mode;
					/* Take care of "no data" */
					/* states first */
    
    if (roots_specified == 0)
	{
	msg ("Command line failed to specify any data files!", 3);
	return (1);
	}
    if (roots_read == 0)
	{
	msg ("Failed to successfully read any of the specified", 3);
	msg ("data files", 3);
	return (1);
	}
    if ((successes + failures) == 0)
	{
	msg ("WARNING: None of the specified data passed the", 3);
	msg ("filtering operations.  Check your file list,", 3);
	msg ("control file, and command line options", 3);
	return (0);
	}
					/* OK, we at least attempted */
					/* some fringe searching */
    if (roots_read == roots_specified)
	{
	if (roots_read == 1)
	    msg ("Read the only root file specified on command line", 2);
	else
	    msg ("Read all %d of the root files specified on command line",
			2, roots_read);
	}
    else
	msg ("Read %d of the %d root files specified on command line",
	    2, roots_read, roots_specified);

    if (successes == 0)
	{
	if (failures == 1)
	    msg ("However, in 1 attempt, no successful fringe searches", 2); 
	else
	    msg ("However, in %d attempts, no successful fringe searches", 
		failures, 2);
	msg ("were completed.  This is bad.", 2);
	return (1);
	}
    if (roots_read == 1) sprintf (str1, "this root");
    else sprintf (str1, "these %d roots", roots_read);
    if (checked == 1) sprintf (str2, "1 baseline was");
    else sprintf (str2, "%d baselines were", checked);
    if (tried == 1) sprintf (str3, "1 baseline was");
    else sprintf (str3, "%d baselines were", tried);
    msg ("In %s, %s checked, and %s processed", 2, str1, str2, str3);
    if (successes == 1)
	{
	sprintf (str1, "baseline/subgroup");
	sprintf (str2, "success");
	}
    else 
	{
	sprintf (str1, "baseline/subgroups");
	sprintf (str2, "successes");
	}
    msg ("Fringe searches were attempted on %d %s, with %d %s", 2,
	successes+failures, str1, successes,  str2);
					/* This ratio is subjective */
    if ((failures/successes) > 2)
	{
	msg ("This is an unacceptably high failure rate, that is", 2);
	msg ("treated as an error by fourfit.", 2);
	if (test_mode) msg ("Output suppressed by test mode", 2);
	return (1);
	}

    if (test_mode) msg ("Output suppressed by test mode", 2);
    return (0);
    }
