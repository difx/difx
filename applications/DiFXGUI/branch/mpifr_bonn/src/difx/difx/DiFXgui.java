package difx;

import java.awt.GraphicsConfiguration;
import java.awt.HeadlessException;
import java.awt.event.*;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.filechooser.FileFilter;
import java.awt.BorderLayout;
import javax.swing.BorderFactory;
import java.awt.Color;

public class DiFXgui
    extends JFrame implements  ChangeListener
{

    private static final String INIFILE = "COMMANDS.CONFIG";

    private JTabbedPane displayarea = new JTabbedPane();
    private DiFXPanel lastselectedpanel;
    private PulsarPanel pulsarpanel;
    private CorrSummaryPanel corrsummarypanel;
    private EditCommonPanel editcommonpanel;
    private EditConfigPanel editconfigpanel;
    private EditDatastreamPanel editdatastreampanel;
    private EditBaselinePanel editbaselinepanel;
    private EditNodePanel editnodepanel;
    private EditFreqPanel editfreqpanel;
    private JMenuBar mainmenubar;
    private JMenu pulsarmenu, mainmenu, modelmenu, advancedmenu;
    private JFileChooser vexchooser, corrconfigchooser;
    private FileFilter vexfilter, corrconfigfilter;
    private String queuegatewaymachine;
    private String corr_root = null;

    private static String mpifxcorrcommand = null;
    private static String vex2configcommand, vex2modelcommand, getnodescommand,
    getpbsnodescommand, getqueuescommand, vex2modetimelistcommand;
    private CorrelationConfig corrconfig;

    JMenuItem mitExit = new JMenuItem();
    JMenuItem mitOpen = new JMenuItem();
    JMenuItem mitSave = new JMenuItem();
    JMenuItem mitSaveAs = new JMenuItem();
    JMenuItem mitNew = new JMenuItem();
    JMenuItem mitNewFromVex = new JMenuItem();
    JMenuItem mitCalcFromVex = new JMenuItem();
    JMenuItem mitCalcManual = new JMenuItem();
    JMenuItem mitAddCalcStation = new JMenuItem();
    JMenuItem mitOpenBinconfig = new JMenuItem();
    JMenuItem mitOpenCube = new JMenuItem();
    JMenuItem mitSaveBinconfig = new JMenuItem();
    JMenuItem mitTrimMatchConfig = new JMenuItem();
    JMenuItem mitAddCalcSource = new JMenuItem();
    JMenuItem mitTrimMatchTimerange = new JMenuItem();
    JMenuItem mitTrimMatchMode = new JMenuItem();
    BorderLayout borderLayout1 = new BorderLayout();

    public DiFXgui() throws HeadlessException
    {
        super("DiFX GUI");

        try
        {
            //create the correlation config object
            corrconfig = new CorrelationConfig(this);

            //read in the commands from the default setup file
            readsetup();

            //create the filechoosers
            vexchooser = new JFileChooser();
            vexfilter = new FileFilter()
            {
                public boolean accept(File f)
                {
                    return f.getName().endsWith(".skd") ||
                        f.getName().endsWith("vex") || f.isDirectory();
                }

                public String getDescription()
                {
                    return "VEX configuration files";
                }
            };
            vexchooser.addChoosableFileFilter(vexfilter);
            vexchooser.setFileFilter(vexfilter);
            corrconfigchooser = new JFileChooser();
            corrconfigfilter = new FileFilter()
            {
                public boolean accept(File f)
                {
                    return f.getName().endsWith(".input") || f.isDirectory();
                }

                public String getDescription()
                {
                    return "Correlation configuration files";
                }
            };
            corrconfigchooser.addChoosableFileFilter(corrconfigfilter);
            corrconfigchooser.setFileFilter(corrconfigfilter);

            jbInit();

            // disable controls until new/open operation by the user
            setGuiInputState(false);
        }
        catch (Exception ex)
        {
            ex.printStackTrace();
        }
    }

    public static String getGetNodesCommand()
    {
        return getnodescommand;
    }

    public static String getGetPBSNodesCommand()
    {
        return getpbsnodescommand;
    }

    public static String getGetQueuesCommand()
    {
        return getqueuescommand;
    }

    public void launchcorrelation()
    {
        Runtime rt;
        Process launchprocess = null;
        PrintWriter out;
        BufferedReader stdoutreader, stderrreader;
        String clustername, line;
        String[] launchcommand = null;
        int walltimehours = corrconfig.getWalltime();
        int procspernode = corrconfig.getProcsPerNode();
        String cwd = System.getProperty("user.dir") + "/";
        int returnval;

        //check if changes need to be committed to disk - if so, do it
        if (!corrconfig.isUpdated())
        {
            int choice = JOptionPane.showConfirmDialog(this,
                "Save changes to current file before launch?",
                "Save changes", JOptionPane.YES_NO_CANCEL_OPTION,
                JOptionPane.QUESTION_MESSAGE);
            if (choice == JOptionPane.CANCEL_OPTION)
                return;
            if (choice == JOptionPane.YES_OPTION)
                corrconfig.saveCorrConfigFile();
        }

        try
        {
            //do a consistency check
            corrconfig.consistencyCheck(true);

            rt = Runtime.getRuntime();

            //check if its a pbs job, and launch appropriately
            if (corrconfig.usePbs())
            {
                //create the little script to be run, and run it
                out = new PrintWriter(new FileWriter("pbs.script", false));
                out.println("#PBS -S /bin/csh");
                out.println("#PBS -l nodes=" + corrconfig.getNumNodes() +
                            ":ppn=" + procspernode + ",walltime=" +
                            walltimehours +
                            ":00:00");
                out.println("@ ppn=" + procspernode);
                out.println();
                out.println("set activedir=" + cwd);
                out.println("set machinefilename=$activedir/machines.list");
                out.println();
                out.println("set nmax=`wc -l $PBS_NODEFILE`");
                out.println("set nmax=$nmax[1]");
                out.println("rm -f $machinefilename");
                out.println("@ at=1");
                out.println("@ i=0");
                out.println("while ($i < $ppn)");
                out.println("        @ at= 1 + $i");
                out.println("        while ($at <= $nmax)");
                out.println(
                    "               set NODENAME=`sed -n -e \"$at p\" $PBS_NODEFILE`");
                out.println("               echo $NODENAME : mpifxcorr " +
                            corrconfig.getCurrentFilename() +
                            " >> $machinefilename");
                out.println("               @ at = $at + $ppn");
                out.println("        end");
                out.println("        @ i++");
                out.println("end");
                out.println("");
                out.println("mpiexec -config $machinefilename");
                out.close();

                out = new PrintWriter(new FileWriter("launchme", false));
                clustername = corrconfig.getClusterName();
                launchcommand = new String[]
                    {
                    "ssh", queuegatewaymachine, "-f", cwd + "/launchme"};

                /*if(clustername.startsWith("DEFAULT"))
                 launchcommand = new String [] {"ssh", queuegatewaymachine, "-f", "\"qsub -j oe -o " +
                 corrconfig.getCurrentFilename() + ".log " + cwd + "/pbs.script\""};
                     else
                 launchcommand = new String [] {"ssh", queuegatewaymachine, "-f", "\"qsub -q " + clustername +
                   " -j oe -o " + corrconfig.getCurrentFilename() + ".log " + cwd + "/pbs.script\""};*/
                if (clustername.startsWith("DEFAULT"))
                    out.println("qsub -j oe -o " +
                                corrconfig.getCurrentFilename() + ".log " + cwd +
                                "/pbs.script");
                else
                    out.println("qsub -q " + clustername + " -j oe -o " +
                                corrconfig.getCurrentFilename() +
                                ".log " + cwd + "/pbs.script");
                out.close();
                rt.exec("chmod 775 launchme");
            }
            else
            {
                //go ahead and run
                launchcommand = new String[]
                    {
                    "mpirun", "-np", corrconfig.getNumNodes() + "",
                    "-machinefile",
                    corrconfig.getMachineFilename(), "-nolocal",
                    mpifxcorrcommand,
                    corrconfig.getCurrentFilename(), ">&!", "mpifxoutput.txt",
                    "&"};
            }
            System.out.print("About to run: ");
            for (int i = 0; i < launchcommand.length; i++)
                System.out.print(launchcommand[i] + " ");
            launchprocess = rt.exec(launchcommand);
            if (corrconfig.usePbs())
            {
                stdoutreader = new BufferedReader(new InputStreamReader(
                    launchprocess.getInputStream()));
                stderrreader = new BufferedReader(new InputStreamReader(
                    launchprocess.getErrorStream()));
                while ( (line = stdoutreader.readLine()) != null)
                    System.out.println(line);
                if ( (line = stderrreader.readLine()) != null)
                {
                    JOptionPane.showMessageDialog(this,
                                                  "Error executing " +
                                                  launchcommand +
                                                  ": " + line, "Error!!!",
                                                  JOptionPane.ERROR_MESSAGE);
                    return; //abort, couldn't load the file
                }
                returnval = launchprocess.waitFor();
            }
        }
        catch (ConsistencyException e)
        {
            JOptionPane.showMessageDialog(this,
                                          "Inconsistency: " + e.getMessage(),
                                          "Configuration inconsistency",
                                          JOptionPane.ERROR_MESSAGE);
        }
        catch (IOException e)
        {
            JOptionPane.showMessageDialog(this,
                                          "Runtime error: " + e.getMessage(),
                                          "Launch problem!!!",
                                          JOptionPane.ERROR_MESSAGE);
        }
        catch (InterruptedException e)
        {
            JOptionPane.showMessageDialog(this,
                                          "Runtime error: " + e.getMessage(),
                                          "Launch problem!!!",
                                          JOptionPane.ERROR_MESSAGE);
        }
    }


    public void stateChanged(ChangeEvent e)
    {
        String confirmmessage = "The last active panel had unsaved changes - would you like to commit them to the current correlation configuration?  Cancel to return to editing last active panel.";
        int commitchanges;
        boolean cancel = false;
        Object source = e.getSource();
        DiFXPanel activepanel;

        if (source == displayarea)
        {
            if (displayarea.getSelectedComponent() == lastselectedpanel)
                return; //take care of the event that results when we switch back after a cancel
            if (!lastselectedpanel.isUpdated())
            {
                commitchanges = JOptionPane.showConfirmDialog(this,
                    confirmmessage, "Save changes?",
                    JOptionPane.YES_NO_CANCEL_OPTION,
                    JOptionPane.QUESTION_MESSAGE);
                switch (commitchanges)
                {
                    case JOptionPane.YES_OPTION:
                        lastselectedpanel.commitcorrchanges();
                    case JOptionPane.NO_OPTION:
                        break;
                    case JOptionPane.CANCEL_OPTION:
                        cancel = true;
                        break;
                }
            }

            if (cancel)
            {
                displayarea.setSelectedComponent(lastselectedpanel);
            }
            else
            {
                activepanel = (DiFXPanel) displayarea.getSelectedComponent();
                activepanel.refreshdisplay();
                lastselectedpanel = activepanel;
            }
        }
    }

    private void loadCorrConfigFile()
    {
        int returnval;

        // open a jfiledialog and pick the file
        returnval = corrconfigchooser.showOpenDialog(this);
        if (returnval == JFileChooser.APPROVE_OPTION)
        {
            corrconfig.loadCorrConfigFile(corrconfigchooser.getSelectedFile().
                                          getAbsolutePath());
            ( (DiFXPanel) displayarea.getSelectedComponent()).refreshdisplay();
        }
    }

    private int saveCorrConfigFileAs()
    {
        int returnval;

        // open a jfiledialog and pick the file
        returnval = corrconfigchooser.showSaveDialog(this);
        if (returnval == JFileChooser.APPROVE_OPTION)
        {
            corrconfig.saveCorrConfigFile(corrconfigchooser.getSelectedFile().
                                          getAbsolutePath());
        }
        return returnval;
    }

    private void createConfigFile(boolean fromvex)
    {
        Runtime rt;
        Process vex2config;
        PrintWriter redirected;
        BufferedReader stdoutreader, stderrreader;
        String line, vexpath = "", corrconfigpath = "";
        int returnval;

        //get the filename(s)
        if (fromvex)
        {
            returnval = vexchooser.showOpenDialog(this);
            if (returnval == JFileChooser.APPROVE_OPTION)
                vexpath = vexchooser.getSelectedFile().getAbsolutePath();
            else
                return; //bail out since they didn't choose a file
        }
        returnval = corrconfigchooser.showSaveDialog(this);
        if (returnval == JFileChooser.APPROVE_OPTION)
            corrconfigpath = corrconfigchooser.getSelectedFile().
                getAbsolutePath();
        else
            return; //bail out since they didn't choose a file

        //if we are creating from vex, run vex2config and tell corrconfig to load from that
        if (fromvex)
        {
            rt = Runtime.getRuntime();
            try
            {
                redirected = new PrintWriter(new FileOutputStream(
                    corrconfigpath));
                vex2config = rt.exec(vex2configcommand + " " + vexpath + " " +
                                     vexchooser.getCurrentDirectory());
                stdoutreader = new BufferedReader(new InputStreamReader(
                    vex2config.getInputStream()));
                stderrreader = new BufferedReader(new InputStreamReader(
                    vex2config.getErrorStream()));
                while ( (line = stdoutreader.readLine()) != null)
                    redirected.println(line);
                if ( (line = stderrreader.readLine()) != null)
                {
                    JOptionPane.showMessageDialog(this,
                                                  "Error executing " +
                                                  vex2configcommand +
                                                  ": " + line, "Error!!!",
                                                  JOptionPane.ERROR_MESSAGE);
                    return; //abort, couldn't load the file
                }
                redirected.flush();
                returnval = vex2config.waitFor();
            }
            catch (Exception e)
            {
                JOptionPane.showMessageDialog(this,
                                              "Error executing " +
                                              vex2configcommand +
                                              ": " + e.getMessage(), "Error!!!",
                                              JOptionPane.ERROR_MESSAGE);
            }
            corrconfig.loadCorrConfigFile(corrconfigpath);
        }
        else
        {
            //otherwise just tell corrconfig to reset
            corrconfig.reset();
            corrconfig.setConfigFilename(corrconfigpath);
        }
        ( (DiFXPanel) displayarea.getSelectedComponent()).refreshdisplay();
    }

    private void createModelFiles(boolean fromvex)
    {
        int returnval;
        String vexpath, line;
        Runtime rt;
        Process vex2model;
        BufferedReader stdoutreader, stderrreader;
        PrintWriter outputreturn;
        boolean skipextrapolate = false;

        if (!fromvex)
        {
            JOptionPane.showMessageDialog(this,
                                          "Manual creation of model files not yet supported - fake up a vex file",
                                          "Sorry!!!", JOptionPane.ERROR_MESSAGE);
            return;
        }

        returnval = vexchooser.showOpenDialog(this);
        if (returnval == JFileChooser.APPROVE_OPTION)
        {
            vexpath = vexchooser.getSelectedFile().getAbsolutePath();

            rt = Runtime.getRuntime();
            try
            {
                System.out.println("About to execute " + vex2modelcommand + " " +
                                   vexpath + " " +
                                   vexchooser.getCurrentDirectory());
                vex2model = rt.exec(vex2modelcommand + " " + vexpath + " " +
                                    vexchooser.getCurrentDirectory() + "/");
                System.out.println("Have started execution");
                stderrreader = new BufferedReader(new InputStreamReader(
                    vex2model.getErrorStream()));
                stdoutreader = new BufferedReader(new InputStreamReader(
                    vex2model.getInputStream()));
                System.out.println("Have created inputstreamreaders");
                //check and see if a reference is required for ATCA
                line = stdoutreader.readLine();
                while (line != null)
                {
                    System.out.println("Read: " + line);
                    if (line.startsWith("Enter ATCA station reference"))
                    {
                        line = JOptionPane.showInputDialog(this,
                            "Please enter the ATCA reference position (eg CATW104): ",
                            "Additional information",
                            JOptionPane.QUESTION_MESSAGE);
                        outputreturn = new PrintWriter(new OutputStreamWriter(
                            vex2model.getOutputStream()));
                        outputreturn.println(line);
                        outputreturn.close();
                    }
                    else if (!skipextrapolate &&
                             line.indexOf(
                                 "EOP FILE NOT UP TO DATE - INTERPOLATING") >=
                             0)
                    {
                        returnval = JOptionPane.showConfirmDialog(this, "The EOP file is not up to date and CALC must extrapolate from old EOPs - do you want to continue?  (Suggested course of action: Select NO and run update_eops in your $CALCDB directory)",
                            "Warning - out-of-date EOPs!!",
                            JOptionPane.YES_NO_OPTION);
                        if (returnval == JOptionPane.YES_OPTION)
                            skipextrapolate = true;
                        else
                            return; //bail out since the EOPs aren't up to date
                    }
                    line = stdoutreader.readLine();
                }

                if ( (line = stderrreader.readLine()) != null)
                {
                    JOptionPane.showMessageDialog(this,
                                                  "Error executing " +
                                                  vex2modelcommand +
                                                  ": " + line, "Error!!!",
                                                  JOptionPane.ERROR_MESSAGE);
                    return; //abort, couldn't load the file
                }
                System.out.println("About to waitfor");
                returnval = vex2model.waitFor();
            }
            catch (Exception e)
            {
                JOptionPane.showMessageDialog(this,
                                              "Error executing " +
                                              vex2modelcommand +
                                              ": " + e.getMessage(), "Error!!!",
                                              JOptionPane.ERROR_MESSAGE);
            }
        }
    }

    private void addCalcStation()
    {
        JOptionPane.showMessageDialog(this,
                                      "Adding a CALC station is not supported yet - edit $CALCDB/stations.tab by hand",
                                      "Sorry!!!", JOptionPane.ERROR_MESSAGE);
    }

    private void addCalcSource()
    {
        JOptionPane.showMessageDialog(this,
                                      "Adding a CALC station is not supported yet - edit $CALCDB/source.tab by hand",
                                      "Sorry!!!", JOptionPane.ERROR_MESSAGE);
    }

    private void readsetup()
    {
        BufferedReader input = null;
        String calc_root = null;
        int walltimehours;

        // read in environment variables
        //corr_root = System.getenv("CORR_ROOT");
        // calc_root = System.getenv("CALCDB");


        System.err.println();

        // validate environment
        /* if (corr_root == null)
         {
             Auxiliary.terminateOnError(
                 "Environment variable CORR_ROOT must be set");
         }*/
        /*
                if (calc_root == null)
                {
                    Auxiliary.terminateOnError(
                        "Environment variable CALCDB must be set");
                }
         */

        String appPath = Auxiliary.getApplicationPath(DiFXManager.class);

        // read the ini file
        try
        {
            input = new BufferedReader(new FileReader(appPath + INIFILE));
        }
        catch (Exception ex)
        {
            Auxiliary.terminateOnError("Can't open " + appPath + INIFILE);
        }

        try
        {
            mpifxcorrcommand = corrconfig.getinputline(input, "MPIFXCORR CMD");

            vex2configcommand = corrconfig.getinputline(input, "VEX2CONFIG CMD");

            vex2modelcommand = corrconfig.getinputline(input, "VEX2MODEL CMD");

            vex2modetimelistcommand = corrconfig.getinputline(input,
                "VEX2MODELLIST CMD");

            getnodescommand = corrconfig.getinputline(input, "GETNODELIST CMD");

            getpbsnodescommand = corrconfig.getinputline(input,
                "GETPBSNODELIST CMD");

            getqueuescommand = corrconfig.getinputline(input, "GETQUEUES CMD");

            walltimehours = Integer.parseInt(corrconfig.getinputline(input,
                "PBS WALLTIME HOURS"));

            queuegatewaymachine = corrconfig.getinputline(input,
                "QUEUE GATEWAY");

            corrconfig.setWalltime(walltimehours);
            corrconfig.setProcsPerNode(1);
        }
        catch (IOException e)
        {
            String message = "Problem reading " + appPath + INIFILE + " " +
                e.getMessage() + " DiFX is aborting";

            JOptionPane.showMessageDialog(this, message, "FATAL ERROR!!!",
                                          JOptionPane.ERROR_MESSAGE);
            System.exit(1);
        }

        validateSetup();

    }

    /**
     * Validates the paramters read from the input commads file.
     * Terminates the application in case invalid parameters are found.
     */
    private void validateSetup()
    {

        if (!Auxiliary.FileExists(mpifxcorrcommand))
        {
            Auxiliary.terminateOnError("Invalid command file parameter value: " +
                                       mpifxcorrcommand);
        }

        if (!Auxiliary.FileExists(vex2configcommand))
        {
            Auxiliary.terminateOnError("Invalid command file parameter value: " +
                                       vex2configcommand);
        }

        if (!Auxiliary.FileExists(vex2modelcommand))
        {
            Auxiliary.terminateOnError("Invalid command file parameter value: " +
                                       vex2modelcommand);
        }

        if (!Auxiliary.FileExists(vex2modetimelistcommand))
        {
            Auxiliary.terminateOnError("Invalid command file parameter value: " +
                                       vex2modetimelistcommand);
        }

        if (!Auxiliary.FileExists(getnodescommand))
        {
            Auxiliary.terminateOnError("Invalid command file parameter value: " +
                                       getnodescommand);
        }

        if (!Auxiliary.FileExists(getpbsnodescommand))
        {
            Auxiliary.terminateOnError("Invalid command file parameter value: " +
                                       getpbsnodescommand);
        }

        if (!Auxiliary.FileExists(getqueuescommand))
        {
            Auxiliary.terminateOnError("Invalid command file parameter value: " +
                                       getqueuescommand);
        }

        if (queuegatewaymachine == null)
        {
            Auxiliary.terminateOnError(
                "Missing command file parameter: QUEUE GATEWAY");
        }

    }

    private void trimDataFilesBySource()
    {
        int accept = JOptionPane.showConfirmDialog(this, "This function only works with data files of format ***_dayofyear_hhmmss.xxx - are you sure you want to continue?",
            "Warning!!!", JOptionPane.YES_NO_OPTION,
            JOptionPane.QUESTION_MESSAGE);
        if (accept == JOptionPane.YES_OPTION)
        {
            corrconfig.trimDataFilesBySource();
        }
    }

    private void trimDataFilesByTimerange()
    {
        int accept = JOptionPane.showConfirmDialog(this, "This function only works with data files of format ***_dayofyear_hhmmss.xxx - are you sure you want to continue?",
            "Warning!!!", JOptionPane.YES_NO_OPTION,
            JOptionPane.QUESTION_MESSAGE);
        if (accept == JOptionPane.YES_OPTION)
        {
            corrconfig.trimDataFilesByTimerange();
        }
    }

    private void trimDataFilesByMode()
    {
        String vexpath, mode, line, vals = "";
        String[] splitline;
        Runtime rt;
        Process vex2modetimelist;
        BufferedReader stdoutreader, stderrreader;
        int accept, numentries;
        int[] modestarttimes;
        int[] modestoptimes;

        accept = JOptionPane.showConfirmDialog(this, "This function only works with data files of format ***_dayofyear_hhmmss.xxx - are you sure you want to continue?",
                                               "Warning!!!",
                                               JOptionPane.YES_NO_OPTION,
                                               JOptionPane.QUESTION_MESSAGE);
        if (accept == JOptionPane.YES_OPTION)
        {
            mode = JOptionPane.showInputDialog(this,
                                               "Enter the name of the mode you want to cut down to",
                                               "Mode Entry",
                                               JOptionPane.QUESTION_MESSAGE);
            accept = vexchooser.showOpenDialog(this);
            if (accept == JFileChooser.APPROVE_OPTION)
            {
                vexpath = vexchooser.getSelectedFile().getAbsolutePath();

                rt = Runtime.getRuntime();
                try
                {
                    System.out.println("About to execute " +
                                       vex2modetimelistcommand + " " + vexpath +
                                       " " + mode);
                    vex2modetimelist = rt.exec(vex2modetimelistcommand + " " +
                                               vexpath + " " + mode);
                    System.out.println("Have started execution");
                    stderrreader = new BufferedReader(new InputStreamReader(
                        vex2modetimelist.getErrorStream()));
                    stdoutreader = new BufferedReader(new InputStreamReader(
                        vex2modetimelist.getInputStream()));
                    System.out.println("Have created inputstreamreaders");
                    line = stdoutreader.readLine();
                    while (line != null)
                    {
                        System.out.println("Read line " + line);
                        vals = vals + " " + line.trim();
                        System.out.println("Vals is now " + vals);
                        line = stdoutreader.readLine();
                    }
                    if ( (line = stderrreader.readLine()) != null)
                    {
                        System.out.println("Stderr not empty! Was " + line);
                        JOptionPane.showMessageDialog(this,
                            "Error executing " + vex2modetimelistcommand + ": " +
                            line, "Error!!!", JOptionPane.ERROR_MESSAGE);
                        return; //abort, couldn't load the file
                    }
                    System.out.println("About to waitfor");
                    vex2modetimelist.waitFor();
                    splitline = vals.trim().split(" ");
                    numentries = splitline.length / 2;
                    if (numentries == 0)
                    {
                        JOptionPane.showMessageDialog(this,
                            "Didn't find any scans for mode " + mode,
                            "Error!!!", JOptionPane.ERROR_MESSAGE);
                        return; //abort, couldn't load the file
                    }
                    modestarttimes = new int[numentries];
                    modestoptimes = new int[numentries];
                    for (int i = 0; i < numentries; i++)
                    {
                        System.out.println("Storing entry " + i + " - will be " +
                                           splitline[2 * i] + ", " +
                                           splitline[2 * i + 1]);
                        modestarttimes[i] = Integer.parseInt(splitline[2 * i]);
                        modestoptimes[i] = Integer.parseInt(splitline[2 * i + 1]);
                    }
                    System.out.println("About to actually trim the files");
                    corrconfig.trimDataFilesByMode(numentries, modestarttimes,
                        modestoptimes);
                }
                catch (Exception e)
                {
                    JOptionPane.showMessageDialog(this,
                                                  "Error executing " +
                                                  vex2modetimelistcommand +
                                                  ": " + e.getMessage(),
                                                  "Error!!!",
                                                  JOptionPane.ERROR_MESSAGE);
                }
            }
        }
    }

    private void closegui()
    {
        int response, saveresponse;

        if (! ( ( (DiFXPanel) displayarea.getSelectedComponent()).isUpdated()))
        {
            response = JOptionPane.showConfirmDialog(this, "Commit changes to the correlation configuration and save to disk before exiting?",
                "Save before exit?", JOptionPane.YES_NO_CANCEL_OPTION,
                JOptionPane.QUESTION_MESSAGE);
            switch (response)
            {
                case JOptionPane.CANCEL_OPTION:
                    break;
                case JOptionPane.YES_OPTION:
                    ( (DiFXPanel) displayarea.getSelectedComponent()).
                        commitcorrchanges();
                    saveresponse = saveCorrConfigFileAs();
                    if (saveresponse == JFileChooser.CANCEL_OPTION)
                        break;
                    //otherwise fall through to the NO response, which is to actually exit
                case JOptionPane.NO_OPTION:
                    System.exit(0);
            }
            return;
        }
        else if (!corrconfig.isUpdated())
        {
            response = JOptionPane.showConfirmDialog(this,
                "Save changes to the current correlation configuration before exiting?",
                "Save before exit?", JOptionPane.YES_NO_CANCEL_OPTION,
                JOptionPane.QUESTION_MESSAGE);
            switch (response)
            {
                case JOptionPane.CANCEL_OPTION:
                    break;
                case JOptionPane.YES_OPTION:
                    saveresponse = saveCorrConfigFileAs();
                    if (saveresponse == JFileChooser.CANCEL_OPTION)
                        break;
                    //otherwise fall through to the NO response, which is to actually exit
                case JOptionPane.NO_OPTION:
                    System.exit(0);
            }
        }
        else
            System.exit(0);
    }

    private void setGuiInputState (boolean enable)
    {
        // disable tab panes
        displayarea.setEnabled(enable);

        // disable buttons
        lastselectedpanel.commitchangebutton.setEnabled(enable);
        lastselectedpanel.launchbutton.setEnabled(enable);
        lastselectedpanel.consistencycheckbutton.setEnabled(enable);

    }

    /**
     * Initializiation of gui components
     * @throws Exception
     */
    private void jbInit() throws Exception
    {

        displayarea.addChangeListener(this);

        pulsarpanel = new PulsarPanel(this, corrconfig);
        corrsummarypanel = new CorrSummaryPanel(this, corrconfig);
        editcommonpanel = new EditCommonPanel(this, corrconfig);
        editconfigpanel = new EditConfigPanel(this, corrconfig);
        editdatastreampanel = new EditDatastreamPanel(this, corrconfig);
        editbaselinepanel = new EditBaselinePanel(this, corrconfig);
        editnodepanel = new EditNodePanel(this, corrconfig);
        editfreqpanel = new EditFreqPanel(this, corrconfig);



        lastselectedpanel = corrsummarypanel;
        mitExit.setText("Exit");
        mitExit.addActionListener(new DiFXgui_mitExit_actionAdapter(this));
        mitSaveAs.addActionListener(new DiFXgui_mitSaveAs_actionAdapter(this));
        mitSaveAs.setText("Save config file as");
        mitSave.addActionListener(new DiFXgui_mitSave_actionAdapter(this));
        mitSave.setText("Save config file");
        mitOpen.addActionListener(new DiFXgui_mitOpen_actionAdapter(this));
        mitNew.addActionListener(new DiFXgui_mitNew_actionAdapter(this));
        mitNewFromVex.addActionListener(new DiFXgui_mitNewFromVex_actionAdapter(this));
        mitNewFromVex.setText("Create new config file from vex");
        mitNew.setText("Create new blank config file");
        mitOpen.setText("Open existing config file");
        mitOpenBinconfig.setText("Open binconfig file");
        mitOpenBinconfig.addActionListener(new
            DiFXgui_mitOpenBinconfig_actionAdapter(this));
        mitOpenCube.setText("Open pulsarcube file");
        mitOpenCube.addActionListener(new DiFXgui_mitOpenCube_actionAdapter(this));
        mitSaveBinconfig.setText("Save binconfig file");
        mitSaveBinconfig.addActionListener(new
            DiFXgui_mitSaveBinconfig_actionAdapter(this));
        mitCalcFromVex.setText("Generate model using CALC from a vex file");
        mitCalcFromVex.addActionListener(new
                                         DiFXgui_mitCalcFromVex_actionAdapter(this));
        mitCalcManual.setText("Generate model using CALC manually");
        mitCalcManual.addActionListener(new DiFXgui_mitCalcManual_actionAdapter(this));
        mitAddCalcStation.setText("Add a station to the CALC database");
        mitAddCalcStation.addActionListener(new
            DiFXgui_mitAddCalcStation_actionAdapter(this));
        mitAddCalcSource.setText("Add a source to the CALC database");
        mitAddCalcSource.addActionListener(new
            DiFXgui_mitAddCalcSource_actionAdapter(this));
        mitTrimMatchConfig.setText("Trim datafile selection to match configs");
        mitTrimMatchConfig.addActionListener(new
            DiFXgui_mitTrimMatchConfig_actionAdapter(this));
        mitTrimMatchTimerange.setText(
            "Trim datafile selection to match correlation timerange");
        mitTrimMatchTimerange.addActionListener(new
            DiFXgui_mitTrimMatchTimerange_actionAdapter(this));
        mitTrimMatchMode.setText("Trim datafile selections to match a mode");
        mitTrimMatchMode.addActionListener(new
            DiFXgui_mitTrimMatchMode_actionAdapter(this));
        this.getContentPane().setLayout(borderLayout1);
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        this.addWindowListener(new DiFXgui_this_windowAdapter(this));
        displayarea.setBorder(BorderFactory.createLineBorder(Color.black));
        // add panes to the tab panel
        displayarea.add("Correlation Summary", corrsummarypanel);
        displayarea.add("Common Settings", editcommonpanel);
        displayarea.add("Config Settings", editconfigpanel);
        displayarea.add("Datastream Settings", editdatastreampanel);
        displayarea.add("Baseline Settings", editbaselinepanel);
        displayarea.add("Frequency Settings", editfreqpanel);
        displayarea.add("Node Settings", editnodepanel);
        displayarea.add("Pulsar", pulsarpanel); // creates the menu
        mainmenubar = new JMenuBar();
        pulsarmenu = new JMenu("Pulsar");
        mainmenu = new JMenu("Main");
        modelmenu = new JMenu("Model");
        advancedmenu = new JMenu("Advanced");


        mainmenu.add(mitOpen);
        mainmenu.add(mitNewFromVex);
        mainmenu.add(mitNew);
        mainmenu.add(mitSave);
        mainmenu.add(mitSaveAs);
        mainmenu.add(mitExit);
        pulsarmenu.add(mitOpenBinconfig);
        pulsarmenu.add(mitOpenCube);
        pulsarmenu.add(mitSaveBinconfig);

        modelmenu.add(mitCalcFromVex);
        modelmenu.add(mitCalcManual);

        modelmenu.add(mitAddCalcStation);
        modelmenu.add(mitAddCalcSource);

        //add all the menus to the menubar
        mainmenubar.add(mainmenu);
        mainmenubar.add(modelmenu);
        mainmenubar.add(pulsarmenu);
        mainmenubar.add(advancedmenu);
        advancedmenu.add(mitTrimMatchConfig);
        advancedmenu.add(mitTrimMatchTimerange);
        advancedmenu.add(mitTrimMatchMode);
        this.getContentPane().add(displayarea, java.awt.BorderLayout.CENTER);

         this.setJMenuBar(mainmenubar);
    }

    public void mitExit_actionPerformed(ActionEvent e)
    {
        closegui();
    }

    public void mitSaveAs_actionPerformed(ActionEvent e)
    {
        saveCorrConfigFileAs();
    }

    public void mitSave_actionPerformed(ActionEvent e)
    {
        corrconfig.saveCorrConfigFile();
    }

    public void mitOpen_actionPerformed(ActionEvent e)
    {
        loadCorrConfigFile();
        /** @todo set enbaled only if no errors have occured */
        setGuiInputState(true);
    }

    public void mitNew_actionPerformed(ActionEvent e)
    {

        createConfigFile(false);
        /** @todo set enbaled only if no errors have occured */
        setGuiInputState(true);
    }

    public void mitNewFromVex_actionPerformed(ActionEvent e)
    {

        createConfigFile(true);
        /** @todo set enbaled only if no errors have occured */
        setGuiInputState(true);
    }

    public void mitOpenBinconfig_actionPerformed(ActionEvent e)
    {
        pulsarpanel.openBinConfigFile();
    }

    public void mitOpenCube_actionPerformed(ActionEvent e)
    {
        pulsarpanel.openPulsarCubeFile();
    }

    public void mitSaveBinconfig_actionPerformed(ActionEvent e)
    {
        pulsarpanel.saveBinConfigFile(true);
    }

    public void mitCalcFromVex_actionPerformed(ActionEvent e)
    {
        createModelFiles(true);
    }

    public void mitCalcManual_actionPerformed(ActionEvent e)
    {
        createModelFiles(false);
    }

    public void mitAddCalcStation_actionPerformed(ActionEvent e)
    {
        addCalcStation();
    }

    public void mitAddCalcSource_actionPerformed(ActionEvent e)
    {
        addCalcSource();
    }

    public void mitTrimMatchConfig_actionPerformed(ActionEvent e)
    {
        trimDataFilesBySource();
    }

    public void mitTrimMatchTimerange_actionPerformed(ActionEvent e)
    {
        trimDataFilesByTimerange();
    }

    public void mitTrimMatchMode_actionPerformed(ActionEvent e)
    {
        trimDataFilesByMode();
    }

    public void this_windowClosing(WindowEvent e)
    {
        closegui();
    }
}

class DiFXgui_this_windowAdapter
    extends WindowAdapter
{
    private DiFXgui adaptee;
    DiFXgui_this_windowAdapter(DiFXgui adaptee)
    {
        this.adaptee = adaptee;
    }

    public void windowClosing(WindowEvent e)
    {
        adaptee.this_windowClosing(e);
    }
}

class DiFXgui_mitTrimMatchMode_actionAdapter
    implements ActionListener
{
    private DiFXgui adaptee;
    DiFXgui_mitTrimMatchMode_actionAdapter(DiFXgui adaptee)
    {
        this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e)
    {
        adaptee.mitTrimMatchMode_actionPerformed(e);
    }
}

class DiFXgui_mitTrimMatchTimerange_actionAdapter
    implements ActionListener
{
    private DiFXgui adaptee;
    DiFXgui_mitTrimMatchTimerange_actionAdapter(DiFXgui adaptee)
    {
        this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e)
    {
        adaptee.mitTrimMatchTimerange_actionPerformed(e);
    }
}

class DiFXgui_mitTrimMatchConfig_actionAdapter
    implements ActionListener
{
    private DiFXgui adaptee;
    DiFXgui_mitTrimMatchConfig_actionAdapter(DiFXgui adaptee)
    {
        this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e)
    {
        adaptee.mitTrimMatchConfig_actionPerformed(e);
    }
}

class DiFXgui_mitCalcManual_actionAdapter
    implements ActionListener
{
    private DiFXgui adaptee;
    DiFXgui_mitCalcManual_actionAdapter(DiFXgui adaptee)
    {
        this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e)
    {
        adaptee.mitCalcManual_actionPerformed(e);
    }
}

class DiFXgui_mitAddCalcStation_actionAdapter
    implements ActionListener
{
    private DiFXgui adaptee;
    DiFXgui_mitAddCalcStation_actionAdapter(DiFXgui adaptee)
    {
        this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e)
    {
        adaptee.mitAddCalcStation_actionPerformed(e);
    }
}

class DiFXgui_mitAddCalcSource_actionAdapter
    implements ActionListener
{
    private DiFXgui adaptee;
    DiFXgui_mitAddCalcSource_actionAdapter(DiFXgui adaptee)
    {
        this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e)
    {
        adaptee.mitAddCalcSource_actionPerformed(e);
    }
}

class DiFXgui_mitCalcFromVex_actionAdapter
    implements ActionListener
{
    private DiFXgui adaptee;
    DiFXgui_mitCalcFromVex_actionAdapter(DiFXgui adaptee)
    {
        this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e)
    {
        adaptee.mitCalcFromVex_actionPerformed(e);
    }
}

class DiFXgui_mitSaveBinconfig_actionAdapter
    implements ActionListener
{
    private DiFXgui adaptee;
    DiFXgui_mitSaveBinconfig_actionAdapter(DiFXgui adaptee)
    {
        this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e)
    {
        adaptee.mitSaveBinconfig_actionPerformed(e);
    }
}

class DiFXgui_mitOpenCube_actionAdapter
    implements ActionListener
{
    private DiFXgui adaptee;
    DiFXgui_mitOpenCube_actionAdapter(DiFXgui adaptee)
    {
        this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e)
    {
        adaptee.mitOpenCube_actionPerformed(e);
    }
}

class DiFXgui_mitOpenBinconfig_actionAdapter
    implements ActionListener
{
    private DiFXgui adaptee;
    DiFXgui_mitOpenBinconfig_actionAdapter(DiFXgui adaptee)
    {
        this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e)
    {
        adaptee.mitOpenBinconfig_actionPerformed(e);
    }
}

class DiFXgui_mitOpen_actionAdapter
    implements ActionListener
{
    private DiFXgui adaptee;
    DiFXgui_mitOpen_actionAdapter(DiFXgui adaptee)
    {
        this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e)
    {
        adaptee.mitOpen_actionPerformed(e);
    }
}

class DiFXgui_mitNew_actionAdapter
    implements ActionListener
{
    private DiFXgui adaptee;
    DiFXgui_mitNew_actionAdapter(DiFXgui adaptee)
    {
        this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e)
    {
        adaptee.mitNew_actionPerformed(e);
    }
}

class DiFXgui_mitNewFromVex_actionAdapter
    implements ActionListener
{
    private DiFXgui adaptee;
    DiFXgui_mitNewFromVex_actionAdapter(DiFXgui adaptee)
    {
        this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e)
    {
        adaptee.mitNewFromVex_actionPerformed(e);
    }
}

class DiFXgui_mitSave_actionAdapter
    implements ActionListener
{
    private DiFXgui adaptee;
    DiFXgui_mitSave_actionAdapter(DiFXgui adaptee)
    {
        this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e)
    {
        adaptee.mitSave_actionPerformed(e);
    }
}

class DiFXgui_mitExit_actionAdapter
    implements ActionListener
{
    private DiFXgui adaptee;
    DiFXgui_mitExit_actionAdapter(DiFXgui adaptee)
    {
        this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e)
    {
        adaptee.mitExit_actionPerformed(e);
    }
}

class DiFXgui_mitSaveAs_actionAdapter
    implements ActionListener
{
    private DiFXgui adaptee;
    DiFXgui_mitSaveAs_actionAdapter(DiFXgui adaptee)
    {
        this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent e)
    {
        adaptee.mitSaveAs_actionPerformed(e);
    }
}
