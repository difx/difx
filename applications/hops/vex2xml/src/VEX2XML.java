/**------------------------------------------------------
* ALMA - Atacama Large Millimeter Array
* (c) Associated Universities Inc., 2013
* (c) Massachusetts Institute of Technology, 2013-2018
* @author Victor Pankratius, MIT Haystack Observatory
* @author Geoffrey Crew, MIT Haystack Observatory
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/

import java.io.*;
import org.antlr.runtime.*;             //lexer and parser code generator
import org.apache.commons.cli.*;        //parsing command line options


public class VEX2XML {

        static String inputFile, outputFile;
        
        /**
         * List of command line options
         * See http://commons.apache.org/proper/commons-cli/usage.html
         */
        private static Options createCommandLineOptions() {
        final Options options = new Options();
        options.addOption("in", true, "VEX Input File");
        options.addOption("out", true, "XML Output File");
        options.addOption("help", false, "Show help information.");
        options.addOption("version", false, "Show version information.");
        return options;
    }

        /**
         * Print this text when called with -help option
         */
        private static void outputCommandLineHelp(final Options options) {      
        final HelpFormatter formater = new HelpFormatter();
        formater.printHelp("VEX2XML -in inputfile.vex -out outputfile.xml",
            options);
    }

    
        /**
         * Get values from command line
         */
        private static void processCommandline(
            final CommandLine cl) throws IllegalArgumentException {          
        
        if (cl.hasOption("in")) {
            inputFile = cl.getOptionValue("in");
        } else {
            System.out.println("Error: Missing input file");
            System.exit(-1);
        }
                
                
        if (cl.hasOption("out")) {
            outputFile = cl.getOptionValue("out");
        } else {
            System.out.println("Error: Missing output file");
            System.exit(-1);
        }
    }
    
        
    
    /**
     * Call ANTLR lexer and parser on file input stream
     * Append original VEX file as comment to XML output
     */
    private static void parseAndTranslate() throws Exception{
        System.out.println("\nInput: "+inputFile+"\nOutput: "+outputFile+"\n");
        
        ANTLRInputStream input = new ANTLRInputStream(
            new FileInputStream(inputFile));
        vexGrammarLexer lexer = new vexGrammarLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        vexGrammarParser parser = new vexGrammarParser(tokens, outputFile);
        parser.vex();

        parser.finalize(inputFile);//append original vex file when done
        
        System.out.println("\nDone.");
    }
    
    
    public static void main(String[] args) throws Exception {   

        System.out.println("-------------------------------------" +
            "-------------------------------------");
        System.out.println("VEX2XML v. 1.2");
        System.out.println("(C) MIT Haystack Observatory, 2013-2018");
        System.out.println("Authors: Victor Pankratius, Geoffrey Crew");
        System.out.println("$HeadURL: https://vault.haystack.mit.edu/svn/hops/trunk/vex2xml/src/VEX2XML.java $");
        System.out.println("-------------------------------------" +
            "-------------------------------------");
        
        // Parse command line
        Options options = createCommandLineOptions();
        CommandLine cl = null;
        try {
            CommandLineParser cmdParser = new PosixParser();
            cl = cmdParser.parse(options, args, true);
        } catch (final ParseException exp) {
            System.out.println("Error:" + exp.getMessage());
            System.exit(-1);
        }
        if (cl == null || args.length < 1) {
            outputCommandLineHelp(options);
            System.exit(-1);
        }
        
        // Process command line and get parameter values
        try {
            if (cl.hasOption("help") || cl.hasOption("version")) {
                outputCommandLineHelp(options);
                System.exit(0);
            } else {
                processCommandline(cl);
            }
        } catch (final IllegalArgumentException e) {
            outputCommandLineHelp(options);
            System.out.println("Illegal arguments on command line: " +
                e.getMessage());
            return;
        }
        
        parseAndTranslate();
    }
}

// eof
