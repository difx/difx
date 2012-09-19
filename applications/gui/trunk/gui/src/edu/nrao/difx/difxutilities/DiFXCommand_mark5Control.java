/*
 * This class composes a "mark5control" message and sends it to a specific host.
 * The XML syntax allows for multiple hosts, but we don't do that currently.
 */
package edu.nrao.difx.difxutilities;

import edu.nrao.difx.difxview.SystemSettings;

import edu.nrao.difx.xmllib.difxmessage.DifxCommand;

/**
 *
 * @author difx
 */
public class DiFXCommand_mark5Control extends DiFXCommand {
    
    public DiFXCommand_mark5Control( String cmd, String node, SystemSettings settings ) {
        super( settings );
        this.header().setType( "DifxCommand" );
        this.header().setTo( node );
        DifxCommand mark5Command = this.factory().createDifxCommand();
        this.body().setDifxCommand( mark5Command );

        mark5Command.setCommand( cmd );
        this.body().setDifxCommand( mark5Command );

    }
    
}
