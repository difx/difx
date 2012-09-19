/*
 * Tell the DiFX host to create a particular directory path.
 */
package edu.nrao.difx.difxutilities;

import edu.nrao.difx.difxview.SystemSettings;

import edu.nrao.difx.xmllib.difxmessage.DifxFileOperation;

/**
 *
 * @author jspitzak
 */
public class DiFXCommand_mkdir extends DiFXCommand {
    
    public DiFXCommand_mkdir( String newDir, SystemSettings settings ) {
        super( settings );
        this.header().setType( "DifxFileOperation" );
        DifxFileOperation mkdir = this.factory().createDifxFileOperation();
        mkdir.setPath( newDir );
        mkdir.setOperation( "mkdir" );
        //  The "data" node is assumed to be the same as the DiFX "control" node
        //  (at least for now).
        mkdir.setDataNode( settings.difxControlAddress() );
        this.body().setDifxFileOperation( mkdir );
    }
    
}
