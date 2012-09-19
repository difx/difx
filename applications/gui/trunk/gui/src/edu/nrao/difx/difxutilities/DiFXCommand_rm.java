/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxutilities;

import edu.nrao.difx.difxview.SystemSettings;

import edu.nrao.difx.xmllib.difxmessage.DifxFileOperation;

/**
 *
 * @author jspitzak
 */
public class DiFXCommand_rm extends DiFXCommand {
    
    public DiFXCommand_rm( String target, String arg, SystemSettings settings ) {
        super( settings );
        this.header().setType( "DifxFileOperation" );
        DifxFileOperation rm = this.factory().createDifxFileOperation();
        rm.setPath( target );
        rm.setOperation( "rm" );
        rm.setArg( arg );
        //  The "data" node is assumed to be the same as the DiFX "control" node
        //  (at least for now).
        rm.setDataNode( settings.difxControlAddress() );
        this.body().setDifxFileOperation( rm );
    }
    
}
