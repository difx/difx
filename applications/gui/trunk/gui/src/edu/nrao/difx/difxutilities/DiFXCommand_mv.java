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
public class DiFXCommand_mv extends DiFXCommand {
    
    public DiFXCommand_mv( String from, String to, SystemSettings settings ) {
        super( settings );
        this.header().setType( "DifxFileOperation" );
        DifxFileOperation mv = this.factory().createDifxFileOperation();
        mv.setPath( from );
        mv.setOperation( "mv" );
        mv.setArg( to );
        //  The "data" node is assumed to be the same as the DiFX "control" node
        //  (at least for now).
        mv.setDataNode( settings.difxControlAddress() );
        this.body().setDifxFileOperation( mv );
    }
    
}
