package edu.nrao.difx.difxdatamodel;


public class Cluster extends DiFXObject {

    public Cluster () {
    }

    @Override
    public void updateObject (DiFXObject newData) {
       super.updateObject(newData);
    }

    @Override
    public boolean isEqual (DiFXObject objToCompare) {
       return( super.isEqual(objToCompare) );
    }

}

