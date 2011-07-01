/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxdatamodel;

/**
 *
 * @author mguerra
 */
public class Module extends DiFXObject {

   // -- config fields
   private String moduleVSN;  // VSN
   private String shelf;

   // -- status fields
   private String state;
   private float  weight;
 
   // Constructor
   public Module()
   {
      // init config/status fields
      moduleVSN = "";
      shelf     = "";
      state     = "";
      weight    = 0.0f;      
   }

   // Config methods
   public String getModuleVSN()
   {
      return moduleVSN;
   }

   public void setModuleVSN(String val)
   {
      moduleVSN = val;
   }

   public String getShelf()
   {
      return shelf;
   }

   public void setShelf(String val)
   {
      shelf = val;
   }

   // Status methods
   public String getState()
   {
      return state;
   }

   public void setState(String val)
   {
      state = val;
   }

   public float getWeight()
   {
      return weight;
   }

   public void setWeight(float val)
   {
      weight = val;
   }

   @Override
   public void updateObject(DiFXObject newData)
   {
      // -- do not update the config fields
      this.moduleVSN = ((Module) newData).getModuleVSN();
      this.shelf     = ((Module) newData).getShelf();
      this.state     = ((Module) newData).getState();
      this.weight    = ((Module) newData).getWeight();

      // update name, id, and type
      super.updateObject(newData);
   }
 
   @Override
   public boolean isEqual(DiFXObject objToCompare)
   {
      // -- compare config and status
      return ( (this.moduleVSN.equalsIgnoreCase(((Module)objToCompare).getModuleVSN())) &&
               (this.shelf.equalsIgnoreCase(((Module)objToCompare).getShelf()))         &&
               (this.state.equalsIgnoreCase(((Module)objToCompare).getState()))         &&
               (this.weight == ((Module)objToCompare).getWeight())                      &&
               super.isEqual(objToCompare) );
   }   
}
