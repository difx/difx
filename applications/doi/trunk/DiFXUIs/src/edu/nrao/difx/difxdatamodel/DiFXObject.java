/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package edu.nrao.difx.difxdatamodel;

public abstract class DiFXObject
{
   private String        objType;
   private int           objId;
   private String        objName;
   private String        msgSrcId;           // sender of last message
   private long          statusTimeStampUTC; //time stamp of last status received
   private DiFXDataModel dataModel;          // Provide call back into the model, just in case

   public DiFXObject()
   {
      objType   = "";
      objId     = 0;
      objName   = "";
      msgSrcId  = "";
      statusTimeStampUTC = 0l;
      dataModel = null;
   }

   public String getObjType()
   {
      return objType;
   }

   public void setObjType(String val)
   {
      this.objType = val;
   }

   public DiFXObject getObject()
   {
      return this;
   }

   public int getObjId()
   {
      return objId;
   }

   public void setObjId(int val)
   {
      this.objId = val;
   }

   public String getObjName()
   {
      return objName;
   }

   public void setObjName(String val)
   {
      this.objName = val;
   }

   public String getMsgSrcId()
   {
      return msgSrcId;
   }

   public void setMsgSrcId(String val)
   {
      this.msgSrcId = val;
   }

   public long getStatusTimeStampUTC()
   {
      return statusTimeStampUTC;
   }

   public void setStatusTimeStampUTC()
   {
      this.statusTimeStampUTC = System.currentTimeMillis();
   }

   public void setStatusTimeStampUTC(long val)
   {
      this.statusTimeStampUTC = val;
   }

   public boolean isStatusCurrent()
   {
      // status is current for 20 seconds UTC
      boolean stat = true;
      return ( (statusTimeStampUTC + 20000l) > System.currentTimeMillis() );
   }

   public DiFXDataModel getDataModel()
   {
      return dataModel;
   }

   public void setDataModel(DiFXDataModel model)
   {
      this.dataModel = model;
   }

   public void updateObject(DiFXObject newData)
   {
      this.objType   = newData.getObjType();
      this.objId     = newData.getObjId();
      this.objName   = newData.getObjName();
      this.msgSrcId  = newData.getMsgSrcId();
      this.statusTimeStampUTC = newData.getStatusTimeStampUTC();
      this.dataModel = newData.getDataModel();
   }

   public boolean isEqual(DiFXObject objToCompare)
   {
      return ( this.objType.equals(objToCompare.getObjType())   &&
               this.objId == objToCompare.getObjId()            &&
               this.objName.equals(objToCompare.getObjName())   &&
               this.msgSrcId.equals(objToCompare.getMsgSrcId()) &&
               this.statusTimeStampUTC == 
                           objToCompare.getStatusTimeStampUTC() &&
               this.dataModel == objToCompare.getDataModel());
   }

   public boolean isVerified()
   {
      return true;
   }
}
