package edu.nrao.difx.difxdatamodel;

public class ProcessorNode extends DiFXObject {

   // -- config fields
   private int     numCPUs;
   private int     numCores;
   private float   bogusGHz;
   private short   type;         // support different type of nodes
   private String  typeString;
   
   // -- status fields
   private String  state;
   private float   cpuLoad;
   private float   memLoad;
   private long    totalMem;
   private long    usedMem;
   private int     netRxRate;
   private int     netTxRate;
   private boolean enabled;

   public ProcessorNode()
   {
      numCPUs    = 0;
      numCores   = 0;
      bogusGHz   = 0.0f;
      type       = 0;
      typeString = "";
      state      = ""; // "Lost"
      cpuLoad    = 0.0f;
      memLoad    = 0.0f;
      totalMem   = 0l;
      usedMem    = 0l;
      netRxRate  = 0;
      netTxRate  = 0;
      enabled    = false;
   }

   public int getNumCPUs()
   {
      return numCPUs;
   }

   public void setNumCPUs(int val)
   {
      numCPUs = val;
   }

   public int getNumCores()
   {
      return numCores;
   }

   public void setNumCores(int val)
   {
      numCores = val;
   }

   public float getBogusGHz()
   {
      return bogusGHz;
   }

   public void setBogusGHz(float val)
   {
      bogusGHz = val;
   }

   public short getType()
   {
      return type;
   }

   public void setType(short val)
   {
      type = val;
   }

   public String getTypeString()
   {
      return typeString;
   }

   public void setTypeString(String val)
   {
      typeString = val;
   }

   public String getState()
   {
      return state;
   }

   public void setState(String val)
   {
      state = val;
   }

   public float getCpuLoad()
   {
      return cpuLoad;
   }

   public void setCpuLoad(float val)
   {
      this.cpuLoad = val;
   }

   public boolean getEnabled()
   {
      return enabled;
   }

   public void setEnabled(boolean val)
   {
      this.enabled = val;
   }

   public float getMemLoad()
   {
      return memLoad;
   }

   public void setMemLoad(float val)
   {
      this.memLoad = val;
   }

   public long getTotalMem()
   {
      return totalMem;
   }

   public void setTotalMem(long val)
   {
      this.totalMem = val;
   }

   public long getUsedMem()
   {
      return usedMem;
   }

   public void setUsedMem(long val)
   {
      this.usedMem = val;
   }

   public int getNetRxRate()
   {
      return netRxRate;
   }

   public void setNetRxRate(int val)
   {
      this.netRxRate = val;
   }

   public int getNetTxRate()
   {
      return netTxRate;
   }

   public void setNetTxRate(int val)
   {
      this.netTxRate = val;
   }

   @Override
   public boolean isStatusCurrent()
   {
      // status is current for 20 seconds UTC
      if ( this.state.equalsIgnoreCase("Rebooting") ||
           this.state.equalsIgnoreCase("PowerOff" ) )
      {
         return ( true );
      }
      else
      {
         return ( super.isStatusCurrent() );
      }
   }

   @Override
   public void updateObject(DiFXObject newData)
   {
      // -- do not update the config fields
      //numCPUs    = ((ProcessorNode) newData).getNumCPUs();
      //numCores   = ((ProcessorNode) newData).getNumCores();
      //bogusGHz   = ((ProcessorNode) newData).getBogusGHz();
      //type       = ((ProcessorNode) newData).getType();
      //typeString = ((ProcessorNode) newData).getTypeString();
      state     = ((ProcessorNode) newData).getState();
      cpuLoad   = ((ProcessorNode) newData).getCpuLoad();
      memLoad   = ((ProcessorNode) newData).getMemLoad();
      totalMem  = ((ProcessorNode) newData).getTotalMem();
      usedMem   = ((ProcessorNode) newData).getUsedMem();
      netRxRate = ((ProcessorNode) newData).getNetRxRate();
      netTxRate = ((ProcessorNode) newData).getNetTxRate();
      enabled   = ((ProcessorNode) newData).getEnabled();
      super.updateObject(newData);
   }

   @Override
   public boolean isEqual(DiFXObject objToCompare)
   {
      // -- compare config and status
      return ( numCPUs   == ((ProcessorNode) objToCompare).getNumCPUs()          &&
               numCores  == ((ProcessorNode) objToCompare).getNumCores()         &&
               bogusGHz  == ((ProcessorNode) objToCompare).getBogusGHz()         &&
               type      == ((ProcessorNode) objToCompare).getType()             &&
               typeString.equals(((ProcessorNode) objToCompare).getTypeString()) &&
               state.equals(((ProcessorNode) objToCompare).getState())           &&
               cpuLoad   == ((ProcessorNode) objToCompare).getCpuLoad()          &&
               memLoad   == ((ProcessorNode) objToCompare).getMemLoad()          &&
               totalMem  == ((ProcessorNode) objToCompare).getTotalMem()         &&
               usedMem   == ((ProcessorNode) objToCompare).getUsedMem()          &&
               netRxRate == ((ProcessorNode) objToCompare).getNetRxRate()        &&
               netTxRate == ((ProcessorNode) objToCompare).getNetTxRate()        &&
               enabled   == ((ProcessorNode) objToCompare).getEnabled()          &&
               super.isEqual(objToCompare));
   }

   @Override
   public boolean isVerified()
   {
      return (state.equalsIgnoreCase("IDLE") && enabled);
   }

}

