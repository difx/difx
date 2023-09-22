/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package edu.nrao.difx.difxdatamodel;

import edu.nrao.difx.xmllib.difxmessage.*;

import java.math.BigDecimal;

public class Mark5Unit extends DiFXObject {

   public enum Mark5Commands
   {
      GETSVN, GETLOAD, GETDIR, RESETMARK5, STARTMARK5A, STOPMARK5A,
      CLEAR, REBOOT, POWEROFF, COPY      
   };

   // -- config fields
   private int    numCPUs  = 0;
   private int    numCores = 0;
   private float  bogusGHz = 0.0f;
   private short  type     = 0;         // support different type of units
   private String typeString = "";
   
   // -- status fields
   private String     state      = "";  //"Lost"; //"Online"
   private boolean    stateChanged = false;
   private float      cpuLoad    = 0.0f;
   private float      memLoad    = 0.0f;
   private long       totalMem   = 0l;
   private long       usedMem    = 0l;
   private int        netRxRate  = 0;
   private int        netTxRate  = 0;
   private boolean    enabled    = false;
   private String     bankAVSN   = "";
   private String     bankBVSN   = "";
   private String     statusWord = "";
   private String     activeBank = "";
   private int        scanNumber = 0;
   private String     scanName   = "";
   private long       position   = 0l;
   private float      playRate   = 0.0f;
   private BigDecimal dataMJD    = new BigDecimal(0.0);
   private String     currentJob = "";
   
   public Mark5Unit()
   {
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

   public boolean isStateChanged()
   {
      return stateChanged;
   }

   public void setStateChanged(boolean val)
   {
      stateChanged = val;
   }
   
   public float getCpuLoad()
   {
      return cpuLoad;
   }

   public void setCpuLoad(float val)
   {
      cpuLoad = val;
   }

   public boolean getEnabled()
   {
      return enabled;
   }

   public void setEnabled(boolean val)
   {
      enabled = val;
   }

   public float getMemLoad()
   {
      return memLoad;
   }

   public void setMemLoad(float val)
   {
      memLoad = val;
   }

   public long getTotalMem()
   {
      return totalMem;
   }

   public void setTotalMem(long val)
   {
      totalMem = val;
   }

   public long getUsedMem()
   {
      return usedMem;
   }

   public void setUsedMem(long val)
   {
      usedMem = val;
   }

   public int getNetRxRate()
   {
      return netRxRate;
   }

   public void setNetRxRate(int val)
   {
      netRxRate = val;
   }

   public int getNetTxRate()
   {
      return netTxRate;
   }

   public void setNetTxRate(int val)
   {
      netTxRate = val;
   }
   
   public String getBankAVSN()
   {
      return bankAVSN;
   }

   public void setBankAVSN(String val)
   {
      bankAVSN = val;
   }

   public String getBankBVSN()
   {
      return bankBVSN;
   }

   public void setBankBVSN(String val)
   {
      bankBVSN = val;
   }

   public String getStatusWord()
   {
      return statusWord;
   }

   public void setStatusWord(String val)
   {
      statusWord = val;
   }

   public String getActiveBank()
   {
      return activeBank;
   }

   public void setActiveBank(String val)
   {
      activeBank = val;
   }
   
   public int getScanNumber()
   {
      return scanNumber;
   }

   public void setScanNumber(int val)
   {
      scanNumber = val;
   }
   
   public String getScanName()
   {
      return scanName;
   }

   public void setScanName(String val)
   {
      scanName = val;
   }
   
   public long getPosition()
   {
      return position;
   }

   public void setPosition(long val)
   {
      position = val;
   }

   public float getPlayRate()
   {
      return playRate;
   }

   public void setPlayRate(float val)
   {
      playRate = val;
   }
   
   public BigDecimal getDataMJD()
   {
      return dataMJD;
   }

   public void setDataMJD(BigDecimal val)
   {
      dataMJD = val;
   }

   public String getCurrentJob()
   {
      return currentJob;
   }

   public void setCurrentJob(String val)
   {
      currentJob = val;
   }

   public DifxMessage CreateDiFXCommandMessage(Mark5Commands cmd)
   {
      ObjectFactory factory = new ObjectFactory();

      // Create header
      Header header = factory.createHeader();
      header.setFrom("doi");
      header.setTo(getObjName());
      header.setMpiProcessId("-1");
      header.setIdentifier("doi");
      header.setType("DifxCommand");

      // Create mark5 command
      DifxCommand mark5Command = factory.createDifxCommand();

      switch (cmd)
      {
         case GETSVN:
            mark5Command.setCommand("GetVSN");
            break;
         case GETLOAD:
            mark5Command.setCommand("GetLoad");
            break;
         case GETDIR:
            mark5Command.setCommand("GetDir");
            break;
         case RESETMARK5:
            mark5Command.setCommand("ResetMark5");
            break;
         case STARTMARK5A:
            mark5Command.setCommand("StartMark5A");
            break;
         case STOPMARK5A:
            mark5Command.setCommand("StopMark5A");
            break;
         case CLEAR:
            mark5Command.setCommand("Clear");
            break;
         case REBOOT:
            mark5Command.setCommand("Reboot");
            break;
         case POWEROFF:
            mark5Command.setCommand("Poweroff");
            break;
         case COPY:
            mark5Command.setCommand("Copy");
            break;
         default:
         {
            mark5Command.setCommand("Invalid");
         }
      }

      // -- Create the XML defined messages and process through the system
      Body body = factory.createBody();
      body.setDifxCommand(mark5Command);

      DifxMessage difxMsg = factory.createDifxMessage();
      difxMsg.setHeader(header);
      difxMsg.setBody(body);

      return difxMsg;
   }

   public DifxMessage CreateDiFXCommandMessage(String parms)
   {
      ObjectFactory factory = new ObjectFactory();

      // Create header
      Header header = factory.createHeader();
      header.setFrom("doi");
      header.setTo(this.getDataModel().getManagerNode().getObjName());
      header.setMpiProcessId("-1");
      header.setIdentifier(getObjName());
      header.setType("DifxCommand");

      // Create mark5 copy command, append parameters
      DifxCommand mark5Command = factory.createDifxCommand();
      mark5Command.setCommand("Copy"+parms);

      // -- Create the XML defined messages and process through the system
      Body body = factory.createBody();
      body.setDifxCommand(mark5Command);

      DifxMessage difxMsg = factory.createDifxMessage();
      difxMsg.setHeader(header);
      difxMsg.setBody(body);

      return difxMsg;
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
      // -- do not update the resource config fields
      //numCPUs    = ((Mark5Unit) newData).getNumCPUs();
      //numCores   = ((Mark5Unit) newData).getNumCores();
      //bogusGHz   = ((Mark5Unit) newData).getBogusGHz();
      //type       = ((Mark5Unit) newData).getType();
      //typeString = ((Mark5Unit) newData).getTypeString();
      state      = ((Mark5Unit) newData).getState();
      cpuLoad    = ((Mark5Unit) newData).getCpuLoad();
      memLoad    = ((Mark5Unit) newData).getMemLoad();
      totalMem   = ((Mark5Unit) newData).getTotalMem();
      usedMem    = ((Mark5Unit) newData).getUsedMem();
      netRxRate  = ((Mark5Unit) newData).getNetRxRate();
      netTxRate  = ((Mark5Unit) newData).getNetTxRate();
      enabled    = ((Mark5Unit) newData).getEnabled();
      bankAVSN   = ((Mark5Unit) newData).getBankAVSN();
      bankBVSN   = ((Mark5Unit) newData).getBankBVSN();
      statusWord = ((Mark5Unit) newData).getStatusWord();
      activeBank = ((Mark5Unit) newData).getActiveBank();
      scanNumber = ((Mark5Unit) newData).getScanNumber();
      scanName   = ((Mark5Unit) newData).getScanName();
      position   = ((Mark5Unit) newData).getPosition();
      playRate   = ((Mark5Unit) newData).getPlayRate();
      dataMJD    = ((Mark5Unit) newData).getDataMJD();            
      currentJob = ((Mark5Unit) newData).getCurrentJob();

      // update name, id, and type
      super.updateObject(newData);
   }

   @Override
   public boolean isEqual(DiFXObject objToCompare)
   {
      // -- compare config and status
      return ( numCPUs    == ((Mark5Unit) objToCompare).getNumCPUs()         &&
               numCores   == ((Mark5Unit) objToCompare).getNumCores()        &&
               bogusGHz   == ((Mark5Unit) objToCompare).getBogusGHz()        &&
               type       == ((Mark5Unit) objToCompare).getType()            &&
               typeString.equals(((Mark5Unit) objToCompare).getTypeString()) &&
               state.equals(((Mark5Unit) objToCompare).getState())           &&
               cpuLoad    == ((Mark5Unit) objToCompare).getCpuLoad()         &&
               memLoad    == ((Mark5Unit) objToCompare).getMemLoad()         &&
               totalMem   == ((Mark5Unit) objToCompare).getTotalMem()        &&
               usedMem    == ((Mark5Unit) objToCompare).getUsedMem()         &&
               netRxRate  == ((Mark5Unit) objToCompare).getNetRxRate()       &&
               netTxRate  == ((Mark5Unit) objToCompare).getNetTxRate()       &&
               enabled    == ((Mark5Unit) objToCompare).getEnabled()         &&
               bankAVSN.equals(((Mark5Unit) objToCompare).getBankAVSN())     &&
               bankBVSN.equals(((Mark5Unit) objToCompare).getBankBVSN())     &&
               statusWord.equals(((Mark5Unit) objToCompare).getStatusWord()) &&
               activeBank.equals(((Mark5Unit) objToCompare).getActiveBank()) &&
               scanNumber == ((Mark5Unit) objToCompare).getScanNumber()      &&
               scanName.equals(((Mark5Unit) objToCompare).getScanName())     &&
               position   == ((Mark5Unit) objToCompare).getPosition()        &&
               playRate   == ((Mark5Unit) objToCompare).getPlayRate()        &&
               dataMJD.equals(((Mark5Unit) objToCompare).getDataMJD())       &&
               currentJob.equals(((Mark5Unit) objToCompare).getCurrentJob()) &&
               super.isEqual(objToCompare) );
   }
   
   @Override
   public boolean isVerified()
   {
      return ( (state.equalsIgnoreCase("IDLE") || state.equalsIgnoreCase("CLOSE")) && enabled );
   }

}

