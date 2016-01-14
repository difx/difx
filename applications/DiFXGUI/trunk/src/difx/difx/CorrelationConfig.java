/***************************************************************************
 *   Copyright (C) 2016 by NRAO                                            *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
package difx;


import java.awt.Component;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.EOFException;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.JOptionPane;

public class CorrelationConfig {
	private static final String blanks = "                    ";
	
	private boolean commonread, datastreamread, configread, dataoverride, updated, usepbs;
	private int baselinetablelength, telescopetablelength, datastreamtablelength, freqtablelength;
	private int numbaselines, numdatastreams, numactiveconfigs, numsources, defaultconfigindex;
	private int executeseconds, startseconds, startmjd, databufferfactor, numdatasegments, numnodes, walltimehours, procspernode;
	private String configfilename, delayfilename, uvwfilename, coreconffilename, outputfilename;
	private String machinefilename, outputformatstring, clustername;
	private String [] sourcenames;
	private ConfigData [] configs;
	private FreqData [] freqtable;
	private TelescopeData [] telescopetable;
	private BaselineData [] baselinetable;
	private DatastreamData [] datastreamtable;
	private String [] polpairs;
	
	Component mainwindow;
	int numconfigsloaded;
	  
	public CorrelationConfig(DiFXgui mainwindow) {
		this.mainwindow = mainwindow;
		this.reset();
		updated = true;
	}
	
	public static String [] getDataFormats() { return DataFormat.formatstrings; }
	public String [] getPolPairs() {
		return polpairs;
	}
	public String getCurrentFilename() {
		return configfilename;
	}
	public int getNumConfigsLoaded() {
		return numconfigsloaded;
	}
	public int getNumActiveConfigs() {
		return numactiveconfigs;
	}
	public int getNumConfigs() {
		return configs.length;
	}
	public boolean isPulsarBinning(int config) {
		return configs[config].pulsarbin;
	}
	public String getPulsarConfigFilename(int config) {
		return configs[config].pulsarfilename;
	}
	public String getConfigSourceName(int config) {
		return configs[config].sourcename;
	}
	public boolean configActive(int config) {
		return configs[config].active;
	}
	public double getIntTime(int configentry) {
		return configs[configentry].inttime;
	}
	public int getBlocksPerSend(int configentry) {
		return configs[configentry].blockspersend;
	}
	public int getGuardBlocks(int configentry) {
		return configs[configentry].guardblocks;
	}
	public int getNumChannels(int configentry) {
		return configs[configentry].numchannels;
	}
	public int [] getConfigDatastreamIndices(int configentry) {
		return configs[configentry].datastreamindices;
	}
	public int [] getConfigBaselineIndices(int configentry) {
		return configs[configentry].baselineindices;
	}
	public String getThreadFilename() {
		return coreconffilename;
	}
	public String getDelayFilename() {
		return delayfilename;
	}
	public String getClusterName() {
		return clustername;
	}
	public String getMachineFilename() {
		return machinefilename;
	}
	public int getExecuteSeconds() {
		return executeseconds;
	}
	public int getNumBaselines() {
		return numbaselines;
	}
	public int getNumDatastreams() {
		return numdatastreams;
	}
	public int getNumDataSegments() {
		return numdatasegments;
	}
	public int getDataBufferFactor() {
		return databufferfactor;
	}
	public int getFreqTableLength() {
		return freqtablelength;
	}
	public int getDatastreamTableLength() {
		return datastreamtablelength;
	}
	public int getBaselineTableLength() {
		return baselinetablelength;
	}
	public int getTelescopeTableLength() {
		return telescopetablelength;
	}
	public String getOutputFilename() {
		return outputfilename;
	}
	public String getOutputFormat() {
		return outputformatstring;
	}
	public int getStartMJD() {
		return startmjd;
	}
	public int getStartSeconds() {
		return startseconds;
	}
	public String getUVWFilename() {
		return uvwfilename;
	}
	public boolean isUpdated() {
		return updated;
	}
	public int getNumNodes() {
		return numnodes;
	}
        public int getWalltime() {
                return walltimehours;
        }
        public int getProcsPerNode() {
                return procspernode;
        }
	public String getTelescopeName(int telescope) {
		return telescopetable[telescope].name;
	}
	public double getTelescopeClockDelay(int telescope) {
		return telescopetable[telescope].clockdelay;
	}
	public double getTelescopeClockRate(int telescope) {
		return telescopetable[telescope].clockrate;
	}
	public double getBandEdgeFreq(int freqentry) {
		return freqtable[freqentry].bandedgefreq;
	}
	public double getBandwidth(int freqentry) {
		return freqtable[freqentry].bandwidth;
	}
	public boolean isLowerSideband(int freqentry) {
		return freqtable[freqentry].lowersideband;
	}
	public String getDatastreamName(int index) {
		return telescopetable[datastreamtable[index].telescopeindex].name;
	}
	public String getDatastreamBandSummary(int index) {
		return datastreamtable[index].getBandSummary();
	}
	public String getBaselineName(int index) {
		return baselinetable[index].getName();
	}
	public int getBaselineDS1index(int baselineindex) {
		return baselinetable[baselineindex].datastream1index;
	}
	public int getBaselineDS2index(int baselineindex) {
		return baselinetable[baselineindex].datastream2index;
	}
	public String getBaselineBandSummary(int index) {
		return baselinetable[index].getBandSummary();
	}
	public int getTelescopeIndex(int datastreamindex) {
		return datastreamtable[datastreamindex].telescopeindex;
	}
	public double getTsys(int dsindex) {
		return datastreamtable[dsindex].tsys;
	}
	public int getDataFormatIndex(int dsindex) {
		return datastreamtable[dsindex].format;
	}
	public int getDatastreamNumFreqs(int dsindex) {
		return datastreamtable[dsindex].numfreqs;
	}
	public double getDatastreamFreqClockOffset(int dsindex, int freq) {
		return datastreamtable[dsindex].freqclockoffsets[freq];
	}
	public int getDatastreamFreqIndex(int dsindex, int freq) {
		return datastreamtable[dsindex].freqtableindices[freq];
	}
	public int getFanout(int dsindex) {
		return datastreamtable[dsindex].fanout;
	}
	public int getNumInputBands(int dsindex) {
		return datastreamtable[dsindex].numinputbands;
	}
	public char getPolarisation(int dsindex, int bandindex) {
		return datastreamtable[dsindex].inputbandpols[bandindex];
	}
	public int getFreqIndex(int dsindex, int bandindex) {
		return datastreamtable[dsindex].freqtableindices[datastreamtable[dsindex].inputbandlocalfreqindices[bandindex]];
	}
	public boolean isNetworkDatastream(int dsindex) {
		return !datastreamtable[dsindex].readfromfile;
	}
	public int getPort(int dsindex) {
		return datastreamtable[dsindex].portnumber;
	}
	public int getTCPWindowSize(int dsindex) {
		return datastreamtable[dsindex].tcpwindowsizekb;
	}
	public String [] getDataFileList(int dsindex) {
		return datastreamtable[dsindex].datafilenames;
	}
	public String [] getSourceNames() {
		return sourcenames;
	}
	public boolean usePbs() {
		return usepbs;
	}
	public int [] getTelescopeDatastreamIndices(int telescopeindex) {
		int telescopedatastreams = 0;
		for(int i=0;i<datastreamtable.length;i++) {
			System.out.println("Doing datastream " + i);
			if(datastreamtable[i].telescopeindex == telescopeindex)
				telescopedatastreams++;
		}
		int [] toreturn = new int[telescopedatastreams];
		telescopedatastreams = 0;
		for(int i=0;i<datastreamtable.length;i++) {
			if(datastreamtable[i].telescopeindex == telescopeindex)
				toreturn[telescopedatastreams++] = i;
		}
		return toreturn;
	}
	public String [] getFreqList() {
		String [] toreturn = new String[freqtablelength];
		for(int i=0;i<freqtablelength;i++)
			toreturn[i] = freqtable[i].toString();
		return toreturn;
	}
	public boolean productselected(int baselineentry, int freqentry, int polproductindex) {
		BaselineData bd = baselinetable[baselineentry];
		DatastreamData dd = datastreamtable[bd.datastream1index];
		
		for(int j=0;j<bd.numfreqs;j++) {
			if(bd.datastream1bandindex[j][0] < dd.inputbandlocalfreqindices.length && 
					dd.freqtableindices[dd.inputbandlocalfreqindices[bd.datastream1bandindex[j][0]]] == freqentry) {
				for(int k=0;k<bd.numpolproducts[j];k++) {
					if(bd.datastream1bandindex[j][k] < datastreamtable[bd.datastream1index].inputbandpols.length
					&& bd.datastream2bandindex[j][k] < datastreamtable[bd.datastream2index].inputbandpols.length
					&& datastreamtable[bd.datastream1index].inputbandpols[bd.datastream1bandindex[j][k]] 
					   == polpairs[polproductindex].charAt(0)
					&& datastreamtable[bd.datastream2index].inputbandpols[bd.datastream2bandindex[j][k]] 
					   == polpairs[polproductindex].charAt(1))
						return true;
				}
			}
		}
		return false;
	}
	public boolean productselectionpossible(int ds1index, int ds2index, int freqentry, int polproductindex) {
		DatastreamData dd1 = datastreamtable[ds1index];
		DatastreamData dd2 = datastreamtable[ds2index];
		
		for(int i=0;i<dd1.numfreqs;i++) {
			if(dd1.freqtableindices[i] == freqentry) {
				for(int j=0;j<dd2.numfreqs;j++) {
					if(dd2.freqtableindices[j] == freqentry) {
						for(int k=0;k<dd1.numinputbands;k++) {
							if(dd1.inputbandlocalfreqindices[k] == i && 
							   dd1.inputbandpols[k] == polpairs[polproductindex].charAt(0)) {
								for(int l=0;l<dd2.numinputbands;l++) {
									if(dd2.inputbandlocalfreqindices[l] == j && 
									   dd2.inputbandpols[l] == polpairs[polproductindex].charAt(1))
										return true;
								}
							}
						}
					}
				}
			}
		}
		
		return false;
	}
	public boolean productselectionpossible(int baselineentry, int freqentry, int polproductindex) {
		BaselineData bd = baselinetable[baselineentry];
		
		return productselectionpossible(bd.datastream1index, bd.datastream2index, freqentry, polproductindex);
	}
	/*public String [] getConfigNames() {
		int count = 0;
		String [] toreturn = new String[numactiveconfigs];
		for(int i=0;i<configs.length;i++)
			toreturn[i] = new String(configs[i].sourcename);
		return toreturn;
	}
	public int getConfigIndex(String sourcename) {
		int toreturn = -1;
		
		for(int i=0;i<numconfigs;i++) {
			if(configs[i].sourcename.equals(sourcename))
				toreturn = i;
		}
		return toreturn;
	}*/
	public void setNumNodes(int toset) {
		numnodes = toset; updated = false;
	}
        public void setWalltime(int toset) {
                walltimehours = toset; updated = false;
        }
        public void setProcsPerNode(int toset) {
                procspernode = toset; updated = false;
        }
	public void setThreadFileName(String toset) {
		coreconffilename = toset; updated = false;
	}
	public void setMachineFileName(String toset) {
		machinefilename = toset; updated = false;
	}
	public void setBandEdgeFreq(int freqentry, double toset) {
		freqtable[freqentry].bandedgefreq = toset; updated = false;
	}
	public void setBandwidth(int freqentry, double toset) {
		freqtable[freqentry].bandwidth = toset; updated = false;
	}
	public void setLowerSideband(int freqentry, boolean toset) {
		freqtable[freqentry].lowersideband = toset; updated = false;
	}
	public void setIntTime(int config, double inttime) {
		configs[config].inttime = inttime; updated = false;
	}
	public void setNumChannels(int config, int numchannels) {
		configs[config].numchannels = numchannels; updated = false;
	}
	public void setPulsarBinOn(int config, boolean pulsarbin) {
		configs[config].pulsarbin = pulsarbin; updated = false;
	}
	public void setPulsarFilename(int config, String filename) {
		configs[config].pulsarfilename = filename; updated = false;
	}
	public void setConfigDatastreamIndices(int config, int [] datastreamindices) {
		configs[config].datastreamindices = datastreamindices; updated = false;
	}
	public void setConfigBaselineIndices(int config, int [] baselineindices) {
		configs[config].baselineindices = baselineindices; updated = false;
	}
	public void setBlocksPerSend(int config, int blockspersend) {
		configs[config].blockspersend = blockspersend; updated = false;
	}
	public void setGuardBlocks(int config, int guardblocks) {
		configs[config].guardblocks = guardblocks; updated = false;
	}
	public void setPostFFringe(int config, boolean postf) {
		configs[config].postffringerot = postf; updated = false;
	}
	public void setQuadDelayInterp(int config, boolean quaddelay) {
		configs[config].quadraticdelayinterp = quaddelay; updated = false;
	}
	public void setWriteAutocorrs(int config, boolean writeauto) {
		configs[config].writeautocorrs = writeauto; updated = false;
	}
	public void setConfigFilename(String configfilename) {
		this.configfilename = configfilename; updated = false;
	}
	public void setPolarisations(int datastreamindex, char [] polarisations) {
		datastreamtable[datastreamindex].inputbandpols = polarisations;
		if(polarisations[0] == 'X' || polarisations[0] == 'Y' || 
				polarisations[0] == 'x' || polarisations[0] == 'y')
			polpairs = new String[] {"XX", "YY", "XY", "YX"};
		else
			polpairs = new String[] {"RR", "LL", "RL", "LR"};
	}
	void setTsys(int datastreamindex, double tsys) {
		datastreamtable[datastreamindex].tsys = tsys;
	}
	void setDataFormatIndex(int datastreamindex, int formatindex) {
		datastreamtable[datastreamindex].format = formatindex;
	}
	void setFanout(int datastreamindex, int fanout) {
		datastreamtable[datastreamindex].fanout = fanout;
	}
	void setDatastreamFreqClockOffset(int datastreamindex, int freq, double offset) {
		datastreamtable[datastreamindex].freqclockoffsets[freq] = offset;
	}
	void setNumInputBands(int datastreamindex, int numbands) {
		datastreamtable[datastreamindex].numinputbands = numbands;
	}
	void setNetworkDatastream(int datastreamindex, boolean network) {
		datastreamtable[datastreamindex].readfromfile = !network;
	}
	void setDataFilenames(int datastreamindex, String [] filenames) {
		System.out.println("About to set filenames for " + datastreamindex + ", length " + filenames.length);
		datastreamtable[datastreamindex].numdatafiles = (filenames == null)?0:filenames.length;
		datastreamtable[datastreamindex].datafilenames = filenames;
	}
	void setPort(int datastreamindex, int port) {
		datastreamtable[datastreamindex].portnumber = port;
	}
	void setTCPWindowSize(int datastreamindex, int windowsize) {
		datastreamtable[datastreamindex].tcpwindowsizekb = windowsize;
	}
	public void setFreqTableLength(int newlength) {
		updated = false;
		int smaller = (newlength < freqtablelength)?newlength:freqtablelength;
		FreqData [] oldfreqtable = freqtable;
		freqtablelength = newlength;
		freqtable = new FreqData[newlength];
		for(int i=0;i<smaller;i++) {
			freqtable[i].bandedgefreq = oldfreqtable[i].bandedgefreq;
			freqtable[i].bandwidth = oldfreqtable[i].bandwidth;
			freqtable[i].lowersideband = oldfreqtable[i].lowersideband;
		}
		for(int i=0;i<datastreamtablelength;i++)
			datastreamtable[i].freqtable = freqtable;
		for(int i=0;i<baselinetablelength;i++)
			baselinetable[i].freqtable = freqtable;
	}
	public void setConfigStatus(int index, boolean active) {
		updated = false;
		if(active) {
			configs[index] = new ConfigData(configs[index].sourcename, true);
			configs[index].datastreamindices = new int[numdatastreams];
			configs[index].ordereddatastreamindices = new int[numdatastreams];
			configs[index].baselineindices = new int[numbaselines];
		}
		configs[index].active = active;
		numactiveconfigs = 0;
		for(int i=0;i<configs.length;i++) {
			if(configs[i].active)
				numactiveconfigs++;
		}
	}
	public void setFreqIndices(int datastreamindex, int []freqindices) {
		int [] uniqueindices = new int[freqindices.length];
		int [] localfreqindices = new int[freqindices.length];
		int [] numpols = new int[freqindices.length];
		int numfreqs = 0;
		for(int i=0;i<freqindices.length;i++) {
			localfreqindices[i] = -1;
			for(int j=0;j<numfreqs;j++) {
				if(freqindices[i] == uniqueindices[j])
					localfreqindices[i] = j;
			}
			if(localfreqindices[i] < 0) {
				uniqueindices[numfreqs++] = freqindices[i];
				localfreqindices[i] = numfreqs - 1;
			}
			numpols[localfreqindices[i]]++;
		}
		datastreamtable[datastreamindex].numfreqs = numfreqs;
		datastreamtable[datastreamindex].inputbandlocalfreqindices = localfreqindices;
		datastreamtable[datastreamindex].freqtableindices = new int[numfreqs];
		datastreamtable[datastreamindex].freqpols = new int[numfreqs];
		double [] oldoffsets = datastreamtable[datastreamindex].freqclockoffsets;
		datastreamtable[datastreamindex].freqclockoffsets = new double[numfreqs];
		for(int i=0;i<numfreqs;i++) {
			datastreamtable[datastreamindex].freqtableindices[i] = uniqueindices[i];
			datastreamtable[datastreamindex].freqpols[i] = numpols[i];
			if(i<oldoffsets.length)
				datastreamtable[datastreamindex].freqclockoffsets[i] = oldoffsets[i];
			else
				datastreamtable[datastreamindex].freqclockoffsets[i] = 0.0;
		}
	}
	
	public void reset() {
		baselinetablelength = telescopetablelength = datastreamtablelength = freqtablelength = 0;
		numbaselines = numdatastreams = defaultconfigindex = numconfigsloaded = numsources = 0;
		executeseconds = startseconds = 0;
		numactiveconfigs = 1; //default
		startmjd = 54000;
		databufferfactor = 32;
		numdatasegments = 8;
		delayfilename = uvwfilename = coreconffilename = outputfilename = outputformatstring = "";
		commonread = datastreamread = configread = dataoverride = updated = false;
		configs = new ConfigData[1];
		configs[0] = new ConfigData("DEFAULT        ", true);
		freqtable = new FreqData[0];
		datastreamtable = new DatastreamData[0];
		telescopetable = new TelescopeData[0];
		baselinetable = new BaselineData[0];
		sourcenames = new String[0];
		usepbs = true;
		polpairs = new String [] {"RR", "LL", "RL", "LR"};
	}
	
	public void addNewFreq() {
		FreqData [] oldfreqtable = freqtable;
		
		freqtablelength++;
		freqtable = new FreqData[freqtablelength];
		for(int i=0;i<freqtablelength-1;i++) {
			freqtable[i] = oldfreqtable[i];
		}
		freqtable[freqtablelength -1] = new FreqData();
		updated = false;
		for(int i=0;i<datastreamtablelength;i++)
			datastreamtable[i].freqtable = freqtable;
		for(int i=0;i<baselinetablelength;i++)
			baselinetable[i].freqtable = freqtable;
	}
	
	public void addNewDatastream(int telescopeindex) {
		DatastreamData [] olddstable = datastreamtable;
		datastreamtable = new DatastreamData[++datastreamtablelength];
		for(int i=0;i<datastreamtablelength-1;i++) {
			datastreamtable[i]  = olddstable[i];
		}
		datastreamtable[datastreamtablelength-1] = new DatastreamData(telescopetable, freqtable);
		datastreamtable[datastreamtablelength-1].telescopeindex = telescopeindex;
		for(int i=0;i<baselinetablelength;i++)
			baselinetable[i].datastreamtable = datastreamtable;
	}
	
	public void copyDatastreamFreqSetup(int sourceindex, int targetindex) {
		datastreamtable[targetindex].format = datastreamtable[sourceindex].format;
		datastreamtable[targetindex].numinputbands = datastreamtable[sourceindex].numinputbands;
		datastreamtable[targetindex].numfreqs = datastreamtable[sourceindex].numfreqs;
		datastreamtable[targetindex].readfromfile = datastreamtable[sourceindex].readfromfile;
		datastreamtable[targetindex].portnumber = datastreamtable[sourceindex].portnumber;
		datastreamtable[targetindex].tcpwindowsizekb = datastreamtable[sourceindex].tcpwindowsizekb;
		datastreamtable[targetindex].freqpols = new int[datastreamtable[sourceindex].freqpols.length];
		for(int i=0;i<datastreamtable[sourceindex].freqpols.length;i++)
			datastreamtable[targetindex].freqpols[i] = datastreamtable[sourceindex].freqpols[i];
		datastreamtable[targetindex].freqtableindices = new int[datastreamtable[sourceindex].freqtableindices.length];
		for(int i=0;i<datastreamtable[sourceindex].freqtableindices.length;i++)
			datastreamtable[targetindex].freqtableindices[i] = datastreamtable[sourceindex].freqtableindices[i];
		datastreamtable[targetindex].inputbandlocalfreqindices = new int[datastreamtable[sourceindex].inputbandlocalfreqindices.length];
		for(int i=0;i<datastreamtable[sourceindex].inputbandlocalfreqindices.length;i++)
			datastreamtable[targetindex].inputbandlocalfreqindices[i] = datastreamtable[sourceindex].inputbandlocalfreqindices[i];
		datastreamtable[targetindex].inputbandpols = new char[datastreamtable[sourceindex].inputbandpols.length];
		for(int i=0;i<datastreamtable[sourceindex].inputbandpols.length;i++)
			datastreamtable[targetindex].inputbandpols[i] = datastreamtable[sourceindex].inputbandpols[i];
	}
	
	public void setBaseline(int tableindex, int datastream1index, int datastream2index, boolean [][] products) {
		int [] nproducts = new int[products.length];
		int nfreqs = 0;
		int count = 0;
		int ds1bandindex, ds2bandindex, pcount;
		DatastreamData ds1, ds2;
		
		baselinetable[tableindex] = new BaselineData(telescopetable, freqtable, datastreamtable);
		ds1 = datastreamtable[datastream1index];
		ds2 = datastreamtable[datastream2index];
		baselinetable[tableindex].datastream1index = datastream1index;
		baselinetable[tableindex].datastream2index = datastream2index;
		for(int i=0;i<products.length;i++) {
			for(int j=0;j<4;j++) {
				if(products[i][j])
					nproducts[i]++;
			}
			if(nproducts[i]>0)
				nfreqs++;
		}
		baselinetable[tableindex].numfreqs = nfreqs;
		baselinetable[tableindex].numpolproducts = new int[nfreqs];
		baselinetable[tableindex].datastream1bandindex = new int[nfreqs][];
		baselinetable[tableindex].datastream2bandindex = new int[nfreqs][];
		for(int i=0;i<products.length;i++) {
			if(nproducts[i] > 0) {
				pcount = 0;
				baselinetable[tableindex].numpolproducts[count] = nproducts[i];
				baselinetable[tableindex].datastream1bandindex[count] = new int[nproducts[i]];
				baselinetable[tableindex].datastream2bandindex[count] = new int[nproducts[i]];
				for(int j=0;j<4;j++) {
					if(products[i][j]) {
						ds1bandindex = -1;
						ds2bandindex = -1;
						for(int k=0;k<ds1.numinputbands;k++) {
							if(ds1.freqtableindices[ds1.inputbandlocalfreqindices[k]] == i
								&& polpairs[j].charAt(0) == ds1.inputbandpols[k]) 
								ds1bandindex = k;
						}
						for(int k=0;k<ds2.numinputbands;k++) {
							if(ds2.freqtableindices[ds2.inputbandlocalfreqindices[k]] == i
								&& polpairs[j].charAt(1) == ds2.inputbandpols[k]) 
								ds2bandindex = k;
						}
						if(ds1bandindex < 0 || ds2bandindex < 0)
							System.err.println("Something has gone badly awry - I can't find the datastream bands for baseline " +
									tableindex + "'s " + i + "'th frequency, " + j + "'th band!!!");
						baselinetable[tableindex].datastream1bandindex[count][pcount] = ds1bandindex;
						baselinetable[tableindex].datastream2bandindex[count][pcount] = ds2bandindex;
						pcount++;
					}
				}
				count++;
			}
		}
	}
	
	public void createNewBaselineTable(int length) {
		baselinetablelength = length;
		baselinetable = new BaselineData[length];
	}
	
	public String removeFrequency(int toremove) {
		updated = false;
		String toreturn = null;
		FreqData [] oldfreqtable = freqtable;
		freqtablelength--;
		
		//chop out the unwanted entry
		freqtable = new FreqData[freqtablelength];
		for(int i=0;i<freqtablelength+1;i++) {
			if(i < toremove)
				freqtable[i] = oldfreqtable[i];
			else if (i > toremove) 
				freqtable[i-1] = oldfreqtable[i];
		}
		
		//remove anything in the freq/baseline tables that refers to this...
		for(int i=0;i<datastreamtablelength;i++) {
			for(int j=0;j<datastreamtable[i].numfreqs;j++) {
				if(datastreamtable[i].freqtableindices[j] == toremove) {
					datastreamtable[i].freqtableindices[j] = -1;
					if(toreturn == null)
						toreturn = "";
					toreturn += telescopetable[datastreamtable[i].telescopeindex].name + ": index " + j;
				}
			}
		}
		
		for(int i=0;i<datastreamtablelength;i++)
			datastreamtable[i].freqtable = freqtable;
		for(int i=0;i<baselinetablelength;i++)
			baselinetable[i].freqtable = freqtable;
		
		return toreturn;
	}
	
	public void removeDatastream(int datastreamindex) {
		int numremoved = 0;
		boolean [] remove = new boolean[baselinetablelength];
		int [] cumulativeremoved = new int[baselinetablelength];
		DatastreamData [] olddstable = datastreamtable;
		BaselineData [] oldbaselinetable = baselinetable;
		datastreamtable = new DatastreamData[datastreamtablelength - 1];
		for(int i=0;i<datastreamtablelength-1;i++)
			datastreamtable[i] = (i<datastreamindex)?olddstable[i]:olddstable[i+1];
		datastreamtablelength--;
		for(int i=0;i<baselinetablelength;i++) {
			if(baselinetable[i].datastream1index == datastreamindex || 
					baselinetable[i].datastream2index == datastreamindex) {
				remove[i] = true;
				numremoved++;
			}
			else {
				remove[i] = false;
				if(baselinetable[i].datastream1index > datastreamindex)
					baselinetable[i].datastream1index--;
				if(baselinetable[i].datastream2index > datastreamindex)
					baselinetable[i].datastream2index--;
			}
			cumulativeremoved[i] = numremoved;
		}
		baselinetable = new BaselineData[baselinetablelength-numremoved];
		for(int i=0;i<numsources;i++) {
			if(configs[i].active) {
				for(int j=0;j<numbaselines;j++) {
					if(remove[configs[i].baselineindices[j]])
						configs[i].baselineindices[j] = -1;
					else
						configs[i].baselineindices[j] -= cumulativeremoved[j];
				}
				for(int j=0;j<numdatastreams;j++) {
					if(configs[i].datastreamindices[j] == datastreamindex)
						configs[i].datastreamindices[j] = -1;
					else if (configs[i].datastreamindices[j] > datastreamindex)
						configs[i].datastreamindices[j] -= 1;
				}
			}
		}
		numremoved = 0;
		int i=0;
		while(i<baselinetablelength) {
			while(i<baselinetablelength && remove[i])
				i++;
			if(i<baselinetablelength) {
				baselinetable[i-numremoved] = oldbaselinetable[i];
				baselinetable[i-numremoved].datastreamtable = datastreamtable;
			}
			i++;
		}
		baselinetablelength -= numremoved;
	}
	
	public void setCommonSettings(String delayfilename, String uvwfilename, 
			                          int startmjd, int startseconds, int executeseconds,
			                          int numdatastreams, int numbaselines, int numdatasegments,
			                          int databufferfactor, boolean dataoverride,  
			                          String outputformat, String outputfilename) {
		boolean deepcheck = !delayfilename.equals(this.delayfilename);
		updated = false;
		this.delayfilename = delayfilename;
		this.uvwfilename = uvwfilename;
		this.startmjd = startmjd;
		this.startseconds = startseconds;
		this.executeseconds = executeseconds;
		this.numdatastreams = numdatastreams;
		this.numbaselines = numbaselines;
		this.numdatasegments = numdatasegments;
		this.databufferfactor = databufferfactor;
		this.dataoverride = dataoverride;
		this.outputformatstring = outputformat;
		this.outputfilename = outputfilename;
		try {
			if(deepcheck)
				loadSourceNames();
		}
		catch(Exception e) {}
	}
	
	public void setNodeSettings(boolean usepbs, String clustername, String machinefilename, 
			String threadfilename, int numnodes) {
		updated = false;
		this.usepbs = usepbs;
		this.clustername = clustername;
		this.machinefilename = machinefilename;
		this.coreconffilename = threadfilename;
		this.numnodes = numnodes;
	}
	
	public void setTelescopeTableData(String [][] data) {
		updated = false;
		if(data != null) {
			TelescopeData [] oldtelescopetable = telescopetable;
			telescopetable = new TelescopeData[data.length];
			for(int i=0;i<telescopetablelength;i++) {
				System.out.println("Saving data " + data[i][0] + " " + data[i][1] + " " + data[i][2]);
				telescopetable[i] = oldtelescopetable[i];
				telescopetable[i].name = data[i][0];
				telescopetable[i].clockdelay = Double.parseDouble(data[i][1]);
				telescopetable[i].clockrate = Double.parseDouble(data[i][2]);
			}
			for(int i=telescopetablelength;i<data.length;i++) {
				telescopetable[i] = new TelescopeData();
				telescopetable[i].name = data[i][0];
				telescopetable[i].clockdelay = Double.parseDouble(data[i][1]);
				telescopetable[i].clockrate = Double.parseDouble(data[i][2]);
			}
			telescopetablelength = telescopetable.length;
		}
	}
	
	public void removeTelescope(String telescopename) {
		BaselineData [] oldbaselinetable;
		DatastreamData [] olddatastreamtable;
		int telescopeid = -1;
		int removecount = 0;
		boolean [] removedatastream = new boolean[datastreamtable.length];
		updated = false;
		
		for(int i=0;i<telescopetablelength;i++) {
			if(telescopetable[i].name.trim().equals(telescopename.trim()))
				telescopeid = i;
		}
		if(telescopeid >= 0) {
			for(int i=0;i<datastreamtablelength;i++) {
				if(datastreamtable[i].telescopeindex == telescopeid)
					removedatastream[i] = true;
				else
					removedatastream[i] = false;
			}
			for(int i=0-removecount;i<baselinetablelength-removecount;i++) {
				if(removedatastream[baselinetable[i].datastream1index] ||
				   removedatastream[baselinetable[i].datastream2index] ) {
					//for(int j=i+i;j<baselinetablelength-removecount;j++)
					//	baselinetable[j-1] = baselinetable[j];
					removecount++;
				}
			}
			oldbaselinetable = baselinetable;
			baselinetable = new BaselineData[baselinetablelength-removecount];
			removecount = 0;
			for(int i=0;i<baselinetablelength;i++) {
				if(removedatastream[baselinetable[i].datastream1index] ||
				   removedatastream[baselinetable[i].datastream2index] )
					baselinetable[i-removecount++] = oldbaselinetable[i];
			}
			baselinetablelength = baselinetable.length;
			
			removecount=0;
			for(int i=0;i<datastreamtablelength;i++) {
				if(removedatastream[i])
					removecount++;
			}
			olddatastreamtable = datastreamtable;
			datastreamtable = new DatastreamData[datastreamtablelength - removecount];
			int count = 0;
			for(int i=0;i<datastreamtablelength;i++) {
				if(!removedatastream[i])
					datastreamtable[count++] = olddatastreamtable[i];
			}
			for(int i=0;i<baselinetablelength;i++)
				baselinetable[i].datastreamtable = datastreamtable;
			datastreamtablelength = datastreamtable.length;
		}
	}
	
	public void trimDataFilesBySource() {
		FileTrimmer ft = new FileTrimmer(uvwfilename, configs);
		for(int i=0;i<datastreamtablelength;i++) {
			datastreamtable[i].datafilenames = ft.trimDataFiles(datastreamtable[i].datafilenames);
			datastreamtable[i].numdatafiles = datastreamtable[i].datafilenames.length;
		}
	}

        public void trimDataFilesByTimerange() {
		FileTrimmer ft = new FileTrimmer(startmjd, startseconds, executeseconds);
		for(int i=0;i<datastreamtablelength;i++) {
			datastreamtable[i].datafilenames = ft.trimDataFiles(datastreamtable[i].datafilenames);
			datastreamtable[i].numdatafiles = datastreamtable[i].datafilenames.length;
		}
	}

        public void trimDataFilesByMode(int numscans, int [] starts, int [] stops) {
	    FileTrimmer ft = new FileTrimmer(startmjd, startseconds, starts, stops, numscans);
	    for(int i=0;i<datastreamtablelength;i++) {
		datastreamtable[i].datafilenames = ft.trimDataFiles(datastreamtable[i].datafilenames);
		datastreamtable[i].numdatafiles = datastreamtable[i].datafilenames.length;
	    }
	}
	
	public String getCommonSummary() {
		return "Config Files:       " + delayfilename + "\n                    " + uvwfilename + 
		"\n                    " + coreconffilename + "\nEXECUTE TIME (SEC): " + executeseconds
		+ "\nSTART MJD:          " + startmjd + "\nSTART SECONDS:      " + startseconds
		+ "\nACTIVE DATASTREAMS: " + numdatastreams + "\nACTIVE BASELINES:   " + 
		numbaselines + "\nOUTPUT FORMAT:      " + outputformatstring + "\nOUTPUT FILENAME:    " + 
		outputfilename;
	}
	
	public String getConfigSummary() {
		String toreturn = "";
		
		for(int i=0;i<configs.length;i++) {
			if(configs[i].active)
				toreturn += configs[i].toString();
		}
		
		return toreturn;
	}
	
	public String getTelescopeSummary() {
		String toreturn = "";
		
		for(int i=0;i<telescopetablelength;i++) {
			toreturn += telescopetable[i].toString();
		}
		
		return toreturn;
	}
	
	public String getDatastreamSummary() {
		String toreturn = "";
		
		for(int i=0;i<datastreamtablelength;i++) {
			toreturn += datastreamtable[i].toString();
		}
		
		return toreturn;
	}
	
	public String getBaselineSummary() {
		String toreturn = "";
		
		for(int i=0;i<baselinetablelength;i++) {
			toreturn += baselinetable[i].toString();
		}
		
		return toreturn;
	}
	
	public void loadCorrConfigFile(String filename) {
		try {
			BufferedReader input = new BufferedReader(new FileReader(filename));
			int currentheader = getSectionHeader(input);
			commonread = false;
			datastreamread = false;
			configread = false;
			updated = false;
			
			while(currentheader != SectionHeader.INPUT_EOF) {
				switch(currentheader)
				{
					case SectionHeader.COMMON:
						System.out.println("Processing common section");
						processCommon(input);
						break;
					case SectionHeader.CONFIG:
						if(!commonread)
						{
							System.out.println("Error - input file out of order!  Attempted to read configuration details without knowledge of common settings - aborting!!!");
							return; //note aborting here
						}
						processConfig(input);
						break;
					case SectionHeader.FREQ:
						processFreqTable(input);
						break;
					case SectionHeader.TELESCOPE:
						processTelescopeTable(input);
						break;
					case SectionHeader.DATASTREAM:
						if(!commonread)
						{
							System.out.println("Error - input file out of order!  Attempted to read configuration details without knowledge of common settings - aborting!!!");
							return; //note aborting here
						}
						processDatastreamTable(input);
						break;
					case SectionHeader.BASELINE:
						if(!commonread)
						{
							System.out.println("Error - input file out of order!  Attempted to read configuration details without knowledge of common settings - aborting!!!");
							return; //note aborting here
						}
						processBaselineTable(input);
						break;
					case SectionHeader.DATA:
						if(!datastreamread)
						{
							System.out.println("Error - input file out of order!  Attempted to read datastream data files without knowledge of datastreams - aborting!!!");
							return; //note aborting here
						}
						processDataTable(input);
						break;
					case SectionHeader.NETWORK:
						if(!datastreamread)
						{
							System.out.println("Error - input file out of order!  Attempted to read datastream network details without knowledge of datastreams - aborting!!!");
							return;
						}
						processNetworkTable(input);
						break;
				}
				currentheader = getSectionHeader(input);
			}
			if(!configread) {
				System.out.println("Error - no config section in input file - aborting!!!");
				return;
			}
			input.close();
			consistencyCheck(false);
			configfilename = filename;
			updated = true;
			System.out.println("Finished processing input file!!!");
		}
		catch (IOException e) {
			JOptionPane.showMessageDialog(mainwindow, "Error processing file " + filename + ": " + e.getMessage(),
                    "File parsing error!!", JOptionPane.ERROR_MESSAGE);
		}
		catch (ConsistencyException e) {
			JOptionPane.showMessageDialog(mainwindow, "Inconsistent data found in file " + filename + ": " + e.getMessage(),
					"Configuration consistency error!!", JOptionPane.ERROR_MESSAGE);
		}
		numconfigsloaded++;
	}
	
	public void saveCorrConfigFile() {
		saveCorrConfigFile(configfilename);
	}
	
	public void saveCorrConfigFile(String filename) {
		try {
			consistencyCheck(true);
			configfilename = filename;
			PrintWriter output = new PrintWriter(new BufferedWriter(new FileWriter(filename)));
			writeCommonTable(output);
			writeConfigTable(output);
			writeFreqTable(output);
			writeTelescopeTable(output);
			writeDatastreamTable(output);
			writeBaselineTable(output);
			writeSourceDataTable(output);
			output.close();
			updated = true;
		}
		catch(IOException e) {
			JOptionPane.showMessageDialog(mainwindow, "Error trying to write file " + filename + ": " + e.getMessage(),
                    "File writing error!!", JOptionPane.ERROR_MESSAGE);
		}
		catch (ConsistencyException e) {
			JOptionPane.showMessageDialog(mainwindow, "Inconsistent data found in configuration: " + e.getMessage(),
					"Configuration consistency error!!", JOptionPane.ERROR_MESSAGE);
		}
	}
	
	//methods for writing out to a config file
	private void writeCommonTable(PrintWriter output) {
		output.println("# COMMON SETTINGS ##!");
		writeoutputline(output, delayfilename, "DELAY FILENAME");
		writeoutputline(output, uvwfilename, "UVW FILENAME");
		writeoutputline(output, coreconffilename, "CORE CONF FILENAME");
		writeoutputline(output, executeseconds, "EXECUTE TIME (SEC)");
		writeoutputline(output, startmjd, "START MJD");
		writeoutputline(output, startseconds, "START SECONDS");
		writeoutputline(output, numdatastreams, "ACTIVE DATASTREAMS");
		writeoutputline(output, numbaselines, "ACTIVE BASELINES");
		writeoutputline(output, dataoverride, "DATA HEADER O/RIDE");
		writeoutputline(output, outputformatstring, "OUTPUT FORMAT");
		writeoutputline(output, outputfilename, "OUTPUT FILENAME");
		output.println();
	}
	
	private void writeConfigTable(PrintWriter output) {
		output.println("# CONFIGURATIONS ###!");
		writeoutputline(output, numactiveconfigs, "NUM CONFIGURATIONS");
		for(int i=0;i<configs.length;i++) {
			if(configs[i].active) {
				writeoutputline(output, configs[i].sourcename.trim(), "CONFIG SOURCE");
				writeoutputline(output, configs[i].inttime, "INT TIME (SEC)");
				writeoutputline(output, configs[i].numchannels ,"NUM CHANNELS");
				writeoutputline(output, configs[i].blockspersend ,"BLOCKS PER SEND");
				writeoutputline(output, configs[i].guardblocks ,"GUARD BLOCKS");
				writeoutputline(output, configs[i].postffringerot ,"POST-F FRINGE ROT");
				writeoutputline(output, configs[i].quadraticdelayinterp ,"QUAD DELAY INTERP");
				writeoutputline(output, configs[i].writeautocorrs ,"WRITE AUTOCORRS");
				writeoutputline(output, configs[i].pulsarbin ,"PULSAR BINNING");
				if(configs[i].pulsarbin)
					writeoutputline(output, configs[i].pulsarfilename, "PULSAR CONFIG FILE");
				for(int j=0;j<numdatastreams;j++)
					writeoutputline(output, configs[i].datastreamindices[j] ,"DATASTREAM " + j + " INDEX");
				for(int j=0;j<numbaselines;j++)
					writeoutputline(output, configs[i].baselineindices[j] ,"BASELINE " + j + " INDEX");
			}
		}
		output.println();
	}
	
	private void writeFreqTable(PrintWriter output) {
		output.println("# FREQ TABLE #######!");
		writeoutputline(output, freqtablelength, "FREQ ENTRIES");
		for(int i=0;i<freqtablelength;i++) {
			writeoutputline(output, freqtable[i].bandedgefreq, "FREQ (MHZ) " + i);
			writeoutputline(output, freqtable[i].bandwidth, "BW (MHZ) " + i);
			writeoutputline(output, ((freqtable[i].lowersideband)?"L":"U"), "SIDEBAND " + i);
		}
		output.println();
	}
	
	private void writeTelescopeTable(PrintWriter output) {
		output.println("# TELESCOPE TABLE ##!");
		writeoutputline(output, telescopetablelength, "TELESCOPE ENTRIES");
		for(int i=0;i<telescopetablelength;i++) {
			writeoutputline(output, telescopetable[i].name, "TELESCOPE NAME " + i);
			writeoutputline(output, telescopetable[i].clockdelay, "CLOCK DELAY (us) " + i);
			writeoutputline(output, telescopetable[i].clockrate, "CLOCK RATE(us/s) " + i);
		}
		output.println();
	}
	
	private void writeDatastreamTable(PrintWriter output) {
		output.println("# DATASTREAM TABLE #!");
		writeoutputline(output, datastreamtablelength, "DATASTREAM ENTRIES");
		writeoutputline(output, databufferfactor, "DATA BUFFER FACTOR");
		writeoutputline(output, numdatasegments, "NUM DATA SEGMENTS");
		for(int i=0;i<datastreamtablelength;i++) {
			writeoutputline(output, datastreamtable[i].telescopeindex, "TELESCOPE INDEX");
			writeoutputline(output, datastreamtable[i].tsys, "TSYS");
			switch(datastreamtable[i].format) {
			case DataFormat.LBASTD:
				writeoutputline(output, "LBASTD", "DATA FORMAT");
				break;
			case DataFormat.LBAVSOP:
				writeoutputline(output, "LBAVSOP", "DATA FORMAT");
				break;
			case DataFormat.MKV:
			case DataFormat.MKV_MKIV:
			case DataFormat.MKV_VLBA:
				writeoutputline(output, "MKV", "DATA FORMAT");
				writeoutputline(output, datastreamtable[i].fanout, "FANOUT");
				break;
			case DataFormat.K5:
				writeoutputline(output, "K5", "DATA FORMAT");
				break;
			case DataFormat.NZ:
				writeoutputline(output, "NZ", "DATA FORMAT");
				break;
			}
			writeoutputline(output, datastreamtable[i].numbits, "QUANTISATION BITS");
			writeoutputline(output, datastreamtable[i].filterbank, "FILTERBANK USED");
			writeoutputline(output, datastreamtable[i].readfromfile, "READ FROM FILE");
			writeoutputline(output, datastreamtable[i].numfreqs, "NUM FREQS");
			for(int j=0;j<datastreamtable[i].numfreqs;j++) {
				writeoutputline(output, datastreamtable[i].freqtableindices[j], "FREQ TABLE INDEX " + j);
				writeoutputline(output, datastreamtable[i].freqclockoffsets[j], "CLK OFFSET " + j + " (us)");
				writeoutputline(output, datastreamtable[i].freqpols[j], "NUM POLS " + j);
			}
			for(int j=0;j<datastreamtable[i].numinputbands;j++) {
				writeoutputline(output, datastreamtable[i].inputbandpols[j], "INPUT BAND " + j + " POL");
				writeoutputline(output, datastreamtable[i].inputbandlocalfreqindices[j], "INPUT BAND " + j + " INDEX");
			}
		}
		output.println();
	}
	
	private void writeBaselineTable(PrintWriter output) {
		output.println("# BASELINE TABLE ###!");
		writeoutputline(output, baselinetablelength, "BASELINE ENTRIES");
		for(int i=0;i<baselinetablelength;i++) {
			writeoutputline(output, baselinetable[i].datastream1index, "D/STREAM A INDEX " + i);
			writeoutputline(output, baselinetable[i].datastream2index, "D/STREAM B INDEX " + i);
			writeoutputline(output, baselinetable[i].numfreqs, "NUM FREQS " + i);
			for(int j=0;j<baselinetable[i].numfreqs;j++) {
				writeoutputline(output, baselinetable[i].numpolproducts[j], "POL PRODUCTS " + i + "/" + j);
				for(int k=0;k<baselinetable[i].numpolproducts[j];k++) {
					writeoutputline(output, baselinetable[i].datastream1bandindex[j][k], "D/STREAM A BAND " + k);
					writeoutputline(output, baselinetable[i].datastream2bandindex[j][k], "D/STREAM B BAND " + k);
				}
			}
		}
		output.println();
	}
	
	private void writeSourceDataTable(PrintWriter output) {
		boolean dodatatable = false;
		boolean donetworktable = false;
		for(int i=0;i<datastreamtablelength;i++) {
			dodatatable = dodatatable || datastreamtable[i].readfromfile;
			donetworktable = donetworktable || !datastreamtable[i].readfromfile;
		}
		
		if(dodatatable) {
			output.println("# DATA TABLE #######!");
			for(int i=0;i<datastreamtablelength;i++) {
				writeoutputline(output, datastreamtable[i].numdatafiles, "D/STREAM " + i + " FILES");
				for(int j=0;j<datastreamtable[i].numdatafiles;j++)
					writeoutputline(output, datastreamtable[i].datafilenames[j], "FILE " + i + "/" + j);
			}
			output.println();
		}
		
		if(donetworktable) {
			output.println("# NETWORK TABLE ####!");
			for(int i=0;i<datastreamtablelength;i++) {
				writeoutputline(output, datastreamtable[i].portnumber, "PORT NUM");
				writeoutputline(output, datastreamtable[i].tcpwindowsizekb, "TCP WINDOW SIZE");
			}
			output.println();
		}
	}
	
	//methods for reading in a config file
	private int getSectionHeader(BufferedReader input) throws IOException
	{
	  String line = "";

	  try {
		  while (line != null && line.equals(""))
			  line = input.readLine(); //skip the whitespace
		  if(line == null)
			  return SectionHeader.INPUT_EOF;

		  //return the type of section this is
		  if(line.startsWith("# COMMON SETTINGS"))
			  return SectionHeader.COMMON;
		  if(line.startsWith("# CONFIGURATIONS"))
			  return SectionHeader.CONFIG;
		  if(line.startsWith("# FREQ TABLE"))
			  return SectionHeader.FREQ;
		  if(line.startsWith("# TELESCOPE TABLE"))
			  return SectionHeader.TELESCOPE;
		  if(line.startsWith("# DATASTREAM TABLE"))
			  return SectionHeader.DATASTREAM;
		  if(line.startsWith("# BASELINE TABLE"))
			  return SectionHeader.BASELINE;
		  if(line.startsWith("# DATA TABLE"))
			  return SectionHeader.DATA;
		  if(line.startsWith("# NETWORK TABLE"))
			  return SectionHeader.NETWORK;
		  return SectionHeader.UNKNOWN;
	  }
	  catch(EOFException e) {
		  return SectionHeader.INPUT_EOF;
	  }
	}
	
	private void processCommon(BufferedReader input) throws IOException, ConsistencyException {
		String line;

		delayfilename = getinputline(input, "DELAY FILENAME");
		uvwfilename = getinputline(input, "UVW FILENAME");
		coreconffilename = getinputline(input, "CORE CONF FILENAME");
		line = getinputline(input, "EXECUTE TIME (SEC)");
		executeseconds = Integer.valueOf(line);
		line = getinputline(input, "START MJD");
		startmjd = Integer.valueOf(line);
		line = getinputline(input, "START SECONDS");
		startseconds = Integer.valueOf(line);
		line = getinputline(input, "ACTIVE DATASTREAMS");
		numdatastreams = Integer.valueOf(line);
		line = getinputline(input, "ACTIVE BASELINES");
		numbaselines = Integer.valueOf(line);
		line = getinputline(input, "DATA HEADER O/RIDE");
		dataoverride = (line.equalsIgnoreCase("TRUE") || line.equalsIgnoreCase("T"))?true:false;
		outputformatstring = getinputline(input, "OUTPUT FORMAT");
	  outputfilename = getinputline(input, "OUTPUT FILENAME");

		loadSourceNames();
		commonread = true;
	}
	
	private void processConfig(BufferedReader input) throws IOException {
		String line;
		int configindex;

		line = getinputline(input, "NUM CONFIGURATIONS");
		numactiveconfigs = Integer.valueOf(line);
		defaultconfigindex = -1;
		for(int i=0;i<numactiveconfigs;i++)
		{
		  line = getinputline(input, "CONFIG SOURCE");
		  if(line.trim().equals("DEFAULT"))
		  	defaultconfigindex = i;
		  configindex = -1;
		  for(int j=0;j<configs.length;j++) {
		  	System.out.println("Comparing " + configs[j].sourcename.trim() + " with " + line.trim());
			  if(configs[j].sourcename.trim().equals(line.trim())) {
				  configindex = j;
				  System.out.println("Found a match!!!");
			  }
		  }
		  if(configindex >= 0) {
			  configs[configindex].active = true;
			  configs[configindex].datastreamindices = new int[numdatastreams];
			  configs[configindex].ordereddatastreamindices = new int[numdatastreams];
			  configs[configindex].baselineindices = new int [numbaselines];
			  line = getinputline(input, "INT TIME (SEC)");
			  configs[configindex].inttime = Double.valueOf(line);
			  line = getinputline(input, "NUM CHANNELS");
			  configs[configindex].numchannels = Integer.valueOf(line);
			  line = getinputline(input, "BLOCKS PER SEND");
			  configs[configindex].blockspersend = Integer.valueOf(line);
			  line = getinputline(input, "GUARD BLOCKS");
			  configs[configindex].guardblocks = Integer.valueOf(line);
			  line = getinputline(input, "POST-F FRINGE ROT");
			  configs[configindex].postffringerot = (line.equalsIgnoreCase("TRUE") || line.equalsIgnoreCase("T"))?true:false;
			  line = getinputline(input, "QUAD DELAY INTERP");
			  configs[configindex].quadraticdelayinterp = (line.equalsIgnoreCase("TRUE") || line.equalsIgnoreCase("T"))?true:false;
			  if(configs[configindex].postffringerot && configs[configindex].quadraticdelayinterp)
			  {
			  	System.err.println("ERROR - cannot quad interpolate delays with post-f fringe rotation - changing to pre-f!!!");
			  	configs[configindex].postffringerot = false;
			  }
			  line = getinputline(input, "WRITE AUTOCORRS");
			  configs[configindex].writeautocorrs = (line.equalsIgnoreCase("TRUE") || line.equalsIgnoreCase("T"))?true:false;
			  line = getinputline(input, "PULSAR BINNING");
			  configs[configindex].pulsarbin = (line.equalsIgnoreCase("TRUE") || line.equalsIgnoreCase("T"))?true:false;
			  if(configs[configindex].pulsarbin)
			  {
			  	configs[configindex].pulsarfilename = getinputline(input, "PULSAR CONFIG FILE");
			  }
			  for(int j=0;j<numdatastreams;j++)
			  {
			  	line = getinputline(input, "DATASTREAM");
			  	configs[configindex].datastreamindices[j] = Integer.valueOf(line);
			  }
			  for(int j=0;j<numbaselines;j++)
			  {
			  	line = getinputline(input, "BASELINE");
			  	configs[configindex].baselineindices[j] = Integer.valueOf(line);
			  }
		  }
		}
		if(defaultconfigindex < 0)
		{
			System.err.println("Warning - no default config found - sources which were not specified will not be correlated!!!");
			configs[0].active = false;
		}

		configread = true;
	}
	
	private void processFreqTable(BufferedReader input) throws IOException {
		String line;

		line = getinputline(input, "FREQ ENTRIES");
		freqtablelength = Integer.valueOf(line);
		freqtable = new FreqData[freqtablelength];

		for(int i=0;i<freqtablelength;i++)
		{
			freqtable[i] = new FreqData();
		    line = getinputline(input, "FREQ (MHZ)");
		    freqtable[i].bandedgefreq = Double.valueOf(line);
		    line = getinputline(input, "BW (MHZ)");
		    freqtable[i].bandwidth = Double.valueOf(line);
		    line = getinputline(input, "SIDEBAND");
		    freqtable[i].lowersideband = (line.equalsIgnoreCase("L") || line.equalsIgnoreCase("LOWER"))?true:false;
		}
	}
	
	private void processTelescopeTable(BufferedReader input) throws IOException {
		String line;

		line = getinputline(input, "TELESCOPE ENTRIES");
		telescopetablelength = Integer.valueOf(line);
		telescopetable = new TelescopeData[telescopetablelength];

		for(int i=0;i<telescopetablelength;i++)
		{
			telescopetable[i] = new TelescopeData();
			telescopetable[i].name = getinputline(input, "TELESCOPE NAME");
		    line = getinputline(input, "CLOCK DELAY (us)");
		    telescopetable[i].clockdelay = Double.valueOf(line);
		    //cout << "Telescopetable " << i << " .clockdelay just got set to " << telescopetable[i].clockdelay << endl;
		    line = getinputline(input, "CLOCK RATE(us/s)");
		    telescopetable[i].clockrate = Double.valueOf(line);
		}
	}
	private void processDatastreamTable(BufferedReader input) throws IOException {
		String line;

		line = getinputline(input, "DATASTREAM ENTRIES");
		datastreamtablelength = Integer.valueOf(line);
		datastreamtable = new DatastreamData[datastreamtablelength];
		if(datastreamtablelength < numdatastreams)
		{
		    System.err.println("Error - not enough datastreams are supplied in the datastream table (" + datastreamtablelength + ") compared to the number of datastreams (" + numdatastreams + "!!!");
		    return;
		}
		  
		//get the information on the length of the internal buffer for the datastreams
		line = getinputline(input, "DATA BUFFER FACTOR");
		databufferfactor = Integer.valueOf(line);
		line = getinputline(input, "NUM DATA SEGMENTS");
		numdatasegments = Integer.valueOf(line);

		for(int i=0;i<datastreamtablelength;i++)
		{
			datastreamtable[i] = new DatastreamData(telescopetable, freqtable);
		    line = getinputline(input, "TELESCOPE INDEX");
		    datastreamtable[i].telescopeindex = Integer.valueOf(line);
		    line = getinputline(input, "TSYS");
		    datastreamtable[i].tsys = Double.valueOf(line);
		    line = getinputline(input, "DATA FORMAT");
		    if(line.equals("LBASTD"))
		        datastreamtable[i].format = DataFormat.LBASTD;
		    else if(line.equals("LBAVSOP"))
		        datastreamtable[i].format = DataFormat.LBAVSOP;
		    else if(line.equals("MKV"))
		    {
		        datastreamtable[i].format = DataFormat.MKV;
		        line = getinputline(input, "FANOUT");
		        datastreamtable[i].fanout = Integer.valueOf(line);
		    }
		    else if(line.equals("NZ"))
		        datastreamtable[i].format = DataFormat.NZ;
		    else if(line .equals("K5"))
		        datastreamtable[i].format = DataFormat.K5;
		    else
		    {
		    	System.err.println("Unnkown data format " + line + " (case sensitive choices are LBASTD, LBAVSOP, MKIV, NZ and K5) - assuming LBASTD!!!");
		        datastreamtable[i].format = DataFormat.LBASTD;
		    }
		    line = getinputline(input, "QUANTISATION BITS");
		    datastreamtable[i].numbits = Integer.valueOf(line);
		    line = getinputline(input, "FILTERBANK USED");
		    datastreamtable[i].filterbank = (line.equalsIgnoreCase("TRUE") || line.equalsIgnoreCase("T"))?true:false;
		    if(datastreamtable[i].filterbank)
		    	System.err.println("Error - filterbank not yet supported!!!");
		    line = getinputline(input, "READ FROM FILE");
		    datastreamtable[i].readfromfile = (line.equalsIgnoreCase("TRUE") || line.equalsIgnoreCase("T"))?true:false;
		    line = getinputline(input, "NUM FREQS");
		    datastreamtable[i].numfreqs = Integer.valueOf(line);
		    //cout << "Datastreamtable[" << i << "] has a numfreqs of " << datastreamtable[i].numfreqs << " - also, it has numbits " << datastreamtable[i].numbits << endl;
		    datastreamtable[i].freqpols = new int[datastreamtable[i].numfreqs];
		    datastreamtable[i].freqtableindices = new int[datastreamtable[i].numfreqs];
		    datastreamtable[i].freqclockoffsets = new double[datastreamtable[i].numfreqs];
		    datastreamtable[i].numinputbands = 0;
		    for(int j=0;j<datastreamtable[i].numfreqs;j++)
		    {
		        line = getinputline(input, "FREQ TABLE INDEX");
		        datastreamtable[i].freqtableindices[j] = Integer.valueOf(line);
		        line = getinputline(input, "CLK OFFSET");
		        datastreamtable[i].freqclockoffsets[j] = Double.valueOf(line);
		        line = getinputline(input, "NUM POLS");
		        datastreamtable[i].freqpols[j] = Integer.valueOf(line);
		        datastreamtable[i].numinputbands += datastreamtable[i].freqpols[j];
		    }
		    datastreamtable[i].bytespersamplenum = (datastreamtable[i].numinputbands*datastreamtable[i].numbits)/8;
		    if(datastreamtable[i].bytespersamplenum == 0)
		    {
		    	datastreamtable[i].bytespersamplenum = 1;
		    	datastreamtable[i].bytespersampledenom = 8/(datastreamtable[i].numinputbands*datastreamtable[i].numbits);
		    }
		    else
		    	datastreamtable[i].bytespersampledenom = 1;
		    if(!datastreamtable[i].filterbank)
		    	datastreamtable[i].numoutputbands = datastreamtable[i].numinputbands;
		    datastreamtable[i].inputbandpols = new char[datastreamtable[i].numinputbands];
		    datastreamtable[i].inputbandlocalfreqindices = new int[datastreamtable[i].numinputbands];
		    for(int j=0;j<datastreamtable[i].numinputbands;j++)
		    {
		    	line = getinputline(input, "INPUT BAND");
		    	datastreamtable[i].inputbandpols[j] = line.charAt(0);
		    	line = getinputline(input, "INPUT BAND");
		    	datastreamtable[i].inputbandlocalfreqindices[j] = Integer.valueOf(line);
		    	if(datastreamtable[i].inputbandlocalfreqindices[j] >= datastreamtable[i].numfreqs)
		    		System.err.println("Error - attempting to refer to freq outside local table!!!");
		    }
		}
		if(datastreamtable[0].inputbandpols[0] == 'X' || datastreamtable[0].inputbandpols[0] == 'Y' || 
				datastreamtable[0].inputbandpols[0] == 'x' || datastreamtable[0].inputbandpols[0] == 'y')
			polpairs = new String[] {"XX", "YY", "XY", "YX"};
		else
			polpairs = new String[] {"RR", "LL", "RL", "LR"};

		datastreamread = true;
	}
	
	private void processBaselineTable(BufferedReader input) throws IOException {
		int tempint;
		int [][] tempintptr;
		String line;

		line = getinputline(input, "BASELINE ENTRIES");
		baselinetablelength = Integer.valueOf(line);
		baselinetable = new BaselineData[baselinetablelength];
		if(baselinetablelength < numbaselines)
		{
			System.err.println("Error - not enough baselines are supplied in the baseline table (" + baselinetablelength + ") compared to the number of baselines (" + numbaselines + ")!!!");
		    return;
		}

		for(int i=0;i<baselinetablelength;i++)
		{
			baselinetable[i] = new BaselineData(telescopetable, freqtable, datastreamtable);
			baselinetable[i].totalbands = 0;
		    line = getinputline(input, "D/STREAM A INDEX");
		    baselinetable[i].datastream1index = Integer.valueOf(line);
		    line = getinputline(input, "D/STREAM B INDEX");
		    baselinetable[i].datastream2index = Integer.valueOf(line);
		    line = getinputline(input, "NUM FREQS");
		    baselinetable[i].numfreqs = Integer.valueOf(line);
		    baselinetable[i].numpolproducts = new int[baselinetable[i].numfreqs];
		    baselinetable[i].datastream1bandindex = new int[baselinetable[i].numfreqs][];
		    baselinetable[i].datastream2bandindex = new int[baselinetable[i].numfreqs][];
		    for(int j=0;j<baselinetable[i].numfreqs;j++)
		    {
		    	line = getinputline(input, "POL PRODUCTS");
		    	baselinetable[i].numpolproducts[j] = Integer.valueOf(line.trim());
		    	baselinetable[i].datastream1bandindex[j] = new int[baselinetable[i].numpolproducts[j]];
		    	baselinetable[i].datastream2bandindex[j] = new int[baselinetable[i].numpolproducts[j]];
		    	for(int k=0;k<baselinetable[i].numpolproducts[j];k++)
		    	{
		    		baselinetable[i].totalbands++;
		    		line = getinputline(input, "D/STREAM A BAND");
		    		baselinetable[i].datastream1bandindex[j][k] = Integer.valueOf(line);
		    		line = getinputline(input, "D/STREAM B BAND");
		    		baselinetable[i].datastream2bandindex[j][k] = Integer.valueOf(line);
		    	}
		    }
		    if(datastreamtable[baselinetable[i].datastream1index].telescopeindex > datastreamtable[baselinetable[i].datastream2index].telescopeindex)
		    {
		    	System.err.println("Error - first datastream for baseline " + i + " has a higher number than second datastream - reversing!!!");
		    	tempint = baselinetable[i].datastream1index;
		    	baselinetable[i].datastream1index = baselinetable[i].datastream2index;
		    	baselinetable[i].datastream2index = tempint;
		    	tempintptr = baselinetable[i].datastream1bandindex;
		    	baselinetable[i].datastream1bandindex = baselinetable[i].datastream2bandindex;
		    	baselinetable[i].datastream2bandindex = tempintptr;
		    }
		}
	}
	
	private void processDataTable(BufferedReader input) throws IOException {
		String line;
		
		for(int i=0;i<datastreamtablelength;i++)
		{
		    line = getinputline(input, "D/STREAM");
		    datastreamtable[i].numdatafiles = Integer.valueOf(line);
		    datastreamtable[i].datafilenames = new String[datastreamtable[i].numdatafiles];
		    for(int j=0;j<datastreamtable[i].numdatafiles;j++)
		    	datastreamtable[i].datafilenames[j] = getinputline(input, "FILE");
		  }
	}
	
	private void processNetworkTable(BufferedReader input) throws IOException {
		String line;

		for(int i=0;i<datastreamtablelength;i++)
		{
			line = getinputline(input, "PORT NUM");
			datastreamtable[i].portnumber = Integer.valueOf(line);
			line = getinputline(input, "TCP WINDOW SIZE");
			datastreamtable[i].tcpwindowsizekb = Integer.valueOf(line);
		}
	}
	
	public void consistencyCheck(boolean strict) throws ConsistencyException
	{
		int tindex, count;
	  double bandwidth, sampletimens, ffttime, nsincrement;
	  
	  //check that the configuration files specified exist and are consistent
	  if(strict) {
	  	try {
	    	checkDelayFile(false);
	    	checkUVWFile();
	    	checkThreadFile();
	  	}
	  	catch(FileNotFoundException e) {
		  	throw new ConsistencyException("Configuration file could not be opened: " + e.getMessage());
		  }
		  catch(IOException e) {
		  	throw new ConsistencyException("Configuration file was not read successfully: " + e.getMessage());
		  }
	  }
		if(!(outputformatstring.equals("RPFITS") || outputformatstring.equals("ASCII") || outputformatstring.equals("SWIN")))
			throw new ConsistencyException("Output format must be RPFITS or ASCII or SWIN - found " + outputformatstring);

	  //check entries in the datastream table
	  for(int i=0;i<datastreamtablelength;i++)
	  {
	    //check the telescope index is acceptable
	    if(datastreamtable[i].telescopeindex < 0 || datastreamtable[i].telescopeindex >= telescopetablelength)
	    {
	    	throw new ConsistencyException("Datastream table entry " + i + " has a telescope index that refers outside the telescope table range ("
	    			+ datastreamtable[i].telescopeindex + ")");
	    }

	    //check the local freq indices are all ok
	    for(int j=0;j<datastreamtable[i].numinputbands;j++)
	    {
	      if(datastreamtable[i].inputbandlocalfreqindices[j] < 0 || datastreamtable[i].inputbandlocalfreqindices[j] >= datastreamtable[i].numfreqs)
	      {
	      	throw new ConsistencyException("Datastream table entry " + i + " has an input band local frequency index (band " + j
	      			+ ") that refers outside the local frequency table range (" + datastreamtable[i].inputbandlocalfreqindices[j] + ")");
	      }
	    }
	    
	    //check the frequency table indices are ok and all the bandwidths all match
	    bandwidth = freqtable[datastreamtable[i].freqtableindices[0]].bandwidth;
	    for(int j=0;j<datastreamtable[i].numfreqs;j++)
	    {
	      if(datastreamtable[i].freqtableindices[j] < 0 || datastreamtable[i].freqtableindices[j] >= freqtablelength)
	      {
	      	throw new ConsistencyException("Datastream table entry " + i + " has a frequency index (freq " + j + ") that refers outside the frequency table range ("
	      			+ datastreamtable[i].freqtableindices[j] + ")");
	      }
	      if(bandwidth != freqtable[datastreamtable[i].freqtableindices[j]].bandwidth)
	      {
	      	throw new ConsistencyException("All bandwidths for a given datastream must be equal");
	      }
	    }
	  }

	  //check that for all configs, the datastreams refer to the same telescope
	  for(int i=0;i<numdatastreams;i++)
	  {
		tindex = -1;
	    for(int j=0;j<configs.length;j++)
	    {
	    	if(configs[j].active) {
	    		if(tindex == -1)
	    			tindex = datastreamtable[configs[j].datastreamindices[i]].telescopeindex;
	    		else if(tindex != datastreamtable[configs[j].datastreamindices[i]].telescopeindex)
		        {
		      	    throw new ConsistencyException("All configs must have the same telescopes!  Config " + j + " datastream " + i + " refers to different telescopes");
		        }
	    	}
	    }
	  }

	  //check entries in the config table, check that number of channels * sample time yields a whole number of nanoseconds and 
	  //that the nanosecond increment is not too large for an int, and generate the ordered datastream indices array
	  for(int i=0;i<configs.length;i++)
	  {
		if(configs[i].active) {
//			work out the ordereddatastreamindices
		  	count = 0;
		  	for(int j=0;j<numdatastreams;j++)
		  	{
		  		configs[i].ordereddatastreamindices[j] = -1;
		  		for(int k=0;k<datastreamtablelength;k++)
		  		{
		  			if(configs[i].datastreamindices[j] == k)
		  				configs[i].ordereddatastreamindices[j] = count++;
		      }
		    }
		  	if(count != numdatastreams)
		  	{
		  		throw new ConsistencyException("Not all datastreams accounted for in the datastream table for config " + i);
		    }

		  	// check that number of channels * sample time yields a whole number of nanoseconds for every datastream
		  	for(int j=0;j<numdatastreams;j++)
		  	{
		  		sampletimens = 1000.0/freqtable[datastreamtable[configs[i].datastreamindices[j]].freqtableindices[0]].bandwidth;
		  		ffttime = sampletimens*configs[i].numchannels*2;
		  		nsincrement = ffttime*configs[i].blockspersend*(databufferfactor/numdatasegments);
		  		if(ffttime - (int)(ffttime+0.5) > 0.00000001 || ffttime - (int)(ffttime+0.5) < -0.000000001)
		  		{
		  			throw new ConsistencyException("FFT chunk time for config " + i + ", datastream " + j + " is not a whole number of nanoseconds ("
		  					+ ffttime + ")");
		      }
		  		if(nsincrement > Integer.MAX_VALUE)
		  		{
		  			throw new ConsistencyException("Increment per read in nanoseconds is " + nsincrement + " - too large to fit in an int");
		      }
		    }

		  	//check that all baseline indices refer inside the table
		  	for(int j=0;j<numbaselines;j++)
		  	{
		  		if(configs[i].baselineindices[j] < 0 || configs[i].baselineindices[j] >= baselinetablelength) //bad index
		  		{
		  			throw new ConsistencyException("Config " + i + " baseline index " + j + " refers to baseline " + configs[i].baselineindices[j]
		  					+ " which is outside the range of the baseline table");
		      }
		    }	
		}
	  }
	  
	  //check the baseline table entries
	  for(int i=0;i<baselinetablelength;i++)
	  {
	  	//check the datastream indices
	    if(baselinetable[i].datastream1index < 0 || baselinetable[i].datastream2index < 0 || baselinetable[i].datastream1index >=
	    	datastreamtablelength || baselinetable[i].datastream2index >= datastreamtablelength)
	    {
	    	throw new ConsistencyException("Baseline table entry " + i + " has a datastream index outside the datastream table range! Its two indices are "
	    			+ baselinetable[i].datastream1index + ", " + baselinetable[i].datastream2index);
	    }

	    //check the band indices
	    for(int j=0;j<baselinetable[i].numfreqs;j++)
	    {
	      for(int k=0;k<baselinetable[i].numpolproducts[j];k++)
	      {
	        if(baselinetable[i].datastream1bandindex[j][k] < 0 || baselinetable[i].datastream1bandindex[j][k] >= datastreamtable[baselinetable[i].datastream1index].numinputbands)
	        {
	        	throw new ConsistencyException("Baseline table entry " + i + ", frequency " + j + ", polarisation product " + k
	        			+ " for datastream 1 refers to a band outside datastream 1's range (" + baselinetable[i].datastream1bandindex[j][k] + ")");
	        }
	        if(baselinetable[i].datastream2bandindex[j][k] < 0 || baselinetable[i].datastream2bandindex[j][k] >= datastreamtable[baselinetable[i].datastream2index].numinputbands)
	        {
	        	throw new ConsistencyException("Baseline table entry " + i + ", frequency " + j + ", polarisation product " + k + 
	        			" for datastream 2 refers to a band outside datastream 2's range (" + baselinetable[i].datastream2bandindex[j][k] + ")");
	        }
	      }
	    }
	  }

	  //check that there is an integer number of blocks per data segment
	  if(databufferfactor % numdatasegments != 0)
	  {
	    throw new ConsistencyException("There must be an integer number of sends per datasegment.  Presently databufferfactor is "
	    		+ databufferfactor + ", and numdatasegments is " + numdatasegments);
	  }
	}
	
	private void checkDelayFile(boolean loadsources) throws IOException, ConsistencyException
	{
		Vector<String> scannames = new Vector<String>();
		String source;
		int year, month, day, hour, minute, second, incrementsecs, numdelaytelescopes,
		    numscans, scanpoints, totalpoints, delaymjd, delaystartseconds, delaylengthseconds;
		boolean found;
		String [] telescopenames;
		numsources = 0;
		BufferedReader input = new BufferedReader(new FileReader(delayfilename));
		year = Integer.valueOf(getinputline(input, "START YEAR"));
		month = Integer.valueOf(getinputline(input, "START MONTH"));
		day = Integer.valueOf(getinputline(input, "START DAY"));
		hour = Integer.valueOf(getinputline(input, "START HOUR"));
	  minute = Integer.valueOf(getinputline(input, "START MINUTE"));
	  second = Integer.valueOf(getinputline(input, "START SECOND"));
	  incrementsecs = Integer.valueOf(getinputline(input, "INCREMENT (SECS)"));
	  numdelaytelescopes = Integer.valueOf(getinputline(input, "NUM TELESCOPES"));
	  telescopenames = new String[numdelaytelescopes];
	  for(int i=0;i<numdelaytelescopes;i++) {
	  	telescopenames[i] = getinputline(input, "TELESCOPE");
	  }
	  
	  //check all the telescopes for this experiment can be found in the delay file
	  if(!loadsources) {
		  for(int i=0;i<configs.length;i++) {
			  if(configs[i].active) {
			  	for(int j=0;j<numdatastreams;j++) {
			  		found = false;
			  		for(int k=0;k<numdelaytelescopes;k++) {
			  			if(telescopenames[k].trim().equals(telescopetable[datastreamtable[configs[i].datastreamindices[j]].telescopeindex].name.trim()))
			  				found = true;
			  		}
			  		if(!found)
			  			throw new ConsistencyException("Telescope " + telescopetable[datastreamtable[configs[i].datastreamindices[j]].telescopeindex].name + " could not be found in the delay file!!!");
			  	}
			  }
		  }
	  }
	  
	  //check that the delay file covers the intended experiment range
	  totalpoints = 0;
	  numscans =  Integer.valueOf(getinputline(input, "NUM SCANS"));
	  for(int i=0;i<numscans;i++) {
	  	scanpoints = Integer.valueOf(getinputline(input, "SCAN"));
	  	totalpoints += scanpoints;
	  	getinputline(input, "SCAN");
	  	source = getinputline(input, "SCAN"); //skip the start point and source name
	  	found = false;
	  	for(int j=0;j<numsources;j++) {
	  		if(source.equals(scannames.elementAt(j)))
	  			found = true;
	  	}
	  	if(!found) {
	  		scannames.add(source);
	  		numsources++;
	  	}
	  		
	  	for(int j=0;j<scanpoints+3;j++)
	  		getinputline(input, "RELATIVE"); //skip all the delay points
	  }
	  if(loadsources) {
		  sourcenames = new String[numsources];
		  for(int i=0;i<numsources;i++)
			  sourcenames[i] = scannames.elementAt(i);
		  return; //bail out, don't care if everything else is consistent
	  }
	  delaymjd = ymd2mjd(year, month, day);
	  delaystartseconds = hour*3600 + minute*60 + second;
	  delaylengthseconds = totalpoints*incrementsecs;
	  if((startmjd-delaymjd)*86400 + startseconds - delaystartseconds < 0 || 
	  	 (startmjd-delaymjd)*86400 + startseconds + executeseconds - (delaystartseconds + delaylengthseconds) > 0)
	  	throw new ConsistencyException("Experiment runs from MJD " + startmjd + " + " + startseconds +
	  			" seconds to MJD " + (startmjd + ((startseconds + executeseconds)/86400)) + " + " +
	  			((startseconds + executeseconds)%86400) + " seconds, while delay file runs from MJD " + 
	  			delaymjd + " + " + delaystartseconds + " to MJD " + (delaymjd + ((delaystartseconds + delaylengthseconds)/86400)) +
	  			 " + " + ((delaystartseconds + delaylengthseconds)%86400) + " seconds");
	}
	
	private void checkUVWFile() throws IOException, ConsistencyException
	{
		int year, month, day, hour, minute, second, incrementsecs, numuvwtelescopes,
		    numscans, scanpoints, totalpoints, uvwmjd, uvwstartseconds, uvwlengthseconds;
		boolean found;
		String [] telescopenames;
		BufferedReader input = new BufferedReader(new FileReader(uvwfilename));
		year = Integer.valueOf(getinputline(input, "START YEAR"));
		month = Integer.valueOf(getinputline(input, "START MONTH"));
		day = Integer.valueOf(getinputline(input, "START DAY"));
		hour = Integer.valueOf(getinputline(input, "START HOUR"));
	  minute = Integer.valueOf(getinputline(input, "START MINUTE"));
	  second = Double.valueOf(getinputline(input, "START SECOND")).intValue();
	  incrementsecs = Integer.valueOf(getinputline(input, "INCREMENT (SECS)"));
	  numuvwtelescopes = Integer.valueOf(getinputline(input, "NUM TELESCOPES"));
	  telescopenames = new String[numuvwtelescopes];
	  for(int i=0;i<numuvwtelescopes;i++) {
	  	telescopenames[i] = getinputline(input, "TELESCOPE");
	  	//skip the telescope mount and xyz
	  	for(int j=0;j<4;j++)
	  		getinputline(input, "TELESCOPE");
	  }
	  
	  //check all the telescopes for this experiment can be found in the delay file
	  for(int i=0;i<configs.length;i++) {
		if(configs[i].active) {
		  	for(int j=0;j<numdatastreams;j++) {
		  		found = false;
		  		for(int k=0;k<numuvwtelescopes;k++) {
		  			if(telescopenames[k].trim().equals(telescopetable[datastreamtable[configs[i].datastreamindices[j]].telescopeindex].name.trim()))
		  				found = true;
		  		}
		  		if(!found)
		  			throw new ConsistencyException("Telescope " + telescopetable[datastreamtable[configs[i].datastreamindices[j]].telescopeindex].name + " could not be found in the uvw file!!!");
		  	}
		}
	  }
	  
	  //check that the delay file covers the intended experiment range
	  totalpoints = 0;
	  numscans =  Integer.valueOf(getinputline(input, "NUM SCANS"));
	  for(int i=0;i<numscans;i++) {
	  	scanpoints = Integer.valueOf(getinputline(input, "SCAN"));
	  	totalpoints += scanpoints;
	  	for(int j=0;j<4;j++)
	  		getinputline(input, "SCAN"); //skip the start point and source name, ra and dec
	  	for(int j=0;j<scanpoints+3;j++)
	  		getinputline(input, "RELATIVE"); //skip all the delay points
	  }
	  uvwmjd = ymd2mjd(year, month, day);
	  uvwstartseconds = hour*3600 + minute*60 + second;
	  uvwlengthseconds = totalpoints*incrementsecs;
	  if((startmjd-uvwmjd)*86400 + startseconds - uvwstartseconds < 0 || 
	  	 (startmjd-uvwmjd)*86400 + startseconds + executeseconds - (uvwstartseconds + uvwlengthseconds) > 0)
	  	throw new ConsistencyException("Experiment runs from MJD " + startmjd + " + " + startseconds +
	  			" seconds to MJD " + (startmjd + ((startseconds + executeseconds)/86400)) + " + " +
	  			((startseconds + executeseconds)%86400) + " seconds, while delay file runs from MJD " + 
	  			uvwmjd + " + " + uvwstartseconds + " to MJD " + (uvwmjd + ((uvwstartseconds + uvwlengthseconds)/86400)) +
	  			 " + " + ((uvwstartseconds + uvwlengthseconds)%86400) + " seconds");
	}
	
	private void checkThreadFile() throws IOException, ConsistencyException
	{
		BufferedReader input = new BufferedReader(new FileReader(coreconffilename));
		int maxcores = Integer.valueOf(getinputline(input, "NUMBER OF CORES"));
		for(int i=0;i<maxcores;i++)
			input.readLine(); //will throw IOException if file ends
		if(numnodes - (numdatastreams + 1) > maxcores)
			throw new ConsistencyException("Thread file " + coreconffilename + " specifies a max of " +
					                           maxcores + " cores, while " + (numnodes - numdatastreams - 1) +
					                           " are to be used"); 
	}
	
	private void loadSourceNames() throws IOException, ConsistencyException {
		checkDelayFile(true);
		ConfigData [] oldconfigs = configs;
		configs = new ConfigData[sourcenames.length + 1];
		for(int i=0;i<oldconfigs.length;i++) {
			if(oldconfigs[i].sourcename.trim().equals("DEFAULT"))
				configs[0] = oldconfigs[i];
		}
		if(configs[0] == null)
			configs[0] = new ConfigData("DEFAULT        ", true);
		for(int i=0;i<sourcenames.length;i++) {
			for(int j=0;j<oldconfigs.length;j++) {
				if(sourcenames[i].equals(oldconfigs[j].sourcename))
					configs[i+1] = oldconfigs[j];
			}
			if(configs[i+1] == null){
				configs[i+1] = new ConfigData(sourcenames[i], false);
			}
		}
		System.out.println("Number of configs is " + configs.length);
	}
	
	/** Converts a year, month and day into Modifed Julian Date (MJD) */
	public static int ymd2mjd(int year, int month, int day)
	{
		return year*367 - ((int)(7*(year + ((int)((month + 9)/12)))/4)) + ((int)(275*month/9)) + day - 678987;
	}
	
	/** Converts an mjd into year, month and day, returned in an array of 3 integers */
	public static int [] mjd2ymd(int mjd)
	{
		int [] toreturn = new int[3];
		
	  int j = mjd + 32044 + 2400001;
	  int g = j / 146097;
	  int dg = j % 146097;
	  int c = ((dg/36524 + 1)*3)/4;
	  int dc = dg - c*36524;
	  int b = dc / 1461;
	  int db = dc % 1461;
	  int a = ((db/365 + 1)*3)/4;
	  int da = db - a*365;
	  int y = g*400 + c*100 + b*4 + a;
	  int m = (da*5 + 308)/153 - 2;
	  int d = da - ((m + 4)*153)/5 + 122;
	  
	  toreturn[0] = y - 4800 + (m + 2)/12;
	  toreturn[1] = (m + 2)%12 + 1;
	  toreturn[2] = d + 1;
	  
	  return toreturn;
	}
	
	/** Throws away the constant length lead-in to a value in the input file, but compares to ensure that the header starts with what it is expected to */
	public String getinputline(BufferedReader input, String startofheader) throws IOException
	{
		char [] cbuf = new char[DiFXManager.KEYWORD_LENGTH];
		input.read(cbuf, 0, DiFXManager.KEYWORD_LENGTH);
		if(!(new String(cbuf).substring(0, startofheader.length()).equals(startofheader))) //not what we expected
			System.err.println("Error - we thought we were reading something starting with '" + startofheader + "', when we actually got '" + new String(cbuf));
		return input.readLine();
	}
	
	private void writeoutputline(PrintWriter output, String value, String keyword) {
		output.println(keyword + ":" + blanks.substring(keyword.length()+1) + value);
	}
	private void writeoutputline(PrintWriter output, char value, String keyword) {
		writeoutputline(output, (new Character(value)).toString(), keyword);
	}
	private void writeoutputline(PrintWriter output, boolean value, String keyword) {
		writeoutputline(output, Boolean.toString(value).toUpperCase(), keyword);
	}
	private void writeoutputline(PrintWriter output, double value, String keyword) {
		writeoutputline(output, Double.toString(value), keyword);
	}
	private void writeoutputline(PrintWriter output, int value, String keyword) {
		writeoutputline(output, Integer.toString(value), keyword);
	}
	
	//private classes for storage of table elements
	private class FreqData {
		double bandedgefreq;
		double bandwidth;
		boolean lowersideband;
		
		public FreqData() {
			bandedgefreq = 0.0;
			bandwidth = 0.0;
			lowersideband = false;
		}
		
		public String toString() {
			return bandedgefreq + " " + bandwidth + " " + Boolean.toString(lowersideband);
		}
	}

	private class BaselineData {
		int datastream1index;
		int datastream2index;
		int numfreqs;
		int totalbands;
		int [] numpolproducts;
		int [][] datastream1bandindex;
		int [][] datastream2bandindex;
		TelescopeData [] telescopetable;
		DatastreamData [] datastreamtable;
		FreqData [] freqtable;
		
		public BaselineData(TelescopeData [] tt, FreqData [] ft, DatastreamData [] dt) {
			telescopetable = tt;
			freqtable = ft;
			datastreamtable = dt;
		}
		
		public String toString() {
			DatastreamData d1 = datastreamtable[datastream1index];
			DatastreamData d2 = datastreamtable[datastream2index];
			String toreturn = "BASELINE:           " + telescopetable[datastreamtable[datastream1index].telescopeindex].name + 
			                  "-" + telescopetable[datastreamtable[datastream2index].telescopeindex].name + "\n" +
			                  "NUMBER OF FREQS:    " + numfreqs + "\n";
			for(int i=0;i<numfreqs;i++) {
				toreturn += freqtable[d1.freqtableindices[d1.inputbandlocalfreqindices[datastream1bandindex[i][0]]]].bandedgefreq + ":             ";
				for(int j=0;j<numpolproducts[i];j++) {
					toreturn += (d1.inputbandpols[datastream1bandindex[i][j]]+"") + (d2.inputbandpols[datastream2bandindex[i][j]]+"") + " ";
				}
				toreturn += "\n";
			}
			toreturn += "\n";
			
			return toreturn;
		}
		
		public String getName() {
			return telescopetable[datastreamtable[datastream1index].telescopeindex].name + 
            "-" + telescopetable[datastreamtable[datastream2index].telescopeindex].name;
		}
		
		public String getBandSummary() {
			DatastreamData d1 = datastreamtable[datastream1index];
			DatastreamData d2 = datastreamtable[datastream2index];
			String toreturn = "";
			for(int i=0;i<numfreqs;i++) {
				toreturn += freqtable[d1.freqtableindices[d1.inputbandlocalfreqindices[datastream1bandindex[i][0]]]].bandedgefreq;
				for(int j=0;j<numpolproducts[i];j++) {
					toreturn += (d1.inputbandpols[datastream1bandindex[i][j]]+"") + (d2.inputbandpols[datastream2bandindex[i][j]]+"");
				}
				if(i!=numfreqs-1) toreturn += ",";
			}
			return toreturn;
		}
	}

	private class ConfigData {
		String sourcename;
		boolean active;
		double inttime;
		int numchannels;
		int blockspersend;
		int guardblocks;
		boolean postffringerot;
		boolean quadraticdelayinterp;
		boolean writeautocorrs;
		boolean pulsarbin;
		int numpolycos;
		int numbins;
		boolean scrunchoutput;
		String pulsarfilename;
		String [][] polycofilenames;
		int [] datastreamindices;
		int [] ordereddatastreamindices;
		int [] baselineindices;
		
		public ConfigData(String sourcename, boolean active) {
			this.sourcename = new String(sourcename);
			this.active = active;
		}
		
		public String toString() {
			String toreturn = "";
			toreturn += "CONFIG SOURCE:      " + sourcename + "\n";
			toreturn += "INT TIME (SEC):     " + inttime + "\n";
			toreturn += "NUM CHANNELS:       " + numchannels + "\n";
			toreturn += "BLOCKS PER SEND:    " + blockspersend + "\n";
			toreturn += "GUARD BLOCKS:       " + guardblocks + "\n";
			toreturn += "POST-F FRINGE ROT:  " + Boolean.valueOf(postffringerot) + "\n";
			toreturn += "QUAD DELAY INTERP:  " + Boolean.valueOf(quadraticdelayinterp) + "\n";
			toreturn += "WRITE AUTOCORRS:    " + Boolean.valueOf(writeautocorrs) + "\n";
			toreturn += "PULSAR BINNING:     " + Boolean.valueOf(pulsarbin) + "\n";
			if(pulsarbin) {
				toreturn += "SCRUNCH OUTPUT:     " + Boolean.valueOf(scrunchoutput) + "\n";
				toreturn += "PULSAR FILENAME:    " + pulsarfilename + "\n";
				for(int i=0;i<numpolycos;i++)
					toreturn += "POLYCO " + i + " FILENAME:    " + polycofilenames[i] + "\n";
			}
			for(int i=0;i<numdatastreams;i++) {
				toreturn += "DATASTREAM " + i + " INDEX: " + datastreamindices[i] + "\n";
			}
			for(int i=0;i<numbaselines;i++) {
				toreturn += "BASELINE " + i + " INDEX:   " + baselineindices[i] + "\n";
			}
			toreturn += "\n";
			return toreturn;
		}
	}

	private class TelescopeData {
		String name;
		double clockdelay;
		double clockrate;
		
		public String toString() {
			return "TELESCOPE NAME:    " + name + "\n" + 
			       "CLOCK DELAY (us):  " + clockdelay + "\n" +
			       "CLOCK RATE (us/s): " + clockrate + "\n" + "\n";
		}
	}

	private class DatastreamData {
		int telescopeindex;
		double tsys;
		int format; //DataFormat
		int numbits;
		int bytespersamplenum;
		int bytespersampledenom;
		int fanout;
		int framebytes;
		boolean filterbank;
		boolean readfromfile;
		int numfreqs;
		int [] freqpols;
		int [] freqtableindices;
		double [] freqclockoffsets;
		int numinputbands;
		int numoutputbands;
		char [] inputbandpols;
		int [] inputbandlocalfreqindices;
		int numdatafiles;
		String [] datafilenames;
		int portnumber;
		int tcpwindowsizekb;
		FreqData [] freqtable;
		TelescopeData [] telescopetable;
		
		public DatastreamData(TelescopeData [] td, FreqData [] ft) {
			freqtable = ft;
			telescopetable = td;
			numfreqs = numinputbands = numdatafiles = portnumber = tcpwindowsizekb = fanout = numbits = 0;
			format = 0;
			readfromfile = true;
			filterbank = false;
		}
		
		public String toString() {
			String toreturn =  "TELESCOPE:          " + telescopetable[telescopeindex].name + "\n" + 
				"TSYS (JY):          " + tsys + "\n" +
				"FORMAT:             " + DataFormat.formatstrings[format] + "\n" +
				"NUMBER OF BITS:     " + numbits + "\n" + 
				((format == DataFormat.MKV)?("FANOUT:             " + fanout + "\n"):"") + 
				"FILTERBANK ON:      " + Boolean.valueOf(filterbank)+ "\n" + 
				"READ FROM FILE:     " + Boolean.valueOf(readfromfile) + "\n" + 
				"NUMBER OF FREQS:    " + numfreqs + "\n" +
				"NUMBER OF BANDS:    " + numinputbands + "\n";
			for(int i=0;i<numinputbands;i++) {
				toreturn += "BAND " + i + ":             " + freqtable[inputbandlocalfreqindices[i]].bandedgefreq + " " + 
				            freqtable[inputbandlocalfreqindices[i]].bandwidth + " " +inputbandpols[i] + "\n";
			}
			if(readfromfile)
			  toreturn += "NUM DATA FILES:     " + numdatafiles + "\n\n";
			else
				toreturn += "PORT NUMBER:        " + portnumber + "\n" + "TCP WINDOW (KB):   " + tcpwindowsizekb + "\n\n";

			return toreturn;		 
		}
		
		public String getBandSummary() {
			String toreturn = "";
			for(int i=0;i<numinputbands;i++) {
				toreturn += freqtable[inputbandlocalfreqindices[i]].bandedgefreq + ":"  + inputbandpols[i] + ",";
			}
			return toreturn.substring(0,toreturn.length()-1);
		}
	}
	
	private class FileTrimmer {
	    Vector<Boolean> active;
	    boolean working, passall, bysource, bymode;
	    int totalpoints, incrementsecs, startmjd, startdoy, startseconds;
	    int corrdoy, corrseconds, correxecute, nummodescans;
	    int [] modestarts;
	    int [] modestops;
	    
		
	    public FileTrimmer(int startmjd, int startseconds, int [] modestarts, int [] modestops, int nmodescans)
	    {
		nummodescans = nmodescans;
		this.modestarts = modestarts;
		this.modestops = modestops;
		int [] ymd = mjd2ymd(startmjd);
		int mjd0 = ymd2mjd(ymd[0], 1, 1) - 1;
		corrdoy = startmjd - mjd0;
		corrseconds = startseconds;
		correxecute = executeseconds;
		working = true;
		passall = false;
		bysource = false;
		bymode = true;
	    }

	        public FileTrimmer(int startmjd, int startseconds, int executeseconds) {
		    int [] ymd = mjd2ymd(startmjd);
		    int mjd0 = ymd2mjd(ymd[0], 1, 1) - 1;
		    corrdoy = startmjd - mjd0;
		    corrseconds = startseconds;
		    correxecute = executeseconds;
		    working = true;
		    passall = false;
		    bysource = false;
		    bymode = false;
		}
	  
		public FileTrimmer(String uvwfilename, ConfigData [] configs) {
			int year, month, day, hour, minute, second, numuvwtelescopes, numscans, scanpoints;
			boolean sourceactive;
			String sourcename;
			active = new Vector<Boolean>();
			bysource = true;
			bymode = false;
			working = true;
			passall = false;
			for(int i=0;i<configs.length;i++) {
				if(configs[i].sourcename.trim().equals("DEFAULT") && configs[i].active)
					passall = true;
			}
			
			try {
				BufferedReader input = new BufferedReader(new FileReader(uvwfilename));
				year = Integer.valueOf(getinputline(input, "START YEAR"));
				month = Integer.valueOf(getinputline(input, "START MONTH"));
				day = Integer.valueOf(getinputline(input, "START DAY"));
				hour = Integer.valueOf(getinputline(input, "START HOUR"));
			  minute = Integer.valueOf(getinputline(input, "START MINUTE"));
			  second = Double.valueOf(getinputline(input, "START SECOND")).intValue();
			  startmjd = ymd2mjd(year, month, day);
			  startdoy = startmjd - ymd2mjd(year, 1, 1) + 1;
			  startseconds = hour*3600 + minute*60 + second;
			  incrementsecs = Integer.valueOf(getinputline(input, "INCREMENT (SECS)"));
			  numuvwtelescopes = Integer.valueOf(getinputline(input, "NUM TELESCOPES"));
			  for(int i=0;i<numuvwtelescopes;i++) {
			  	getinputline(input, "TELESCOPE");
			  	//skip the telescope mount and xyz
			  	for(int j=0;j<4;j++)
			  		getinputline(input, "TELESCOPE");
			  }
			  
			  totalpoints = 0;
			  numscans =  Integer.valueOf(getinputline(input, "NUM SCANS"));
			  for(int i=0;i<numscans;i++) {
			  	sourceactive = false;
			  	scanpoints = Integer.valueOf(getinputline(input, "SCAN"));
			  	totalpoints += scanpoints;
			  	getinputline(input, "SCAN"); //skip the start point
			  	sourcename = getinputline(input, "SCAN");
			  	for(int j=0;j<configs.length;j++) {
			  		if(configs[j].sourcename.trim().equals(sourcename.trim()) &&configs[j].active)
			  			sourceactive = true;
			  	}
			  	getinputline(input, "SCAN"); //skip the ra
			  	getinputline(input, "SCAN"); // skip the dec
			  	for(int j=0;j<scanpoints;j++) {
			  		getinputline(input, "RELATIVE"); //skip all the delay points
			  		active.add(new Boolean(sourceactive));
			  	}
			  	for(int j=0;j<3;j++)
			  		getinputline(input, "RELATIVE"); //skip the extra delay points
			  }
			}
			catch(IOException e) {
				working = false;
			}
		}
		
		public String [] trimDataFiles(String [] datafiles) {
			if(passall || !working)
				return datafiles; //if default is active or we had a problem, don't trim any
			
			int numtokens, doy, hhmmss, offset, mjd;
			int numkept = 0;
			boolean [] keep = new boolean[datafiles.length];
			StringTokenizer st;
			for(int i=0;i<datafiles.length;i++) {
				st = new StringTokenizer(datafiles[i], "_.");
				numtokens = st.countTokens();
				try {
					for(int j=0;j<numtokens-3;j++)
						st.nextToken();
					doy = Integer.parseInt(st.nextToken());
					hhmmss = Integer.parseInt(st.nextToken());
					if(bysource) {
					    offset = ((doy-startdoy)*86400 + (hhmmss/10000)*3600 + ((hhmmss%10000)/100)*60 + hhmmss%100 - startseconds)/incrementsecs;
					    keep[i] = active.elementAt(offset).booleanValue();
					}
					else if (bymode) {
					    offset = (doy-corrdoy)*86400 + (hhmmss/10000)*3600 + ((hhmmss%10000)/100)*60 + hhmmss%100;
					    keep[i] = false;
					    for(int j=0;j<nummodescans;j++) {
						if(offset >= modestarts[j] && offset < modestops[j])
						    keep[i] = true;
					    }
					}
					else {
					    offset = (doy-corrdoy)*86400 + (hhmmss/10000)*3600 + ((hhmmss%10000)/100)*60 + hhmmss%100 - corrseconds;
					    keep[i] = ((offset >= 0) && (offset < correxecute))?true:false;

					}
					if(keep[i])
					    numkept++;
				}
				catch(NumberFormatException e) {
					keep[i] = true; //can't work out what the time is so better keep it
					numkept++;
				}
				catch(ArrayIndexOutOfBoundsException e) {
					keep[i] = false;  //outside the uvw range so ditch it
				}
			}
			String [] toreturn = new String[numkept];
			int count = 0;
			for(int i=0;i<datafiles.length;i++) {
				if(keep[i])
					toreturn[count++] = datafiles[i];
			}
			
			return toreturn;
		}
	}
	
	private static class DataFormat {
		public static final int LBASTD = 0;
		public static final int LBAVSOP = 1;
		public static final int MKV = 2;
		public static final int MKV_MKIV = 3;
		public static final int MKV_VLBA = 4;
		public static final int NZ = 5;
		public static final int K5 = 6;		
		
		public static final String [] formatstrings = {"LBA", "LBAVSOP", "MKV", "MKV_MKIV", "MKV_VLBA", "K5", "NZ"};
	}
	
	private class SectionHeader {
		public static final int COMMON = 0;
		public static final int CONFIG = 1;
		public static final int FREQ = 2;
		public static final int TELESCOPE = 3;
		public static final int DATASTREAM = 4;
		public static final int BASELINE = 5;
		public static final int DATA = 6;
		public static final int NETWORK = 7;
		public static final int INPUT_EOF = 8;
		public static final int UNKNOWN = 9;
	}
}
