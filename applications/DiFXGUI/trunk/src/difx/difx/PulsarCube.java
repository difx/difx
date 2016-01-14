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

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.DoubleBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.channels.FileChannel;


public class PulsarCube {
	// constants
	private static final int NUM_AXES = 4;

	public static final String[] AXIS_NAMES = { "TIME", "BAND", "CHANNEL",
			"BIN" };

	// cube variables
	private String pulsarcubefilename, stationname;

	private float[][][][] rawcube;

	private double[][] axisvalues;

	private double[][] zvalues;

	private double[] xvalues;

	private double[] yvalues;

	private int xindex, yindex;

	private int[] counterstart;

	private int[] counterlength;

	private int[] counter;

	private double bandwidth, tsys, inttime;

	private int numfreqs, numbands, numpbins, numchannels, numrecords;

	// binconfig variables
	private String binconfigfilename;

	private int numpolycofiles, numconfigpbins;

	private String[] polycofilenames;

	private boolean scrunchOutput;

	private double[] binconfigweights;

	private double[] binconfigphases;

	// io variables
	private BufferedReader input;

	private BufferedWriter output;

	private DataInputStream binaryinput;

	public PulsarCube(String binconfigfilename, String pulsarcubefilename) {
		super();
		this.binconfigfilename = binconfigfilename;
		this.pulsarcubefilename = pulsarcubefilename;
	}

	public PulsarCube() {
		counterstart = new int[NUM_AXES];
		counterlength = new int[NUM_AXES];
		counter = new int[NUM_AXES];
		axisvalues = new double[NUM_AXES][1];
		for (int i = 0; i < NUM_AXES; i++)
			axisvalues[i][0] = 0.0;
	}

	public void loadBinConfigFile(String binconfigfilename) {
		String line;

		this.binconfigfilename = binconfigfilename;

		// read in the info and store it
		try {
			input = new BufferedReader(new FileReader(binconfigfilename));
			line = getInputLine(input, "NUM POLYCO FILES:");
			numpolycofiles = Integer.valueOf(line);
			polycofilenames = new String[numpolycofiles];
			for (int i = 0; i < numpolycofiles; i++) {
				polycofilenames[i] = getInputLine(input, "POLYCO FILE");
			}
			line = getInputLine(input, "NUM PULSAR BINS");
			numconfigpbins = Integer.valueOf(line);
			line = getInputLine(input, "SCRUNCH OUTPUT");
			scrunchOutput = Boolean.valueOf(line);
			binconfigphases = new double[numconfigpbins];
			binconfigweights = new double[numconfigpbins];
			for (int i = 0; i < numconfigpbins; i++) {
				line = getInputLine(input, "BIN PHASE END");
				binconfigphases[i] = Double.valueOf(line);
				line = getInputLine(input, "BIN WEIGHT");
				binconfigweights[i] = Double.valueOf(line);
				// System.out.println("Phase was " + binconfigphases[i] + ",
				// weight was " + binconfigweights[i]);
			}
			input.close();
		} catch (IOException e) {
			System.err.println("Error parsing binconfigfile "
					+ binconfigfilename + ": " + e.getMessage());
		}
	}

	public void saveBinConfigFile() {
		saveBinConfigFile(binconfigfilename);
	}

	public void saveBinConfigFile(String filename) {
		try {
			output = new BufferedWriter(new FileWriter(filename));

			writeOutputLine(output, "NUM POLYCO FILES:", numpolycofiles + "");
			for (int i = 0; i < numpolycofiles; i++)
				writeOutputLine(output, "POLYCO FILE" + i + ":",
						polycofilenames[i]);
			writeOutputLine(output, "NUM PULSAR BINS:", numconfigpbins + "");
			writeOutputLine(output, "SCRUNCH OUTPUT:", Boolean
					.toString(scrunchOutput));
			for (int i = 0; i < numconfigpbins; i++) {
				writeOutputLine(output, "BIN PHASE END " + i + ":",
						binconfigphases[i] + "");
				writeOutputLine(output, "BIN WEIGHT " + i + ":",
						binconfigweights[i] + "");
			}

			output.close();
		} catch (IOException e) {
			System.err.println("Error writing binconfigfile " + filename + ": "
					+ e.getMessage());
		}
	}

	public void loadPulsarCubeFile(String pulsarcubefilename) {
		String line;
		int skip, bytesperrecord, bytesread, firstmjd, mjd;
		double mjdfraction;
		byte[] buffer;
		FileChannel channel;
		ByteBuffer mainbuffer, mjdbuffer, fractionbuffer;
		FloatBuffer floatbuffer;
		DoubleBuffer doublebuffer;
		IntBuffer intbuffer;
		skip = 0;
		firstmjd = 0;
		this.pulsarcubefilename = pulsarcubefilename;

		try {
			// read in the info and store it - first the ascii header
			input = new BufferedReader(new FileReader(pulsarcubefilename));
			line = getInputLine(input, "NUM CHANNELS:");
			skip += DiFXManager.KEYWORD_LENGTH + line.length() + 1;
			numchannels = Integer.valueOf(line);
			axisvalues[2] = new double[numchannels];
			for (int i = 0; i < numchannels; i++)
				axisvalues[2][i] = i;
			line = getInputLine(input, "NUM PULSAR BINS:");
			skip += DiFXManager.KEYWORD_LENGTH + line.length() + 1;
			numpbins = Integer.valueOf(line);
			axisvalues[3] = new double[numpbins];
			for (int i = 0; i < numpbins; i++) {
				line = getInputLine(input, "BIN");
				skip += DiFXManager.KEYWORD_LENGTH + line.length() + 1;
				axisvalues[3][i] = Double.valueOf(line);
			}
			line = getInputLine(input, "NUM FREQS:");
			skip += DiFXManager.KEYWORD_LENGTH + line.length() + 1;
			numfreqs = Integer.valueOf(line);
			line = getInputLine(input, "NUM BANDS:");
			skip += DiFXManager.KEYWORD_LENGTH + line.length() + 1;
			numbands = Integer.valueOf(line);
			axisvalues[1] = new double[numbands];
			System.out.println("The number of bands is " + numbands);
			for (int i = 0; i < numbands; i++) {
				line = getInputLine(input, "BAND");
				skip += DiFXManager.KEYWORD_LENGTH + line.length() + 1;
				axisvalues[1][i] = Double.valueOf(line);
			}
			line = getInputLine(input, "BANDWIDTH");
			skip += DiFXManager.KEYWORD_LENGTH + line.length() + 1;
			bandwidth = Double.valueOf(line);
			line = getInputLine(input, "NUM POLYCO FILES");
			skip += DiFXManager.KEYWORD_LENGTH + line.length() + 1;
			numpolycofiles = Integer.valueOf(line);
			polycofilenames = new String[numpolycofiles];
			for (int i = 0; i < numpolycofiles; i++) {
				polycofilenames[i] = getInputLine(input, "POLYCO FILE");
				skip += DiFXManager.KEYWORD_LENGTH
						+ polycofilenames[i].length() + 1;
			}
			stationname = getInputLine(input, "STATION NAME");
			skip += DiFXManager.KEYWORD_LENGTH + stationname.length() + 1;
			line = getInputLine(input, "TSYS");
			skip += DiFXManager.KEYWORD_LENGTH + line.length() + 1;
			tsys = Double.valueOf(line);
			line = getInputLine(input, "INT TIME (SEC)");
			skip += DiFXManager.KEYWORD_LENGTH + line.length() + 1;
			inttime = Double.valueOf(line);
			input.close();

			// read through to work out how many time chunks there are
			bytesperrecord = 4 * numbands * numchannels * numpbins + 12;
			bytesread = bytesperrecord;
			buffer = new byte[bytesperrecord];
			binaryinput = new DataInputStream(new BufferedInputStream(
					new FileInputStream(pulsarcubefilename)));
			binaryinput.skip(skip);
			while (bytesread == bytesperrecord) {
				bytesread = binaryinput.read(buffer, 0, bytesperrecord);
				numrecords++;
			}
			numrecords--;
			binaryinput.close();

			// create the data storage areas
			rawcube = new float[numrecords][numbands][numchannels][numpbins];
			axisvalues[0] = new double[numrecords];

			// now read the binary data
			channel = (new FileInputStream(pulsarcubefilename)).getChannel();
			channel.position(skip);
			mainbuffer = ByteBuffer.allocate(bytesperrecord - 12);
			mjdbuffer = ByteBuffer.allocate(4);
			fractionbuffer = ByteBuffer.allocate(8);

			// binaryinput = new DataInputStream(new BufferedInputStream(new
			// FileInputStream(pulsarcubefilename)));
			// binaryinput.skip(skip);
			for (int i = 0; i < numrecords; i++) {
				mjdbuffer.clear();
				fractionbuffer.clear();
				bytesread = channel.read(mjdbuffer);
				if (bytesread != 4)
					System.out.println("Bytes read was only " + bytesread
							+ ", not 4!!!");
				bytesread = channel.read(fractionbuffer);
				if (bytesread != 8)
					System.out.println("Bytes read was only " + bytesread
							+ ", not 8!!!");
				mjdbuffer.order(ByteOrder.LITTLE_ENDIAN);
				fractionbuffer.order(ByteOrder.LITTLE_ENDIAN);
				mjdbuffer.position(0);
				fractionbuffer.position(0);
				intbuffer = mjdbuffer.asIntBuffer();
				doublebuffer = fractionbuffer.asDoubleBuffer();
				System.out.println("Remaing in the two buffers is "
						+ intbuffer.remaining() + ", "
						+ doublebuffer.remaining());
				mjd = intbuffer.get(0);
				mjdfraction = doublebuffer.get(0);

				mainbuffer.clear();
				bytesread = channel.read(mainbuffer);
				mainbuffer.position(0);
				if (bytesread != bytesperrecord - 12)
					System.out
							.println("Error - did not read sufficient bytes from file - only got "
									+ bytesread + "/" + bytesperrecord + "!!!");
				mainbuffer.order(ByteOrder.LITTLE_ENDIAN);
				floatbuffer = mainbuffer.asFloatBuffer();

				// mjd = binaryinput.readInt();
				// mjdfraction = binaryinput.readDouble();
				if (i == 0)
					firstmjd = mjd;
				axisvalues[0][i] = (mjd - firstmjd) + mjdfraction;
				for (int j = 0; j < numbands; j++) {
					for (int k = 0; k < numchannels; k++) {
						for (int l = 0; l < numpbins; l++) {
							/*
							 * templong = binaryinput.readLong(); reversedlong =
							 * 0; for(int t=0;t<8;t++) reversedlong |=
							 * ((templong >> t*8) & 0xff) << (7-t)*8;
							 * rawcube[i][j][k][l] =
							 * Double.longBitsToDouble(reversedlong);
							 * System.out.println("Just read in " +
							 * rawcube[i][j][k][l]); //rawcube[i][j][k][l] =
							 * binaryinput.readDouble();
							 */
							rawcube[i][j][k][l] = floatbuffer.get();
						}
					}
				}
			}
			binaryinput.close();
		} catch (IOException e) {
			System.err.println("Error parsing pulsarcubefile: "
					+ e.getMessage());
		}
	}

	public double[] getAxisValues(int axis) {
		return axisvalues[axis];
	}

	public void calculate3DPlotValues(int axis1, int index1, int axis2,
			int index2) {
		System.out.println("xindex is " + xindex + ", yindex is " + yindex);
		System.out.println("xlength is " + axisvalues[xindex].length);
		System.out.println("ylength is " + axisvalues[yindex].length);
		xvalues = axisvalues[xindex];
		yvalues = axisvalues[yindex];
		counterstart[xindex] = 0;
		counterlength[xindex] = xvalues.length;
		counterstart[yindex] = 0;
		counterlength[yindex] = yvalues.length;
		if (index1 < 0) { // want to average
			counterstart[axis1] = 0;
			counterlength[axis1] = axisvalues[axis1].length;
		} else {
			counterstart[axis1] = index1;
			counterlength[axis1] = 1;
		}

		if (index2 < 0) { // want to average
			counterstart[axis2] = 0;
			counterlength[axis2] = axisvalues[axis2].length;
		} else {
			counterstart[axis2] = index2;
			counterlength[axis2] = 1;
		}

		zvalues = new double[yvalues.length][xvalues.length];
		for (int i = 0; i < xvalues.length; i++) {
			for (int j = 0; j < yvalues.length; j++) {
				zvalues[j][i] = 0.0;
			}
		}

		for (counter[0] = counterstart[0]; counter[0] < (counterstart[0] + counterlength[0]); counter[0]++) {
			for (counter[1] = counterstart[1]; counter[1] < (counterstart[1] + counterlength[1]); counter[1]++) {
				for (counter[2] = counterstart[2]; counter[2] < (counterstart[2] + counterlength[2]); counter[2]++) {
					for (counter[3] = counterstart[3]; counter[3] < (counterstart[3] + counterlength[3]); counter[3]++) {
						zvalues[counter[yindex]][counter[xindex]] += rawcube[counter[0]][counter[1]][counter[2]][counter[3]];
						// System.out.println("Just added " +
						// rawcube[counter[0]][counter[1]][counter[2]][counter[3]]);
					}
				}
			}
		}
	}

	public double[][] getZValues() {
		return zvalues;
	}

	public double[] getXValues() {
		return xvalues;
	}

	public double[] getYValues() {
		return yvalues;
	}

	public double[] getBinConfigPhases() {
		return binconfigphases;
	}

	public double[] getBinConfigWeights() {
		return binconfigweights;
	}

	public void setBinConfigPhases(double[] binconfigphases) {
		this.binconfigphases = binconfigphases;
	}

	public void setBinConfigWeights(double[] binconfigweights) {
		this.binconfigweights = binconfigweights;
	}

	public int getNumConfigPBins() {
		return numconfigpbins;
	}

	public void setXIndex(int xindex) {
		this.xindex = xindex;
	}

	public void setYIndex(int yindex) {
		this.yindex = yindex;
	}

	private String getInputLine(BufferedReader input, String keyword)
			throws IOException {
		String toreturn = null;
		// these two println's should be exceptions!

		toreturn = input.readLine();
		if (toreturn.startsWith(keyword))
			toreturn = toreturn.substring(DiFXManager.KEYWORD_LENGTH);
		else
			throw new IOException(
					"Error - expected to read something starting with "
							+ keyword + " when instead we got"
							+ toreturn.substring(0, 21));

		return toreturn;
	}

	private void writeOutputLine(BufferedWriter output, String keyword,
			String value) throws IOException {
		output.write(keyword);
		for (int i = 0; i < DiFXManager.KEYWORD_LENGTH - keyword.length(); i++)
			output.append(' ');
		output.write(value + '\n');
	}
}
