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


import javax.swing.JFrame;

public class DiFXManager {

	public static final int INITIAL_FRAME_WIDTH = 1200;

	public static final int INITIAL_FRAME_HEIGHT = 800;

	public static final int KEYWORD_LENGTH = 20;

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		// TODO Stop from closing without checking
		DiFXgui difx = new DiFXgui();
		difx.setSize(INITIAL_FRAME_WIDTH, INITIAL_FRAME_HEIGHT);
		difx.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
		difx.setVisible(true);
	}

}
