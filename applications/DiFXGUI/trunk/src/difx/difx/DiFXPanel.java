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

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingConstants;


public abstract class DiFXPanel extends JPanel implements ActionListener {
	protected JButton commitchangebutton, consistencycheckbutton, launchbutton;
	protected JScrollPane scrollpane;
	protected CorrelationConfig corrconfig;
	protected JPanel mainpanel;
	protected boolean updated;
	protected GridLayout onecollayout, twocollayout;
	protected DiFXgui parent;

	public DiFXPanel(DiFXgui parent, CorrelationConfig corrconfig) {
		this.parent = parent;
		this.corrconfig = corrconfig;
		this.setLayout(new BorderLayout());
		twocollayout = new GridLayout(1,2);
		onecollayout = new GridLayout(0,1);
		JPanel northpanel = new JPanel();
		northpanel.setLayout(new GridLayout(1,3,10,10));
		mainpanel = new JPanel();
		scrollpane = new JScrollPane(mainpanel);
		commitchangebutton = new JButton("Save current config");
		commitchangebutton.addActionListener(this);
		consistencycheckbutton = new JButton("Check config consistency");
		consistencycheckbutton.addActionListener(this);
		launchbutton = new JButton("Launch current correlation");
		launchbutton.addActionListener(this);
		northpanel.add(consistencycheckbutton);
		northpanel.add(commitchangebutton);
		northpanel.add(launchbutton);
		add(scrollpane);
		add(northpanel, "North");
		updated = true;
	}
	
	public boolean isUpdated() {
		return updated;
	}

	public abstract void refreshdisplay();

	///Save the currently displayed parameters to the CorrelationConfig.  Must set updated to true.
	public abstract void commitcorrchanges();
	
	protected void addInputField(JPanel target, Component toadd, String label) {
		addInputField(target, toadd, label, null);
	}
	
	protected void addInputField(JPanel target, Component toadd, String label, JButton button) {
		JPanel temp = new JPanel(twocollayout);
		if(button == null)
			temp.add(new JLabel(label, SwingConstants.RIGHT));
		else {
			JPanel left = new JPanel(new BorderLayout());
			left.add(button, "West");
			left.add(new JLabel(label, SwingConstants.RIGHT));
			temp.add(left);
		}
		temp.add(toadd);
		target.add(temp);
	}

	public void actionPerformed(ActionEvent e) {
		int choice;
		Object source = e.getSource();
		if (source == commitchangebutton)
			commitcorrchanges();
		else if (source == consistencycheckbutton) {
			try {
				corrconfig.consistencyCheck(true);
				JOptionPane.showMessageDialog(this, "Current configuration has no inconsistencies", "Success!!!", JOptionPane.INFORMATION_MESSAGE);
			}
			catch(ConsistencyException err) {
				JOptionPane.showMessageDialog(this, "Inconsistency found: " + err.getMessage(), "Consistency Error!!!", JOptionPane.ERROR_MESSAGE);
			}
		}
		else if(source == launchbutton) {
			System.out.println("Going to launch!!!");
			if(!updated) {
				choice = JOptionPane.showConfirmDialog(this, "Save changes from current page to correlation configuration before launching correlation?", 
						"Save changes?", JOptionPane.YES_NO_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE);
				if(choice == JOptionPane.CANCEL_OPTION)
					return;
				if(choice == JOptionPane.YES_OPTION)
					commitcorrchanges();
			}
				
			parent.launchcorrelation();
		}
	}
}
