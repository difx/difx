package difx;


import java.awt.Dimension;
import java.awt.Font;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;

import javax.swing.Box;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;


public class CorrSummaryPanel extends DiFXPanel implements ComponentListener {
	private JTextArea[] sectionsummaries;

	private JScrollPane scrollpane;

	private Box displaybox;
	
	private static int NUM_SECTION_SUMMARIES = 5;

	public CorrSummaryPanel(DiFXgui parent, CorrelationConfig corrconfig) {
		super(parent, corrconfig);
		this.addComponentListener(this);
		sectionsummaries = new JTextArea[NUM_SECTION_SUMMARIES];
		displaybox = Box.createVerticalBox();
		for (int i = 0; i < NUM_SECTION_SUMMARIES; i++) {
			sectionsummaries[i] = new JTextArea();
			sectionsummaries[i].setEditable(false);
			sectionsummaries[i].setFont(new Font("Monospaced", Font.PLAIN, 12));
			displaybox.add(sectionsummaries[i]);
			displaybox.add(Box.createVerticalStrut(10));
		}
		scrollpane = new JScrollPane(displaybox);
		scrollpane
				.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
		scrollpane
				.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
	}

	private void displaysummary() {
		mainpanel.removeAll();
		mainpanel.repaint();

		if (corrconfig.getNumConfigsLoaded() > 0) {
			sectionsummaries[0].setText(corrconfig.getCommonSummary());
			sectionsummaries[1].setText(corrconfig.getConfigSummary());
			sectionsummaries[2].setText(corrconfig.getTelescopeSummary());
			sectionsummaries[3].setText(corrconfig.getDatastreamSummary());
			sectionsummaries[4].setText(corrconfig.getBaselineSummary());

			mainpanel.add(scrollpane);
		}
	}

	@Override
	public void commitcorrchanges() {
		// TODO Auto-generated method stub

	}

	@Override
	public void refreshdisplay() {
		// TODO Auto-generated method stub
		if (corrconfig != null)
			displaysummary();
	}

	public void componentHidden(ComponentEvent e) {
	}

	public void componentMoved(ComponentEvent e) {
	}

	public void componentShown(ComponentEvent e) {
	}

	public void componentResized(ComponentEvent e) {
		if (e.getSource() == this) {
			scrollpane.setPreferredSize(mainpanel.getSize());
			doLayout();
		}
	}
}
