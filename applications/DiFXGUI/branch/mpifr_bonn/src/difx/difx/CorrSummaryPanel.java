package difx;

import java.awt.Dimension;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import javax.swing.Box;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import java.awt.BorderLayout;
import javax.swing.JTree;
import javax.swing.JSplitPane;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.TreePath;
import java.awt.Color;
import java.awt.Component;

public class CorrSummaryPanel
    extends DiFXPanel implements ComponentListener
{

    private JTextArea[] sectionsummaries;
    private int selectedNodeIndex = -1;
    private JScrollPane scrollpane;
    private static int NUM_SECTION_SUMMARIES = 5;
    Box displaybox = Box.createVerticalBox();
    JTextArea txtSection1 = new JTextArea();
    JTextArea txtSection2 = new JTextArea();
    JTextArea txtSection3 = new JTextArea();
    JTextArea txtSection4 = new JTextArea();
    JTextArea txtSection5 = new JTextArea();

    JSplitPane splitpane = new JSplitPane();
    JTree trvIndex = null;

    /**
     * Constructor
     * @param parent DiFXgui
     * @param corrconfig CorrelationConfig
     */
    public CorrSummaryPanel(DiFXgui parent, CorrelationConfig corrconfig)
    {

        super(parent, corrconfig);
        try
        {
            this.addComponentListener(this);
            scrollpane = new JScrollPane(displaybox);

            jbInit();

            sectionsummaries = new JTextArea[NUM_SECTION_SUMMARIES];
            sectionsummaries[0] = txtSection1;
            sectionsummaries[1] = txtSection2;
            sectionsummaries[2] = txtSection3;
            sectionsummaries[3] = txtSection4;
            sectionsummaries[4] = txtSection5;

            trvIndex.setEnabled(false);

        }

        catch (Exception ex)
        {
            ex.printStackTrace();
        }

    }

    private void displaysummary()
    {
        mainpanel.removeAll();
        mainpanel.repaint();

        if (corrconfig.getNumConfigsLoaded() > 0)
        {

            txtSection1.setText(corrconfig.getCommonSummary());
            txtSection2.setText(corrconfig.getConfigSummary());
            txtSection3.setText(corrconfig.getTelescopeSummary());
            txtSection4.setText(corrconfig.getDatastreamSummary());
            txtSection5.setText(corrconfig.getBaselineSummary());

            trvIndex.setEnabled(true);
        }

    }

    @Override
    public void commitcorrchanges()
    {
        // TODO Auto-generated method stub

    }

    @Override
    public void refreshdisplay()
    {
        // TODO Auto-generated method stub
        if (corrconfig != null)
        {
            displaysummary();

        }

        if (txtSection1.getText() == "")
        {
            trvIndex.setEnabled(false);
        }
        else
        {
            trvIndex.setEnabled(true);

        }

    }

    public void componentHidden(ComponentEvent e)
    {
    }

    public void componentMoved(ComponentEvent e)
    {
    }

    public void componentShown(ComponentEvent e)
    {
    }

    public void componentResized(ComponentEvent e)
    {
        if (e.getSource() == this)
        {
            scrollpane.setPreferredSize(mainpanel.getSize());
            doLayout();
        }
    }

    private void jbInit() throws Exception
    {
        trvIndex = new JTree(fillTreeview());
        displaybox = Box.createVerticalBox();
        this.setMinimumSize(new Dimension(100, 46));
        this.setPreferredSize(new Dimension(100, 37));
        txtSection1.setEditable(false);
        txtSection1.setText("");
        txtSection2.setEditable(false);
        txtSection2.setText("");
        txtSection3.setEditable(false);
        txtSection3.setText("");
        txtSection4.setEditable(false);
        txtSection4.setText("");
        txtSection5.setEditable(false);
        txtSection5.setText("");
        scrollpane.setMinimumSize(new Dimension(222, 22));
        scrollpane.setPreferredSize(new Dimension(600, 93));
        trvIndex.addTreeSelectionListener(new
                                          CorrSummaryPanel_trvIndex_treeSelectionAdapter(this));
        trvIndex.setRootVisible(false);
        displaybox.add(txtSection1);
        displaybox.add(txtSection2);
        displaybox.add(txtSection3);
        displaybox.add(txtSection4);
        displaybox.add(txtSection5);

        this.add(splitpane, java.awt.BorderLayout.CENTER);
        splitpane.add(scrollpane, JSplitPane.RIGHT);
        splitpane.add(trvIndex, JSplitPane.LEFT);
        scrollpane.getViewport().add(displaybox);
    }

    /**
     * Fills the node object with the navigations treeview nodes
     * @return DefaultMutableTreeNode
     */
    private DefaultMutableTreeNode fillTreeview()
    {
        DefaultMutableTreeNode rootNode = new DefaultMutableTreeNode();
        DefaultMutableTreeNode node = null;

        node = new DefaultMutableTreeNode("Common");
        rootNode.add(node);
        node = new DefaultMutableTreeNode("Config");
        rootNode.add(node);
        node = new DefaultMutableTreeNode("Telescope");
        rootNode.add(node);
        node = new DefaultMutableTreeNode("Datastream");
        rootNode.add(node);
        node = new DefaultMutableTreeNode("Baseline");
        rootNode.add(node);

        rootNode.add(node);

        return (rootNode);
    }

    /**
     * Highlights the section selected from the treeview. The scrollbar position is set
     * to the beginning of the currently selected section
     */
    private void markSelectedSection()
    {

        if ( (selectedNodeIndex < 0) ||
            (selectedNodeIndex > NUM_SECTION_SUMMARIES))
        {
            return;
        }
        int lineCount = 0;
        scrollpane.getVerticalScrollBar().setValue(0);

        for (int i = 0; i < selectedNodeIndex; i++)
        {
            lineCount += sectionsummaries[i].getHeight();
        }

        scrollpane.getVerticalScrollBar().setValue(lineCount);

        txtSection1.setBackground(Color.white);
        txtSection2.setBackground(Color.white);
        txtSection3.setBackground(Color.white);
        txtSection4.setBackground(Color.white);
        txtSection5.setBackground(Color.white);

        txtSection1.setForeground(Color.black);
        txtSection2.setForeground(Color.black);
        txtSection3.setForeground(Color.black);
        txtSection4.setForeground(Color.black);
        txtSection5.setForeground(Color.black);

        sectionsummaries[selectedNodeIndex].setBackground(Color.getHSBColor(0,
            0, (float) 0.95));
        sectionsummaries[selectedNodeIndex].setForeground(Color.red);

    }

    public void trvIndex_valueChanged(TreeSelectionEvent e)
    {

        TreePath treePath = trvIndex.getSelectionPath();
        int selectedNodes[] = trvIndex.getSelectionRows();

        if (selectedNodes.length == 1)
        {
            selectedNodeIndex = selectedNodes[0];
            markSelectedSection();
        }

    }
}

class CorrSummaryPanel_trvIndex_treeSelectionAdapter
    implements TreeSelectionListener
{
    private CorrSummaryPanel adaptee;
    CorrSummaryPanel_trvIndex_treeSelectionAdapter(CorrSummaryPanel adaptee)
    {
        this.adaptee = adaptee;
    }

    public void valueChanged(TreeSelectionEvent e)
    {
        adaptee.trvIndex_valueChanged(e);
    }
}
