package difx;

import javax.swing.JFrame;

public class DiFXManager
{

    public static final int INITIAL_FRAME_WIDTH = 1200;

    public static final int INITIAL_FRAME_HEIGHT = 800;

    public static final int KEYWORD_LENGTH = 20;

    /**
     * @param args
     */
    public static void main(String[] args)
    {

        // TODO Stop from closing without checking
        DiFXgui difx = new DiFXgui();

        difx.setSize(INITIAL_FRAME_WIDTH, INITIAL_FRAME_HEIGHT);
        difx.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
        difx.setVisible(true);
    }

}
