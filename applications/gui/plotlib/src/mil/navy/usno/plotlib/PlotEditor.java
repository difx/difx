/***************************************************************************
 *   Copyright (C) 2016 by John Spitzak                                    *
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
/*
 * This panel contains all of the controls used to edit a 2D plot.
 */
package mil.navy.usno.plotlib;

import java.awt.Color;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import javax.swing.BorderFactory;
import javax.swing.border.TitledBorder;

import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.JLabel;
import javax.swing.JTabbedPane;
import javax.swing.JCheckBox;

import mil.navy.usno.widgetlib.IndexedPanel;
import mil.navy.usno.widgetlib.NodeBrowserScrollPane;

/**
 *
 * @author jspitzak
 */
public class PlotEditor extends NodeBrowserScrollPane {
    
    public PlotEditor( Plot2DObject thisPlot ) {
        super();
        this.setLayout( null );
        _thisPlot = thisPlot;
        
        //  Global properties
        IndexedPanel globalPanel = new IndexedPanel( "Global Properties" );
        //globalPanel.setLayout( null );
        //globalPanel.setBounds( 5, 10, 485, 95 );
        //globalPanel.setBorder( BorderFactory.createTitledBorder( "Global Properties" ) );
        globalPanel.openHeight( 95 );
        globalPanel.closedHeight( 20 );
        this.addNode( globalPanel );
        //  Name field - the name is used to organize plots.
        //_name = new JTextField( thisPlot.name() );
        _name = new JTextField( "" );
        _name.setBounds( 60, 20, 150, 20 );
        _name.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                nameChange();
            }
        });
        globalPanel.add( _name );
        JLabel nameLabel = new JLabel( "name:" );
        nameLabel.setBounds( 10, 20, 45, 20 );
        nameLabel.setHorizontalAlignment( JLabel.RIGHT );
        _name.setToolTipText( "The plot \"name\" is used for internal reference - not to be confused with the title." );
        nameLabel.setToolTipText( "The plot \"name\" is used for internal reference - not to be confused with the title." );
        globalPanel.add( nameLabel );
        
        //  The plot can quickly be made invisible with the visible checkbox.
        _visibleCheck = new JCheckBox();
        _visibleCheck.setSelected( thisPlot.visible() );
        _visibleCheck.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                visibleCheck();
            }
        });
        _visibleCheck.setBounds( 400, 20, 20, 20 );
        _visibleCheck.setToolTipText( "Include/remove this plot in/from the drawing list.  If not included, it will not be drawn." );
        globalPanel.add( _visibleCheck );
        JLabel visibleLabel = new JLabel( "Visible" );
        visibleLabel.setBounds( 425, 20, 100, 20 );
        visibleLabel.setToolTipText( "Include/remove this plot in/from the drawing list.  If not included, it will not be drawn." );
        globalPanel.add( visibleLabel );
        
        //  The plot title
        IndexedPanel titlePanel = new IndexedPanel( "Title" );
        //titlePanel.setLayout( null );
        //titlePanel.setBounds( 5, 110, 485, 100 );
        //titlePanel.setBorder( BorderFactory.createTitledBorder( "Title" ) );
        titlePanel.openHeight( 100 );
        titlePanel.closedHeight( 20 );
        this.addNode( titlePanel );
        _title = new JTextField( "" );
        _title.setBounds( 20, 20, 380, 20 );
        _title.setToolTipText( "The string used for the plot title.  This can be a complex string with embedded commands." );
        _title.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                titleChange();
            }
        });
        titlePanel.add( _title );
        _titleJustification = new JustificationButtons();
        _titleJustification.setBounds( 402, 20, 60, 20 );
        _titleJustification.setToolTipText( "Left, center, or right justification of the title text on the specified position." );
        _titleJustification.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                titleJustificationChange();
            }
        });
        titlePanel.add( _titleJustification );
        _titleLocationX = new PixelLocation();
        _titleLocationX.setBounds( 40, 45, 160, 20 );
        _titleLocationX.setPixelsToolTip( "X Position of the title measured in pixels from the left side of the plot." );
        _titleLocationX.setFractionToolTip( "X Position of the title as a fraction of the plot frame width." );
        _titleLocationX.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                titleLocationChange();
            }
        });
        titlePanel.add( _titleLocationX );
        JLabel titleLocXLabel = new JLabel( "x:" );
        titleLocXLabel.setBounds( 10, 45, 25, 20 );
        titleLocXLabel.setHorizontalAlignment( JLabel.RIGHT );
        titleLocXLabel.setToolTipText( "Title X position relative to the left side of the plot.  This number can be specified in pixels or as a fraction of the plot width." );
        titlePanel.add( titleLocXLabel );
        _titleLocationY = new PixelLocation();
        _titleLocationY.setBounds( 40, 70, 160, 20 );
        _titleLocationY.setPixelsToolTip( "Y Position of the title measured in pixels from the top of the plot (up is positive)." );
        _titleLocationY.setFractionToolTip( "Y Position of the title as a fraction of the plot frame height (up is positive)." );
        _titleLocationY.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                titleLocationChange();
            }
        });
        titlePanel.add( _titleLocationY );
        JLabel titleLocYLabel = new JLabel( "y:" );
        titleLocYLabel.setBounds( 10, 70, 25, 20 );
        titleLocYLabel.setHorizontalAlignment( JLabel.RIGHT );
        titleLocYLabel.setToolTipText( "Title Y position relative to the top of the plot (up is positive).  This number can be specified in pixels or as a fraction of the plot height." );
        titlePanel.add( titleLocYLabel );
        _titleColor = new CheckedColorButton();
        _titleColor.setBounds( 382, 45, 80, 20 );
        _titleColor.colorToolTip( "This button shows the color for the title text.  Pressing the button launches a color chooser." );
        _titleColor.checkToolTip( "Apply the color to the title text.  If not checked, the iherited color will be used." );
        _titleColor.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                titleColorChange();
            }
        });
        titlePanel.add( _titleColor );
        JLabel titleColorLabel = new JLabel( "color:" );
        titleColorLabel.setBounds( 382 - 90, 45, 85, 20 );
        titleColorLabel.setHorizontalAlignment( JLabel.RIGHT );
        titleColorLabel.setToolTipText( "Title text color.  The checkbox indicates whether the color is applied or if existing color is used." );
        titlePanel.add( titleColorLabel );
        _titleFont = new CheckedFontSetting();
        _titleFont.setBounds( 252, 70, 210, 20 );
        _titleFont.fontToolTipName( "Title Font" );
        _titleFont.checkToolTip( "Apply the font settings to the Title Font.  If not checked, the inherited font, whatever that is, will be used." );
        _titleFont.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                titleFontChange();
            }
        });
        titlePanel.add( _titleFont );
        JLabel titleFontLabel = new JLabel( "font:" );
        titleFontLabel.setBounds( 252 - 50, 70, 45, 20 );
        titleFontLabel.setHorizontalAlignment( JLabel.RIGHT );
        titleFontLabel.setToolTipText( "Title font.  The checkbox indicates whether the font is applied or if inherited color is used." );
        titlePanel.add( titleFontLabel ); 
        
        //  Frame properties
        IndexedPanel framePanel = new IndexedPanel( "Frame" );
        framePanel.openHeight( 130 );
        framePanel.closedHeight( 20 );
        this.addNode( framePanel );
        //  These numbers specify the location and size of the frame of the plot.
        //  There are two boxes for each number - one measurement in pixels, and
        //  one as a fraction of total window size.
        _frameX = new PixelLocation();
        _frameX.setBounds( 40, 20, 160, 20 );
        _frameX.setPixelsToolTip( "X Position of the plot frame measured in pixels from the left side of the window." );
        _frameX.setFractionToolTip( "X Position of the plot frame as a fraction of the window width." );
        _frameX.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                frameChange();
            }
        });
        framePanel.add( _frameX );
        JLabel frameXLabel = new JLabel( "x:" );
        frameXLabel.setBounds( 10, 20, 25, 20 );
        frameXLabel.setHorizontalAlignment( JLabel.RIGHT );
        frameXLabel.setToolTipText( "Plot frame X position relative to the left side of the window.  This number can be specified in pixels or as a fraction of the window width.  The latter changes with changes in the window size, the former is fixed." );
        framePanel.add( frameXLabel );
        _frameY = new PixelLocation();
        _frameY.setBounds( 40, 45, 160, 20 );
        _frameY.setPixelsToolTip( "Y Position of the plot frame measured in pixels from the top of the window (down is positive)." );
        _frameY.setFractionToolTip( "Y Position of the plot frame measured from the top of the window as a fraction of the window height (down is positive)." );
        _frameY.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                frameChange();
            }
        });
        framePanel.add( _frameY );
        JLabel frameYLabel = new JLabel( "y:" );
        frameYLabel.setBounds( 10, 45, 25, 20 );
        frameYLabel.setHorizontalAlignment( JLabel.RIGHT );
        frameYLabel.setToolTipText( "Plot frame Y position relative to the top of the window (down is positive).  This number can be specified in pixels or as a fraction of the window height.  The latter changes with changes in the window size, the former is fixed." );
        framePanel.add( frameYLabel );
        _frameW = new PixelLocation();
        _frameW.setBounds( 40, 70, 160, 20 );
        _frameW.setPixelsToolTip( "Width of the plot frame measured in pixels." );
        _frameW.setFractionToolTip( "Width of the plot frame measured as a fraction of the window width." );
        _frameW.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                frameChange();
            }
        });
        framePanel.add( _frameW );
        JLabel frameWLabel = new JLabel( "w:" );
        frameWLabel.setBounds( 10, 70, 25, 20 );
        frameWLabel.setHorizontalAlignment( JLabel.RIGHT );
        frameWLabel.setToolTipText( "Plot frame width.  This number can be specified in pixels or as a fraction of the window width." );
        framePanel.add( frameWLabel );
        _frameH = new PixelLocation();
        _frameH.setBounds( 40, 95, 160, 20 );
        _frameH.setPixelsToolTip( "Height of the plot frame measured in pixels." );
        _frameH.setFractionToolTip( "Height of the plot frame measured as a fraction of the window height." );
        _frameH.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                frameChange();
            }
        });
        framePanel.add( _frameH );
        JLabel frameHLabel = new JLabel( "h:" );
        frameHLabel.setBounds( 10, 95, 25, 20 );
        frameHLabel.setHorizontalAlignment( JLabel.RIGHT );
        frameHLabel.setToolTipText( "Plot frame height.  This number can be specified in pixels or as a fraction of the window height." );
        framePanel.add( frameHLabel );
        _drawFrame = new JCheckBox( "Draw Frame" );
        _drawFrame.addActionListener(new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                drawFrame();
            }
        });
        _drawFrame.setBounds( 250, 20, 20, 20 );
        _drawFrame.setToolTipText( "Include/remove a frame around this plot area." );
        framePanel.add( _drawFrame );
        JLabel drawFrameLabel = new JLabel( "Draw Frame" );
        drawFrameLabel.setBounds( 275, 20, 100, 20 );
        drawFrameLabel.setToolTipText( "Include/remove a frame around this plot area." );
        framePanel.add( drawFrameLabel );
        _frameColor = new CheckedColorButton();
        _frameColor.setBounds( 382, 20, 80, 20 );
        _frameColor.colorToolTip( "This button shows the color for the plot frame.  Pressing the button launches a color chooser." );
        _frameColor.checkToolTip( "Apply the color to the plot frame.  If not checked, the inherited color will be used." );
        _frameColor.addActionListener( new ActionListener() {
            public void actionPerformed( ActionEvent evt ) {
                frameColorChange();
            }
        });
        framePanel.add( _frameColor );
    }
    
    /*
     * Change the name of this plot to match the "name" text field.
     */
    public void nameChange() {
        _thisPlot.name( _name.getText() );
        if ( _tabbedPane != null )
            _tabbedPane.setTitleAt( _tabbedPane.indexOfComponent( this ), _name.getText() );
        if ( _plotterUI != null )
            _plotterUI.buildPlotMenu();
    }
    
    /*
     * Change the title of the plot.
     */
    public void titleChange() {
        _thisPlot.title( _title.getText(), _titleJustification.justification() );
        _plotWindow.updateUI();
    }
    
    /*
     * Change the justification of the plot title.
     */
    public void titleJustificationChange() {
        _thisPlot.title( _title.getText(), _titleJustification.justification() );
        _plotWindow.updateUI();
    }
    
    /*
     * Change the location of the plot title.
     */
    public void titleLocationChange() {
        _thisPlot.titleXPos( _titleLocationX.value(), _titleLocationX.isFraction() );
        _thisPlot.titleYPos( _titleLocationY.value(), _titleLocationY.isFraction() );
        _plotWindow.updateUI();
    }
    
    /*
     * Change the color of the plot title.
     */
    public void titleColorChange() {
        _thisPlot.titleColor( _titleColor.color(), _titleColor.apply() );
        _plotWindow.updateUI();
    }
    
    /*
     * Change the font of the plot title.
     */
    public void titleFontChange() {
        _thisPlot.titleFont( _titleFont.font() );
        _plotWindow.updateUI();
    }
    
    /*
     * Change the frame of the plot to match current settings.
     */
    public void frameChange() {
        _thisPlot.frameX( _frameX.value(), _frameX.isFraction() );
        _thisPlot.frameY( _frameY.value(), _frameY.isFraction() );
        _thisPlot.frameW( _frameW.value(), _frameW.isFraction() );
        _thisPlot.frameH( _frameH.value(), _frameH.isFraction() );
        _plotWindow.updateUI();
    }
    
    /*
     * Change whether the frame is visible or not.
     */
    public void drawFrame() {
        _thisPlot.drawFrame( _drawFrame.isSelected() );
        _plotWindow.updateUI();
    }
    
    /*
     * Change the color of the frame.
     */
    public void frameColorChange() {
        if ( _frameColor.apply() )
            _thisPlot.frameColor( _frameColor.color() );
        else
            _thisPlot.frameColor( null );
        _plotWindow.updateUI();
    }
    
    /*
     * Change the "visible" state of the plot.
     */
    public void visibleCheck() {
        _thisPlot.visible( _visibleCheck.getSelectedObjects() != null );
        if ( _plotWindow != null )
            _plotWindow.updateUI();
    }
    
    /*
     * Change the values displayed in a editor fields to match the current values
     * for the plot.
     */
    public void dataChange() {
        if ( _thisPlot != null && _plotWindow != null ) {
            _name.setText( _thisPlot.name() );
            _visibleCheck.setSelected( _thisPlot.visible() );
            _title.setText( _thisPlot.titleString() );
            _titleJustification.justification( _thisPlot.titleJustification() );
            _titleLocationX.windowSize( _thisPlot.w() );
            _titleLocationX.value( _thisPlot.titleXPos(), _thisPlot.titleXUseFraction() );
            _titleLocationY.windowSize( _thisPlot.h() );
            _titleLocationY.value( _thisPlot.titleYPos(), _thisPlot.titleYUseFraction() );
            if ( _thisPlot.titleColorSet() )
                _titleColor.color( _thisPlot.titleColor() );
            _titleColor.apply( _thisPlot.titleColorSet() );
            _titleFont.font( _thisPlot.titleFont() );
            _frameX.windowSize( _plotWindow.getWidth() );
            _frameX.value( _thisPlot.frameX(), _thisPlot.frameXUseFraction() );
            _frameY.windowSize( _plotWindow.getHeight() );
            _frameY.value( _thisPlot.frameY(), _thisPlot.frameYUseFraction() );
            _frameW.windowSize( _plotWindow.getWidth() );
            _frameW.value( _thisPlot.frameW(), _thisPlot.frameWUseFraction() );
            _frameH.windowSize( _plotWindow.getHeight() );
            _frameH.value( _thisPlot.frameH(), _thisPlot.frameHUseFraction() );
            _drawFrame.setSelected( _thisPlot.drawFrame() );
            if ( _thisPlot.frameColor() != null ) {
                _frameColor.color( _thisPlot.frameColor() );
                _frameColor.apply( true );
            }
            else
                _frameColor.apply( false );
        }
    }
    
    public void tabbedPane( JTabbedPane thisPane ) {
        _tabbedPane = thisPane;
    }
    public void plotterUI( PlotterUI thisUI ) {
        _plotterUI = thisUI;
    }
    public void plotWindow( PlotWindow thisWin ) {
        _plotWindow = thisWin;
    }
    
    public Plot2DObject plot() {
        return _thisPlot;
    }
    
    Plot2DObject _thisPlot;
    JTextField _name;
    JTextField _title;
    JTabbedPane _tabbedPane;
    JCheckBox _visibleCheck;
    PlotterUI _plotterUI;
    PlotWindow _plotWindow;
    JustificationButtons _titleJustification;
    PixelLocation _titleLocationX;
    PixelLocation _titleLocationY;
    CheckedColorButton _titleColor;
    CheckedFontSetting _titleFont;
    PixelLocation _frameX;
    PixelLocation _frameY;
    PixelLocation _frameW;
    PixelLocation _frameH;
    JCheckBox _drawFrame;
    CheckedColorButton _frameColor;
    JCheckBox _drawBackground;
    CheckedColorButton _backgroundColor;
    
}
