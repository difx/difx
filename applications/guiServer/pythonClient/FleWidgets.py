################################################################################
#
#  These are widgets for doing things in pyFltk.
#
################################################################################
import fltk

class Progress( fltk.Fl_Widget ):
	def __init__( self, x, y, w, h, label = None ):
		fltk.Fl_Widget.__init__( self, x, y, w, h, label )
		self._minimum = 0.0
		self._maximum = 1.0
		self._showPercentage = True
		self._textColor = fltk.FL_BLACK
		self._value = None
			
	def minimum( self, newVal ):
		self._minimum = newVal
		
	def maximum( self, newVal ):
		self._maximum = newVal
		
	def value( self, newVal ):
		self._value = newVal
		
	def showPercentage( self, newVal ):
		self._showPercentage = newVal
		
	def textColor( self, newVal ):
		self._textColor = newVal

	def draw( self ):
		fltk.fl_push_clip( self.x(), self.y(), self.w(), self.h() )
		#  Clear the background
		fltk.fl_color( self.color() )
		fltk.fl_rectf( self.x(), self.y(), self.w(), self.h() )
		#  Draw an appropriate bar using the "selection color"
		if self._value != None:
			fltk.fl_color( self.selection_color() )
			fltk.fl_rectf( self.x(), self.y(), int( self.w() * ( self._value - self._minimum ) / ( self._maximum - self._minimum ) ), self.h() )
			#  Draw a percentage if that's what the user wants.
			if self._showPercentage:
				fltk.fl_color( self._textColor )
				percentage = int( 0.5 + 100.0 * ( self._value - self._minimum ) / ( self._maximum - self._minimum ) )
				labelStr = str( percentage ) + "%"
				strWidth = int( fltk.fl_width( labelStr.encode() ) / 2.0 )
				fltk.fl_draw( labelStr.encode(), self.x() + ( self.w() ) / 2 - strWidth, self.y() + self.h() - 6 )
		#  Frame the thing.
		fltk.fl_draw_box( self.box(), self.x(), self.y(), self.w(), self.h(), fltk.FL_GRAY )
		fltk.fl_pop_clip()

