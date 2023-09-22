#!/bin/env python

from pyx import *

def makenumber(number, filename):
	c = canvas.canvas()
	circ = path.circle(0, 0, 1)
	if len(number) > 3:
		c.fill(circ, [trafo.scale(sx=0.6, sy=0.2)])
	else:
		c.fill(circ, [trafo.scale(sx=0.4, sy=0.2)])
	c.text(0, 0, number, [text.valign.middle, text.halign.center, color.rgb.white])
	c.writePDFfile(filename)

def makeblank(filename):
	c = canvas.canvas()
	circ = path.circle(0, 0, 1)
	c.fill(circ, [trafo.scale(sx=0.4, sy=0.2), color.rgb.white])
	c.writePDFfile(filename)
	

makenumber("1.1", "11.pdf")
makenumber("1.2", "12.pdf")
makenumber("1.5", "15.pdf")
makenumber("2.0", "20.pdf")
makenumber("2.0.1", "201.pdf")
makeblank('blank.pdf')
