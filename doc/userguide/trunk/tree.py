#!/bin/env python

# written while watching 2008 olympics

import cairo
from string import split, strip
from math import pi
from sys import argv

WIDTH, HEIGHT = 700, 960

fontSize = 15
cornerRadius = 6
extraSpace = 5
deltaX = 5
deltaY = 5
scale = 1

def numspaces(str):
	n = 0
	for s in str:
		if s == ' ':
			n += 1
		else:
			return n

def loaddata(fn):
	lines = open(fn).readlines()
	lastn = -1
	add = {}
	for l in lines:
		n = numspaces(l)
		s = split(strip(l))
		if len(s) == 0:
			continue
		dir = s[0]
		if len(s) < 2:
			info = ''
		else:
			info = strip(l[n+len(dir):])
		data = [dir, info]
		if lastn == -1:
			add[0] = data
		else:
			if n > lastn:
				add[n] = last
			add[n].append(data)
		lastn = n
		last = data
			
	return add[0]

def drawtree(ctx, x, y, data):
	if len(data) < 2:
		return
	ctx.move_to(x, y)
	ctx.select_font_face("monospace", 0, 1)
	ctx.show_text(data[0]+' ')
	ctx.select_font_face("serif", 0, 0)
	comments = split(data[1], "\\");
	delta = 0
	x0, y0 = ctx.get_current_point()
	for c in comments:
		ctx.move_to(x0, y0)
		k = split(strip(c), '"')
		l = len(k)
		for i in range(l):
			if i%2 == 0:
				ctx.select_font_face("serif", 0, 0)
			else:
				ctx.select_font_face("monospace", 0, 0)
			ctx.show_text(k[i])
		delta += fontSize+2
		y0 = y + delta
	delta += extraSpace

	y0 = y+deltaY
	y += delta
	daughters = data[2:]
	n = len(daughters)
	for i in range(n):
		v = daughters[i]
		ctx.move_to(x+deltaX, y0)
		y0 = y-deltaY
		if i < n-1:
			ctx.line_to(x+deltaX, y0)
			ctx.line_to(x+3*deltaX, y0)
		else:
			ctx.arc_negative(x+deltaX+cornerRadius, y0-cornerRadius, cornerRadius, pi, pi/2.)
			ctx.line_to(x+3*deltaX, y0)
		ctx.stroke()
		y = drawtree(ctx, x+4*deltaX, y, v)
	return y

# Setup Cairo
#surface = cairo.ImageSurface(cairo.FORMAT_ARGB32, WIDTH, HEIGHT)
surface = cairo.PDFSurface(argv[2], WIDTH, HEIGHT)
ctx = cairo.Context(surface)
ctx.scale(scale, scale)
ctx.set_font_size(fontSize)

data = loaddata(argv[1])

drawtree(ctx, 10, 10+fontSize, data)

#surface.write_to_png("tree.png")
