#! python

import sys
import os
import pprint
import hopstest as ht
import ROOT
import numpy as np
from ctypes import *
from math import *

from .processing import *

class histogram_configuration(object):

    #default values for init
    def __init__(self):
        self.nxbins = 100
        self.nybins = 100
        self.nzbins = 100
        self.auto_minmax = True
        self.xmin = 0
        self.xmax = 0
        self.ymin = 0
        self.ymax = 0
        self.zmin = 0
        self.zmax = 0
        self.draw_option = ""

def create_1d_histogram(value_list, value_name, histogram_name="", histo_config=None):

    #determine the min, max values, and the mean and std deviation
    value_max = max(value_list)
    value_min = min(value_list)
    value_mean = np.mean(value_list)
    value_stddev = np.std(value_list)

    n_bins=100
    draw_opt = ""
    if histo_config != None:
        n_bins = histo_config.nxbins
        draw_opt = histo_config.draw_option
        if histo_config.auto_minmax is False:
            value_min = histo_config.xmin
            value_max = histo_config.xmax
    
    hist_name = "hist-" + value_name
    if histogram_name != "": 
        hist_name = histogram_name
    
    canvas = ROOT.TCanvas("canvas-" + hist_name, "canvas-" + hist_name, 200, 10, 700, 500)
    canvas.cd()
    
    hist = ROOT.TH1D(hist_name, hist_name, n_bins, value_min, value_max)
    #hist.SetFillColor(color_style_list[x])
    #hist.SetFillStyle(fill_style_list[x])
    hist.SetStats(0)
    for y in value_list:
        hist.Fill(y)

    #title the histogram (must draw it first)
    hist.Draw()
    hist.GetXaxis().SetTitle(value_name)
    hist.GetXaxis().CenterTitle()
    hist.GetYaxis().SetTitle("counts")
    hist.GetYaxis().CenterTitle()
    hist.SetTitle(value_name + " distribution")
    hist.Draw()
    canvas.Update()

    return [hist, canvas]

def create_2d_histogram(value_list1, value_list2, value_name1, value_name2, histogram_name="", histo_config=None ):
    #determine the min, max values, and the mean and std deviation
    value_max1 = max(value_list1)
    value_min1 = min(value_list1)
    value_mean1 = np.mean(value_list1)
    value_stddev1 = np.std(value_list1)

    value_max2 = max(value_list2)
    value_min2 = min(value_list2)
    value_mean2 = np.mean(value_list2)
    value_stddev2 = np.std(value_list2)
    
    n_bins1=100
    n_bins2=100
    draw_opt = ""
    if histo_config != None:
        n_bins1 = histo_config.nxbins
        n_bins2 = histo_config.nybins
        draw_opt = histo_config.draw_option
        if histo_config.auto_minmax is False:
            value_min1 = histo_config.xmin
            value_max1 = histo_config.xmax
            value_min2 = histo_config.ymin
            value_max2 = histo_config.ymax
    
    hist_name = "hist-" + value_name1 + "-vs-" + value_name2
    if histogram_name != "": 
        hist_name = histogram_name

    canvas = ROOT.TCanvas("canvas-" + hist_name, "canvas-" + hist_name, 200, 10, 700, 500)
    canvas.cd()

    hist = ROOT.TH2D(hist_name, hist_name, n_bins1, value_min1, value_max1, n_bins2, value_min2, value_max2)
    #hist.SetFillColor(color_style_list[x])
    #hist.SetFillStyle(fill_style_list[x])
    hist.SetStats(0)

    for i in range(0, len(value_list1) ):
        print "histo adding: ", str(value_list1[i]), " , ", str(value_list2[i])
        hist.Fill(value_list1[i], value_list2[i])

    #title the histogram (must draw it first)
    hist.Draw(draw_opt)
    hist.GetXaxis().SetTitle(value_name1)
    hist.GetXaxis().CenterTitle()
    hist.GetYaxis().SetTitle(value_name2)
    hist.GetYaxis().CenterTitle()
    hist.SetTitle(value_name1 + " vs. " + value_name2)
    hist.Draw(draw_opt)
    canvas.Update()

    return [hist, canvas]


def create_3d_histogram(value_list1, value_list2, value_list3, value_name1, value_name2, value_name3, histogram_name="", histo_config=None):
    #determine the min, max values, and the mean and std deviation
    value_max1 = max(value_list1)
    value_min1 = min(value_list1)
    value_mean1 = np.mean(value_list1)
    value_stddev1 = np.std(value_list1)

    value_max2 = max(value_list2)
    value_min2 = min(value_list2)
    value_mean2 = np.mean(value_list2)
    value_stddev2 = np.std(value_list2)

    value_max3 = max(value_list3)
    value_min3 = min(value_list3)
    value_mean3 = np.mean(value_list3)
    value_stddev3 = np.std(value_list3)
    
    n_bins1=100
    n_bins2=100
    n_bins3=100
    draw_opt = ""
    if histo_config != None:
        n_bins1 = histo_config.nxbins
        n_bins2 = histo_config.nybins
        n_bins3 = histo_config.nzbins
        draw_opt = histo_config.draw_option
        if histo_config.auto_minmax is False:
            value_min1 = histo_config.xmin
            value_max1 = histo_config.xmax
            value_min2 = histo_config.ymin
            value_max2 = histo_config.ymax
            value_min3 = histo_config.zmin
            value_max3 = histo_config.zmax
    
    hist_name = "hist-" + value_name1 + "-vs-" + value_name2 + "-vs-" + value_name3
    if histogram_name != "": 
        hist_name = histogram_name

    canvas = ROOT.TCanvas("canvas-" + hist_name, "canvas-" + hist_name, 200, 10, 700, 500)
    canvas.cd()


    hist = ROOT.TH3D(hist_name, hist_name, n_bins1, value_min1, value_max1, n_bins2, value_min2, value_max2, n_bins3, value_min3, value_max3)
    #hist.SetFillColor(color_style_list[x])
    #hist.SetFillStyle(fill_style_list[x])
    hist.SetStats(0)

    for i in range(0, len(value_list1) ):
        hist.Fill(value_list1[i], value_list2[i], value_list3[i])

    #title the histogram (must draw it first)
    hist.Draw(draw_opt)
    hist.GetXaxis().SetTitle(value_name1)
    hist.GetXaxis().CenterTitle()
    hist.GetYaxis().SetTitle(value_name2)
    hist.GetYaxis().CenterTitle()
    hist.GetZaxis().SetTitle(value_name3)
    hist.GetZaxis().CenterTitle()
    hist.SetTitle(value_name1 + " vs. " + value_name2 + " vs. " + value_name3)
    hist.Draw(draw_opt)
    canvas.Update()

    return [hist, canvas]
    
def plot_histograms(histogram_list, use_same=False):
    #plots each histogram in the list in a new canvas, returns a list of canvases
    #unless use_same==True, in which case all histograms will be plotted on the same canvas
    canvas_list = []
    for i in range(0, len(histogram_list)):
        hname = histogram_list[i].GetName()
        cname = "canvas-" + hname
        if use_same is True:
            cname = "canvas-collection" + hname
        canvas = ROOT.TCanvas(cname, cname, 200, 10, 700, 500)
        canvas_list.append(canvas)
        if use_same is True and i != 0:
            histogram_list[i].Draw('SAME')
        else:
            histogram_list[i].Draw()
        canvas.Update()
