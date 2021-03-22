"""library generate some plots from proxy phase-cal data """

#core imports
from __future__ import absolute_import
from __future__ import division
from builtins import next
from builtins import str
from past.utils import old_div
from builtins import object
from builtins import range
import datetime
import sys
import os
import math
import cmath
import copy

#non-core imports
import numpy as np
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import matplotlib.colors as mcolors
import matplotlib.colorbar as mcbar
import matplotlib.cm as cmx
from mpl_toolkits.mplot3d import Axes3D

#hops package python libs
from . import proxy_cable_cal
from . import pcc_delay_fitting


class ProxyCableDelayFitPlotter(object):

    def plot_individual_band_delays(self, st_exp_phasor_collection, pcc_config):

        #create output dir if not already there
        if not os.path.exists(pcc_config.output_dir):
            os.makedirs(pcc_config.output_dir)

        #get our station code
        station_code = st_exp_phasor_collection.station_code
        station_id = st_exp_phasor_collection.mk4_site_id

        #we want to refererence all scan times w.r.t midnight 00:00:00
        #of the day of the first scan/start of the session
        exp_start_time = proxy_cable_cal.PccDate()
        exp_start_time.year = st_exp_phasor_collection.reference_scan_object.start_time.year
        exp_start_time.day = st_exp_phasor_collection.reference_scan_object.start_time.day
        exp_start_time.hour = 0
        exp_start_time.minute = 0
        exp_start_time.second = 0

        for bp in st_exp_phasor_collection.active_band_pol_list:
            band, pol = bp.split(':')
            auto_fig_name = "bandmodel." + station_id + "." + band + "." + pol + ".png"

            reltime = []
            delay = []
            dc_phase = []
            rmse_phase = []

            for scan in st_exp_phasor_collection.sspc_list:
                year = scan.start_time.year
                doy = scan.start_time.day
                hour = scan.start_time.hour
                minute = scan.start_time.minute
                second = scan.start_time.second
                source_name = scan.source_name
                scan_name = scan.scan_name
                azi = scan.azimuth
                ele = scan.elevation
                if bp in scan.fit_results.keys():
                    scan_relative_time = scan.start_time.get_time_delta_seconds(exp_start_time)
                    scan_relative_time /= 3600 #convert to hours
                    band_delay = scan.fit_results[bp].delay/pcc_delay_fitting.PICOSECOND
                    phase_model_midband = (scan.fit_results[bp].phase_offset)*(180.0/math.pi)
                    phase_model_dc = scan.fit_results[bp].model_dc_phase
                    phase_rmse = scan.fit_results[bp].phase_rmse
                    #include data in plot
                    if scan.fit_results[bp].valid is True:
                        reltime.append(scan_relative_time)
                        delay.append(band_delay)
                        dc_phase.append(math.degrees(phase_model_dc))
                        rmse_phase.append(math.degrees(phase_rmse))

            #now create a plot for this band/pol
            auto_fig = plt.figure(figsize=(8.5,11))
            plt.subplot(311)
            #dc edge phase
            plt.plot(reltime, dc_phase, 'kx', markersize=4)
            plt.grid(True)
            plt.ylabel("phase at DC (deg)")
            plt.ylim(-180,180)
            ax = plt.gca()
            ax.set_yticks([-180, -90, 0, 90, 180])
            plt.subplot(312)
            #delay
            plt.plot(reltime, delay, 'k-x', markersize=4)
            plt.grid(True)
            plt.ylabel("delay (ps)")
            plt.subplot(313)
            #phase rms
            plt.plot(reltime, rmse_phase, 'k-x', markersize=4)
            plt.ylabel("fit rmse (deg)")
            # plt.xlabel("hours since: " + exp_data.get_refererence_scan().start_time.as_string() )
            plt.xlabel("hours since: " + exp_start_time.as_string() )
            plt.grid(True)
            auto_fig_name = "bandmodel." + station_id + "." + band + "." + pol + ".png"
            auto_fig.savefig(os.path.join(pcc_config.output_dir, auto_fig_name) )
            plt.close(auto_fig)


    def plot_mean_band_delay(self, st_exp_phasor_collection, pcc_config):
        #determine the pols, and bands which we are allowed to average over
        chan_map = proxy_cable_cal.ChannelToBandMap(pcc_config.mode)
        allowed_pols = chan_map.intersecting_pol_subset(pcc_config.pol_list)
        all_bands = chan_map.intersecting_band_subset(pcc_config.band_list)
        allowed_bands = list(all_bands)

        #default is to not include band 'A' in mean delay calculation in VGOS or MIXED mode
        if pcc_config.mode == "VGOS" or pcc_config.mode == "MIXED" or pcc_config.mode == "BBMIXED":
            if 'A' in allowed_bands:
                allowed_bands.remove('A')

        #default is to not include band 'A' or 'B' in mean delay calculation in BBMIXED mode
        if pcc_config.mode == "BBMIXED":
            if 'B' in allowed_bands:
                allowed_bands.remove('B')

        #numerical index for all bands and allowed pols for plot-grid arrangement
        pdict = dict()
        bdict = dict()
        for index in list(range(0, len(allowed_pols) ) ):
            pdict[allowed_pols[index]] = index

        for index in list(range(0, len(all_bands) ) ):
            bdict[all_bands[index]] = index
        npols = len(pdict)
        nbands = len(bdict)

        #create output dir if not already there
        if not os.path.exists(pcc_config.output_dir):
            os.makedirs(pcc_config.output_dir)

        #get our station code
        station_code = st_exp_phasor_collection.station_code
        station_id = st_exp_phasor_collection.mk4_site_id

        #we want to refererence all scan times w.r.t midnight 00:00:00
        #of the day of the first scan/start of the session
        exp_start_time = proxy_cable_cal.PccDate()
        exp_start_time.year = st_exp_phasor_collection.reference_scan_object.start_time.year
        exp_start_time.day = st_exp_phasor_collection.reference_scan_object.start_time.day
        exp_start_time.hour = 0
        exp_start_time.minute = 0
        exp_start_time.second = 0

        #now lets compute the mean band delays for each pol (for the allowed bands)
        mean_pol_reltime_data = dict()
        mean_pol_delay_data = dict()
        pol_band_set_used = dict()

        for p in allowed_pols:
            #for every pol, construct a list of allowed band-pol combos
            allowed_bp = []
            band_set = set()
            for b in allowed_bands:
                bp = b + ":" + p
                allowed_bp.append(bp)
            #the relative time, and mean band delay for this pol which we
            #want to plot
            reltime_list = []
            mean_delay_list = []
            for scan in st_exp_phasor_collection.sspc_list:
                year = scan.start_time.year
                doy = scan.start_time.day
                hour = scan.start_time.hour
                minute = scan.start_time.minute
                second = scan.start_time.second
                source_name = scan.source_name
                scan_name = scan.scan_name
                valid = False
                count = 0
                mean_delay = 0.0
                reltime = 0.0
                for bp in allowed_bp:
                    if bp in scan.fit_results.keys():
                        if scan.fit_results[bp].valid is True:
                            band_set.add(bp.split(':')[0])
                            valid = True #have at least one data point for this scan
                            mean_delay += scan.fit_results[bp].delay/pcc_delay_fitting.PICOSECOND
                            if count == 0: #set the reltime once...they should all be the same for one scan
                                scan_relative_time = scan.start_time.get_time_delta_seconds(exp_start_time)
                                scan_relative_time /= 3600 #convert to hours
                                reltime = scan_relative_time
                            count += 1
                if valid is True:
                    reltime_list.append(reltime)
                    mean_delay_list.append(mean_delay/count)

            mean_pol_reltime_data[p] = reltime_list
            mean_pol_delay_data[p] = mean_delay_list
            pol_band_set_used[p] = band_set

        #now we construct the plots for each band-pol, so grab the data for each
        #need to stash the relative-time and delay for each band pol
        scan_name_data = dict()
        reltime_data = dict()
        delay_data = dict()
        fit_valid_data = dict()
        for bp in st_exp_phasor_collection.active_band_pol_list:
            band, pol = bp.split(':')
            reltime_list = []
            delay_list = []
            scan_name_list = []
            fit_valid_list = []
            for scan in st_exp_phasor_collection.sspc_list:
                year = scan.start_time.year
                doy = scan.start_time.day
                hour = scan.start_time.hour
                minute = scan.start_time.minute
                second = scan.start_time.second
                source_name = scan.source_name
                scan_name = scan.scan_name
                azi = scan.azimuth
                ele = scan.elevation
                if bp in scan.fit_results.keys():
                    scan_relative_time = scan.start_time.get_time_delta_seconds(exp_start_time)
                    scan_relative_time /= 3600 #convert to hours
                    band_delay = scan.fit_results[bp].delay/pcc_delay_fitting.PICOSECOND
                    fit_valid_list.append( scan.fit_results[bp].valid)
                    reltime_list.append(scan_relative_time)
                    delay_list.append(band_delay)
                    scan_name_list.append(scan_name)
            scan_name_data[bp] = scan_name_list
            reltime_data[bp] = reltime_list
            delay_data[bp] = delay_list
            fit_valid_data[bp] = fit_valid_list

        #now lets make plots
        #now create a plot for this band/pol
        auto_fig = plt.figure(figsize=(8.5,11))
        plot_title = 'Experiment: ' + str(st_exp_phasor_collection.exp_num) + ', ' + str(st_exp_phasor_collection.exp_name) + ' Station ' + station_id + ' \n Band delay comparison for pols: ' + str(allowed_pols)
        auto_fig.suptitle(plot_title)

        #plot the by-pol mean delays here:
        for p in allowed_pols:
            pol_num = pdict[p] + 1
            pol_mean_delay = mean_pol_delay_data[p]
            pol_reltimes = mean_pol_reltime_data[p]
            plt.subplot(nbands+1, npols, pol_num)
            plt.plot(pol_reltimes, pol_mean_delay, 'kx', markersize=3)
            band_string = ''.join( sorted( list(pol_band_set_used[p]) ) )
            plt.ylabel(p + " mean (" + band_string  + ") delay (ps)")
            plt.grid(True)

        #plot all individual band pols here:
        for bp in st_exp_phasor_collection.active_band_pol_list:
            band, pol = bp.split(':')
            if band in bdict and pol in pdict:
                band_num = bdict[band]
                pol_num = pdict[pol] + 1
                plot_num = npols*(band_num+1) + pol_num
                plt.subplot(nbands+1, npols, plot_num)
                plt.grid(True)
                # index = get_band_index_standalone(pol,band)
                # band_delay_diff = exp_data.mean_delay_results.band_diff[index]  #REMOVED IN FAVOR OF PLOTTING THE ACTUAL DELAY
                reltime = reltime_data[bp]
                delay = delay_data[bp]
                plt.plot(reltime, delay, 'kx', markersize=3)
                plt.ylabel(band + ":"  + pol + " delay (ps)")
                plt.grid(True)
                if plot_num/(nbands) > npols: #last row of plots gets the x-axis time label
                    plt.xlabel("hours since: " + exp_start_time.as_string() )

        plt.tight_layout()
        auto_fig.subplots_adjust(top=0.94)
        auto_fig_name = "meanbanddelay." + station_id + ".png"
        auto_fig.savefig(os.path.join(pcc_config.output_dir, auto_fig_name))
        plt.close(auto_fig)

################################################################################

    def plot_bandfits(self, st_exp_phasor_collection, pcc_config, n_points=500, use_degrees=True):
        """ Plot the fit, residuals, and the neighborhood of the local minimum found for the fit solution """
        #we want to plot:
        #(1) the phasor data
        #(2) the fitted delay model solution + mid-band phase/location
        #(3) the phase residuals
        #(4) the fit function as a function of fit parameters: delay/phase

        #create output dir if not already there
        if not os.path.exists(pcc_config.output_dir):
            os.makedirs(pcc_config.output_dir)

        scale_freq = 1e9 #use GHz for freq axis
        factor = 1.0
        if use_degrees is True:
            factor = 180.0/math.pi


        for scan in st_exp_phasor_collection.sspc_list:
            for bp in scan.fit_results.keys():
                if scan.fit_results[bp].valid is True:
                    ref_freq = scan.fit_results[bp].fit_data.reference_frequency
                    tone_phasors = scan.fit_results[bp].fit_data.tone_phasors
                    delay = scan.fit_results[bp].delay
                    phase_offset = scan.fit_results[bp].phase_offset
                    residuals = scan.fit_results[bp].residuals

                #get the phasor data into lists
                phase_arr = []
                phase_err_arr = []
                freq_arr = []

                #this is here to make a plot of the phasor measurements during each scan

                count = 0
                x_arr = []
                y_arr = []
                z_arr = []
                auto_fig = plt.figure(figsize=(8.5,11))

                for x in tone_phasors:
                    freq_arr.append( x[0] )
                    phase_arr.append( cmath.phase(x[1])*factor )
                    ph_stddev = math.sqrt( x[2].get_phase_variance() )
                    phase_err_arr.append( factor*ph_stddev )
                    phasor_measurements = x[2].data
                    x_arr.extend([c.real for c in phasor_measurements])
                    y_arr.extend([c.imag for c in phasor_measurements])
                    z_arr.extend( [x[0]/scale_freq]*len(phasor_measurements) )

                maps = plt.colormaps()
                if ('viridis' not in maps):
                    cmap = cmx.get_cmap(maps[0])
                else:
                    cmap = cmx.get_cmap('viridis')

                normalize = mcolors.Normalize(vmin=min(z_arr), vmax=max(z_arr))
                colors = [cmap(normalize(value)) for value in z_arr]

                plt.scatter(x_arr, y_arr, s=1, c=colors)
                plt.title("P-cal phasor measurements during scan: " + scan.scan_name + ", station: " + st_exp_phasor_collection.mk4_site_id + " band-pol:" + bp.replace(':','-'))
                plt.grid(True)
                plt.ylabel("Imag)")
                plt.xlabel("Real")
                ax = plt.gca()
                cax, _ = mcbar.make_axes(ax, location='bottom', anchor=(0.5, -0.2), aspect=30 )
                cbar = mcbar.ColorbarBase(cax, cmap=cmap, norm=normalize, orientation='horizontal')
                cbar.ax.set_xlabel("Frequency (GHz)")
                auto_fig_name = "tone-phasors-" + scan.scan_name + "." +  st_exp_phasor_collection.mk4_site_id + "." + bp.replace(':','-') + ".png"
                auto_fig.savefig(os.path.join(pcc_config.output_dir, auto_fig_name) )
                plt.close(auto_fig)

                min_freq = min(freq_arr)
                max_freq = max(freq_arr)
                step =(max_freq - min_freq)/n_points

                #get the residuals
                resid_arr = []
                resid_freq_arr = []
                for x in residuals:
                    resid_freq_arr.append( x[0] )
                    resid_arr.append( x[1]*factor )

                #create plotting arrays of the delay models
                model_phase = []
                model_freq = []
                for n in list(range(0,n_points)):
                    freq = min_freq + n*step
                    phase = cmath.phase( pcc_delay_fitting.calc_delay_phasor(delay, phase_offset, ref_freq, freq) )*factor
                    model_freq.append(freq)
                    model_phase.append(phase)

                #now create a plot for this band/pol
                auto_fig = plt.figure(figsize=(8.5,11))
                plt.subplot(311)
                plt.title("Band delay-fit for scan: " + scan.scan_name + ", station: " + st_exp_phasor_collection.mk4_site_id + " band-pol:" + bp.replace(':','-'))
                plt.errorbar(freq_arr, phase_arr, yerr=phase_err_arr, fmt='bo', markersize=4)
                plt.plot(model_freq, model_phase, 'r-')
                plt.grid(True)
                plt.ylabel("Phase (deg)")
                ax = plt.gca()
                ticks_x = ticker.FuncFormatter(lambda a, pos: '{0:g}'.format(old_div(a,scale_freq)))
                ax.xaxis.set_major_formatter(ticks_x)
                plt.xlabel("Frequency (GHz)")

                #residuals
                plt.subplot(312)
                plt.errorbar(resid_freq_arr, resid_arr, yerr=phase_err_arr, fmt='bo', markersize=4)
                plt.grid(True)
                plt.ylabel("Phase residuals (deg)")
                ax2 = plt.gca()
                ticks_x2 = ticker.FuncFormatter(lambda a, pos: '{0:g}'.format(old_div(a,scale_freq)))
                ax2.xaxis.set_major_formatter(ticks_x2)
                plt.xlabel("Frequency (GHz)")
                plt.grid(True)

                #now we want to create a plot of the minimization function and put
                #a star in the location of the minimum

                bd_fitter = pcc_delay_fitting.BandDelayFitter()
                n_points_de = 500
                n_points_ph = 50
                delay_window = 4e-9
                x = np.zeros((n_points_de, n_points_ph))
                y = np.zeros((n_points_de, n_points_ph))
                z = np.zeros((n_points_de, n_points_ph))
                for i in list(range(0,n_points_de)):
                    for j in list(range(0,n_points_ph)):
                        x[i][j] = (delay - delay_window) + i*(2.0*delay_window)/n_points_de
                        y[i][j] = (-math.pi + j*2.0*math.pi/n_points_ph )*(old_div(180.0,math.pi))
                        param = [ x[i][j], y[i][j]*(old_div(math.pi,180.0)) ]
                        z[i][j] = bd_fitter.fit_function1(param,  scan.fit_results[bp].fit_data)

                plt.subplot(313)
                plt.contourf(x, y, z, 20 )
                plt.ylim(-180,180)
                ax3 = plt.gca()
                ax3.set_yticks([-180, -90, 0, 90, 180])
                plt.ylabel("Phase offset at \n reference frequency (deg)")
                cbar = plt.colorbar(pad=0.2, orientation='horizontal')
                cbar.set_label('Minimization function')
                plt.plot(delay, phase_offset*(old_div(180.0,math.pi)), 'r+', markersize=7)
                ax3.annotate('   ($t$, $\phi$) = (' + str(round(old_div(delay,pcc_delay_fitting.PICOSECOND),2) ) + ' ps, ' + str(round(phase_offset*(old_div(180.0,math.pi)),0)) + '$^\circ$)', xy=(delay, phase_offset*(old_div(180.0,math.pi))), color='red')
                scale_delay = 1e-9
                ticks_x3 = ticker.FuncFormatter(lambda a, pos: '{0:g}'.format(old_div(a,scale_delay)))
                ax3.xaxis.set_major_formatter(ticks_x3)
                plt.xlabel("Delay (ns)")

                auto_fig_name = "bandfit-" + scan.scan_name + "." +  st_exp_phasor_collection.mk4_site_id + "." + bp.replace(':','-') + ".png"
                auto_fig.savefig(os.path.join(pcc_config.output_dir, auto_fig_name) )
                plt.close(auto_fig)



    def plot_phasor_amplitude_surface(self, st_exp_phasor_collection, pcc_config, use_degrees=True):
        """plot the amplitude of each p-cal phasor as a surface function of time,frequency """
        #create output dir if not already there
        if not os.path.exists(pcc_config.output_dir):
            os.makedirs(pcc_config.output_dir)

        scale_freq = 1e9 #use GHz for freq axis
        factor = 1.0
        if use_degrees is True:
            factor = 180.0/math.pi

        #we want to refererence all scan times w.r.t midnight 00:00:00
        #of the day of the first scan/start of the session
        exp_start_time = proxy_cable_cal.PccDate()
        exp_start_time.year = st_exp_phasor_collection.reference_scan_object.start_time.year
        exp_start_time.day = st_exp_phasor_collection.reference_scan_object.start_time.day
        exp_start_time.hour = 0
        exp_start_time.minute = 0
        exp_start_time.second = 0

        for bp in st_exp_phasor_collection.active_band_pol_list:
            band, pol = bp.split(':')

            reltime = []
            freq_arr = []
            phasor_amp_arr = []

            for scan in st_exp_phasor_collection.sspc_list:
                year = scan.start_time.year
                doy = scan.start_time.day
                hour = scan.start_time.hour
                minute = scan.start_time.minute
                second = scan.start_time.second
                source_name = scan.source_name
                scan_name = scan.scan_name
                azi = scan.azimuth
                ele = scan.elevation
                if bp in scan.fit_results.keys():
                    scan_relative_time = scan.start_time.get_time_delta_seconds(exp_start_time)
                    scan_relative_time /= 3600 #convert to hours
                    #get the fit data
                    #if scan.fit_results[bp].valid is True:
                    reltime.append(scan_relative_time)
                    fit_data = scan.fit_results[bp].fit_data
                    #freq_arr = list(range(0, len(fit_data.tone_phasors)))
                    freq_arr = [tph[0]/scale_freq for tph in fit_data.tone_phasors ]
                    for tone_phasor in fit_data.tone_phasors:
                        phasor_amp_arr.append(abs(tone_phasor[1]))

            n_points_t = len(reltime)
            n_points_f = len(freq_arr)

            if n_points_f !=0 and n_points_t != 0:
                x = np.zeros((n_points_t, n_points_f))
                y = np.zeros((n_points_t, n_points_f))
                z = np.zeros((n_points_t, n_points_f))
                for i in list(range(0,n_points_t)):
                    for j in list(range(0,n_points_f)):
                        x[i][j] = reltime[i]
                        y[i][j] = freq_arr[j]
                        z[i][j] = phasor_amp_arr[i*n_points_f + j]

                #this is here to make a plot of the phasor measurements during each scan
                auto_fig = plt.figure(figsize=(8.5,11))
                plt.contourf(x,y,z,20)
                plt.title("P-cal phasor amplitude as function of time/frequency for station:" + st_exp_phasor_collection.mk4_site_id + " band-pol:" + bp.replace(':','-'))
                plt.ylabel("P-cal tone frequency (GHz)")
                plt.xlabel("Hours since: " + exp_start_time.as_string() )
                cbar = plt.colorbar(pad=0.2, orientation='horizontal')
                cbar.set_label('P-cal phasor amplitude')
                auto_fig_name = "phasor_amp" + "." +  st_exp_phasor_collection.mk4_site_id + "." + bp.replace(':','-') + ".png"
                auto_fig.savefig(os.path.join(pcc_config.output_dir, auto_fig_name) )
                plt.close(auto_fig)
