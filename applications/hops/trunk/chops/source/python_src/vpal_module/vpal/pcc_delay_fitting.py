"""library to handle the calculation of proxy-cable-calibration delays from phase-cal data """

#core imports
from __future__ import print_function
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

import logging
pcc_fit_logger = logging.getLogger(__name__)

from .utility import limit_periodic_quantity_to_range
from .utility import minimum_angular_difference

#non-core imports
import numpy as np
import scipy.optimize
import scipy.stats

PICOSECOND = 1e-12
EPS = 1e-15

################################################################################

class PhasorFitData(object):
    """container class to work-around scipy.optimize requirements on function args"""
    def __init__(self, ref_freq, freq_phasor_pair_list):
        self.reference_frequency = ref_freq
        self.tone_phasors = freq_phasor_pair_list

    def get_mean_phase(self):
        phase_list = []
        for fq_ph in self.tone_phasors:
            angle = cmath.phase(fq_ph[1])
            phase_list.append(angle)
        return scipy.stats.circmean( np.array(phase_list), low=-1.0*math.pi, high=math.pi)

    def get_phase_std(self):
        phase_list = []
        for fq_ph in self.tone_phasors:
            angle = cmath.phase(fq_ph[1])
            phase_list.append(angle)
        return scipy.stats.circstd( np.array(phase_list), high=math.pi, low=-1.0*math.pi)

################################################################################



class DelayFitResults(object):
    """container class to store the delay-fit results for a particular pol-band"""
    def __init__(self, fit_data):
        self.fit_data = fit_data #data to be fit
        self.valid = False #fit success/failure indicator
        self.delay = 0 #delay fit to data (sec)
        self.phase_offset = 0 #phase at the reference frequency (radians)
        self.model_dc_phase = 0 #phase extrapolated to DC (radians)
        self.phase_rmse = 0 #root-mean-squared error of the fit(radians)
        self.residuals = [] #phase residuals

###############################################################################


class BasinhoppingStep(object):
    """ defines the stepsizes to be taken when using the basin hopping algorithm to find the delay/phase"""
    def __init__(self, stepsize_delay=PICOSECOND, stepsize_phase=3.0*(math.pi/180.0) ):
        self.delay_step = stepsize_delay
        self.phase_step = stepsize_phase
    def __call__(self, x):
        x[0] += np.random.uniform(-self.delay_step, self.delay_step)
        x[1] += np.random.uniform(-self.phase_step, self.phase_step)
        return x


################################################################################


def calc_delay_phasor(delay, phase_offset, reference_frequency, frequency):
    """compute complex number associated with a particular delay/phase_offset,frequency and reference_frequency """
    val = cmath.exp(1j*( 2.0*math.pi*delay*(frequency - reference_frequency) + phase_offset) )
    return val

################################################################################


class BandDelayFitter(object):

    def __init__(self):
        self.basinhopping_niter = 10
        self.channel_bandwidth = 32e6


    def fit_band_delay(self, tone_phasors, ref_frequency, cut_threshold=0.0, verbosity=0):
        #now lets fit the model
        fit_data = PhasorFitData(ref_frequency, tone_phasors)
        result = DelayFitResults(fit_data)

        #if not more than 1 data point then do not try to fit this one
        if len(tone_phasors) < 2:
            result.valid = False
            return result
        else:
            delay = self.get_initial_band_delay(fit_data.tone_phasors, fit_data.reference_frequency)
            phase_offset = fit_data.get_mean_phase() #this works under the assumption the ref_freq is in the center of the band (true for now), but we may need to make this more robust
            param = [delay, phase_offset]
            stepper = BasinhoppingStep()

            #re-seed the (singleton) random number generator which scipy.optimize calls
            #this affects the results on the ~1ps level (less than our error), and is mainly done for consistency/repeatability
            np.random.seed(1)

            #initial fit is done using basin-hopping (stochastic fitter)
            ret = scipy.optimize.basinhopping(self.fit_function1, param, niter=self.basinhopping_niter, minimizer_kwargs={"method" : "Nelder-Mead", "args" : (fit_data,)}, take_step=stepper)
            #now try to strip outliers, which have residuals which are larger than some factor*sigma, score
            ph_resid = self.get_phase_residuals(fit_data, param[0], param[1])
            ph_resid_angles = [x[1] for x in ph_resid]
            n_cut = 0;
            for n in list(range(0,len(ph_resid_angles))):
                if n < len(fit_data.tone_phasors):
                    if (abs(ph_resid_angles[n]) < cut_threshold*math.sqrt(fit_data.tone_phasors[n][2].get_phase_variance()) ) or (cut_threshold == 0.0):
                        fit_data.tone_phasors[n][3] = True
                    else:
                        fit_data.tone_phasors[n][3] = False #flip use-flag to false
                        n_cut += 1

            ret = scipy.optimize.basinhopping(self.fit_function1, param, niter=self.basinhopping_niter, minimizer_kwargs={"method" : "Nelder-Mead", "args" : (fit_data,)}, take_step=stepper)
            #then, a refined estimate is done locally
            param = ret.x
            display_fit_info = False
            if verbosity >= 3:
                display_fit_info = True
            #use Nelder-Mead instead of CG as it is more stable
            # init_simplex = []
            # init_simplex.append([param[0], param[1]])
            # init_simplex.append([param[0] + stepper.delay_step*0.1, param[1] ])
            # init_simplex.append([param[0], param[1] + stepper.phase_step*0.1 ])
            # ret = scipy.optimize.minimize(self.fit_function1, param, args=(fit_data,), method="Nelder-Mead", options = {'initial_simplex':init_simplex, 'disp':display_fit_info} )
            ret = scipy.optimize.minimize(self.fit_function1, param, args=(fit_data,), method="Nelder-Mead", options = {'disp':display_fit_info} )
            #ret = scipy.optimize.minimize(self.fit_function3, param, args=(fit_data,), method="CG", options = {'disp':display_fit_info} )
            result.valid = ret.success
            result.delay = ret.x[0]
            result.phase_offset = ret.x[1]
            result.model_dc_phase = cmath.phase( cmath.exp(1j*result.phase_offset)*cmath.exp(-1j*(2.0*math.pi*(result.delay)*ref_frequency) )  )
            #compute the residuals, and root-mean-squared error
            result.residuals = self.get_phase_residuals(fit_data, result.delay, result.phase_offset)
            rmse = 0.0
            n_resid = len(result.residuals)
            if n_resid != 0:
                for frq_resid in result.residuals:
                    ra = frq_resid[1]
                    rmse += ra*ra
                result.phase_rmse = math.sqrt(rmse/n_resid)

            #if the data is so bad that there are only 3 un-cut phasors, then flag this result as invalid
            if len(fit_data.tone_phasors) - n_cut <= 3:
                result.valid = False

        return result

    def get_phase_residuals(self, fit_data, delay, phase_offset):
        ref_freq = fit_data.reference_frequency
        residuals = []
        for a_pair in fit_data.tone_phasors:
            freq = a_pair[0]
            phasor = a_pair[1]
            model_angle = cmath.phase( calc_delay_phasor(delay, phase_offset, ref_freq, freq) )
            phasor_angle = cmath.phase(phasor)
            resid_angle = minimum_angular_difference(model_angle, phasor_angle, low_value=-1*math.pi, high_value=math.pi)
            residuals.append([freq, resid_angle])
        return residuals

    def fit_function1(self, par, fit_data):
        """a fit function to be optimized when fitting for the band delay """
        # par[0] is the delay, par[1] is phase_offset
        # objective sum is maximized when all the phasors
        # are aligned by the delay model along the same direction(phase)
        reference_frequency = fit_data.reference_frequency

        delay = par[0]
        phase_offset = par[1]
        #unweighted sum, all non-zero phasors contribute equally
        cobjsum = complex(0,0)
        for a_pair in fit_data.tone_phasors:
            freq = a_pair[0]
            phasor = a_pair[1]
            if abs(phasor) > EPS and a_pair[3] is True:
                conj_model = calc_delay_phasor(delay, phase_offset, reference_frequency, freq).conjugate()
                cobjsum += conj_model*(phasor/abs(phasor))
        return -1.0*cobjsum.real

    def fit_function2(self, par, fit_data):
        """a fit function to be optimized when fitting for the band delay """
        # par[0] is the delay, par[1] is phase_offset
        # objective sum is maximized when all the phasors
        # are aligned by the delay model along the same direction(phase)
        reference_frequency = fit_data.reference_frequency

        delay = par[0]
        phase_offset = par[1]
        #weighted sum, all phasors are weighted by their amplitude
        cobjsum = complex(0,0)
        abssum = 0
        for a_pair in fit_data.tone_phasors:
            freq = a_pair[0]
            phasor = a_pair[1]
            if abs(phasor) > EPS and a_pair[3] is True:
                abssum += abs(phasor)
                conj_model = calc_delay_phasor(delay, phase_offset, reference_frequency, freq).conjugate()
                cobjsum += conj_model*phasor
        return ( (cobjsum.imag)*(cobjsum.imag)/(absum*absum) )

    def fit_function3(self, par, fit_data):
        """a fit function to be optimized when fitting for the band delay """
        # par[0] is the delay, par[1] is phase_offset
        # objective sum is maximized when all the phasors
        # are aligned by the delay model along the same direction(phase)
        reference_frequency = fit_data.reference_frequency
        delay = par[0]
        phase_offset = par[1]
        #unweighted sum, all non-zero phasors contribute equally
        chi2sum = 0.0
        for a_pair in fit_data.tone_phasors:
            if a_pair[3] is True:
                freq = a_pair[0]
                phasor = a_pair[1]
                ph_var = a_pair[2].get_phase_variance()
                model = calc_delay_phasor(delay, phase_offset, reference_frequency, freq)
                delta = minimum_angular_difference( cmath.phase(phasor), cmath.phase(model), -1.0*math.pi, math.pi)
                chi2sum += delta*delta/ph_var
        return chi2sum


    def get_initial_band_delay(self, phasor_freq_pairs, reference_frequency):
        """estimate intial values for the band delay and phase"""

        #this is pretty slow ~O(N^2) and simplistic
        delay_estimates = []
        for x in phasor_freq_pairs:
            for y in phasor_freq_pairs:
                delta_freq = y[0] - x[0]
                if delta_freq > self.channel_bandwidth:
                    angle1 = cmath.phase(y[1])
                    angle2 = cmath.phase(x[1])
                    if abs(angle2 - angle1) < math.pi:
                        delta_phase = minimum_angular_difference(angle1, angle2, low_value=-1.0*math.pi, high_value=math.pi)
                        delay_estimates.append(delta_phase/(2.0*math.pi*delta_freq))

        #get median delay
        if len(delay_estimates) == 0:
            return 0.0
        else:
            delay = np.median(delay_estimates)
            return delay
