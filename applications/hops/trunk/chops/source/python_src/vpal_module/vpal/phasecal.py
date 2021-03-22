'''
phasecal.py

Module providing some useful functions for the phase calibration software.

'''

import copy
import numpy as np
import numpy.linalg as la
from functools import reduce


def mult_corr(Rxx_full, bp_good=None, bad_nans=False):
    '''
    Compute multiple correlation coefficients for selected
    bandpols, whose indices are in the sequence bp_good.

      C.T is C transposed,
      Rxx is the matrix of cross-correlations between the band-pol series
             with the i-th bandpol excluded.

    Input parameters:
      Rxx_full[nbandpol,nbandpol] correlation matrix of the cable delay
                 time sequences in the rows of delps[nbandpol,:]
      bp_good: list of indices of good band-pols
      bad_nans:
          False: return in R_mult only the multiple correlations for the
                 bandpols listed in bp_good. len(R_mult) == len(bp_good).
          True:  return in R_mult the multiple correlations at the locations
                 listed in bp_good, and fill the rest with NaNs.
                 len(R_mult) == np.size(Rxx_full,0).

    Returns:
      R_mult, array of multiple correlation coefficients, one for each
              bandpol indicated in bp_good. The size of R_mult depends on
              the boolean value in bad_nans parameter.
    '''

    nbandpol = np.size(Rxx_full,0)

    if bp_good is None:
        Rxx_good = np.copy(Rxx_full)
        nbandpol_good = np.size(Rxx_full, 0)
    else:
        #
        # From matrix Rxx_full, extract a sub-matrix Rxx_good
        # having only columns and rows with the indices from list bp_good
        #
        cols = list(bp_good)          # Like [ 2,   3,   4,   5,   7 ]
        rows = [[i] for i in cols]    # Like [[2], [3], [4], [5], [7]]

        Rxx_good = Rxx_full[rows,cols]
        nbandpol_good = np.size(Rxx_good, 0)

    R_mult2 = np.zeros(nbandpol_good, dtype=float)  # Squared mult corrs
    R_mult_good = np.zeros_like(R_mult2)            # Mult corr coefficients

    for ibp in range(nbandpol_good):
        #
        # The ibp-th bandpol is assumed an independent variable.
        #
        # Rxx is the correlation matrix of all the the bandpols, or those
        # listed in bp_good, if it is given. It is obtained by scratching off
        # ibp-th row and ibp-th column from Rxx_good
        Rxx = np.delete(np.delete(Rxx_good, ibp, axis=0), ibp, axis=1)

        # cor is the vector of cross-correlations of each of the bandpols,
        # except the ibp-th one, with the ibp-th bandpol.
        cor = np.delete(Rxx_good[ibp,:], ibp, axis=0)

        invRxx = la.inv(Rxx)
        R_mult2[ibp] = reduce(np.dot, [cor, invRxx, cor])  # = C.T * Rxx^-1 * C

    R_mult_good = np.sqrt(R_mult2)

    if bad_nans:
        R_mult = np.zeros(nbandpol, dtype=float)
        R_mult[:] = np.NaN
        #ibp_good = 0
        for ibp in range(nbandpol_good):
            R_mult[bp_good[ibp]] = R_mult_good[ibp]

        return R_mult

    else:
        return R_mult_good



def write_xcorrmx(fout, title, Rxx_full, bp_good, station, \
                  experiment_number, experiment_code, bp_sym):
    '''
    Save the cross-correlation matrix in file fout.
    '''

    nbandpol = np.size(Rxx_full,0)
    nbp_sym = len(bp_sym)

    wrl2_1 = '#\n# ' + title + '\n#'
    # + ' Station ' + station + \
    #          ', Exp. ' + experiment_number + ', Code ' + experiment_code + \
    #         '\n#\n'
    wrl2_2 = 14*' '
    for ibp in range(nbp_sym):         # nbp_sym = 8 bandpols
        wrl2_2 += '    ' + bp_sym[ibp] + '  '

    fout.write(wrl2_1 + '\n')
    fout.write(wrl2_2 + '\n')

    for iy in range(nbandpol):
        wrl = 11*' ' + bp_sym[iy] + ' '
        for ix in range(nbandpol):
            if (iy in bp_good) and (ix in bp_good):
                wrl += ' {:6.3f} '.format(Rxx_full[iy,ix])
            else:
                wrl += 8*' '
        fout.write(wrl + '\n')
    fout.write('\n')


def write_numbers(fout, prefix, numbers, bp_good):
    '''
    Write a line of numbers at the positions at bp_good, and blanks at others.
    The numbers can be multiple correlations or correlation medians and
    they are supposed to be printed just at the locations of the
    cross-correlation matrix columns.
    '''
    nbandpol = len(numbers)
    wrl = copy.copy(prefix)   # 13*' '
    for ibp in range(nbandpol):
        if ibp in bp_good:
            wrl += ' {:6.3f} '.format(numbers[ibp])
        else:
            wrl += 8*' '
    fout.write(wrl + '\n')




def write_title(fout, title, station, experiment_number, experiment_code, \
                  bp_sym, threshold_median):
    '''
    Write title in file fout.
    '''

    nbp_sym = len(bp_sym)
    wrl1 = '#\n# ' + title + '\n#\n# Station ' + station + \
             ', Experiment ' + experiment_number + \
             ', Code ' + experiment_code + '\n'
    wrl2 = '#\n# Median threshold: ' + str(threshold_median) + '\n'

    fout.write(wrl1)
    fout.write(wrl2)

    # wrline1 = '# ' + experiment_code + ' ' + experiment_number + ' '
    # for ibp in range(nbp_sym):         # nbp_sym = 8 bandpols
    #     wrline1 += '    ' + bp_sym[ibp] + '  '

    # fout.write(wrline1 + '\n')
