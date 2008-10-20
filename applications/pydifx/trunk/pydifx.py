"""
This file contains functions for setting parameters in the difx configuration
file and running the correllator

TODO make it more efficient by storing the entire vex file? (to make this work we would need to make too many changes to the individual functions
"""
from math import floor

import observation
import difxlog as log
import vex2calc.vex2calcheader as vex2calcheader
import vex2calc.vex2calcantenna as vex2calcantenna
import vex2calc.vex2sched as vex2sched
import vex2calc.eop as eop
import vex2calc.calcfooter as vex2calcfooter
import vex2flag
import log2input
import machinegen
import calcif
import mpifxcorr
import killdifx
import difx2fits
from vex2calc.readvex import VexSched
from pfile import set_parameter, get_parameter, del_parameter

class DifxJob:
    """
    Run the correllator. All parameters are taken from observation.py.

    Assumptions:
        - all files in their default places
        - root = obscode
    """

    def  __init__(self, root):
        self.set_defaults(root)

    def set_defaults(self, root):
        """
        Set all files to root.extension.

        e.g.
        Correlator.input = root.input
        
        n.b.
        the path of the .threads file is read from
        the input file

        root can also be a path.
        """
        self.vex = root + '.skd'
        self.input = root + '.input' #input is a function similar to eval
        self.machine = root + '.machine'
        self.cluster = root + '.cluster'
        self.calc = root + '.calc'
        self.tsys = 'tsys'
        self.obscode = root
        self.fits_file = root.upper() + '.FITS'


    def get_calc(self, parameter, occurrence = 0):
        get_parameter(parameter, self.calc, occurrence)

    def set_calc(self, parameter, value, occurrence = 0, just = 20):
        set_parameter(parameter, value, self.calc, occurrence, just)

    def del_calc(self, parameter, occurrence = 0):
        del_parameter(parameter, self.calc, occurrence)

    def get_input(self, parameter, occurrence = 0):
        get_parameter(parameter, self.input, occurrence)

    def set_input(self, parameter, value, occurrence = 0, just = 20):
        set_parameter(parameter, value, self.input, occurrence, just)

    def del_input(self, parameter, occurrence = 0):
        del_parameter(parameter, self.input, occurrence)

    def vex2input(self):
        pass

    def vex2calc(self, job_id = None, increment = None, spectral_average = None, taper_function = None,
                 antennas = None,
                 offset = None, tail = None,
                 extra = None, download = None, force = None,):

        self.job_id = job_id
        self.increment = increment
        self.spectral_average = spectral_average
        self.taper_function = taper_function
        self.antennas = antennas
        self.offset = offset
        self.tail = tail
        self.extra = extra
        self.download = download
        self.force = force

        vex2calcheader.write_header(self.vex, self.calc, self.obscode, job_id, increment, spectral_average, taper_function)
        vex2calcantenna.write_antenna(self.vex, self.calc, antennas)
        vex2sched.write_scan(self.vex, self.calc, offset, tail, increment)
        eop.write_eops(floor(VexSched(self.vex).startmjd() + 0.5), self.calc, extra, download, force)
        calcfooter.write_footer(self.obscode, self.calc)

    def vex2flag(self, flag_file = None, shrink = None, printuv = None):
        
        self.flag_file = flag_file
        self.shrink = shrink
        self.printuv = printuv

        vex2flag.write_flag(self.vex, flag_file, shrink, printuv)

    def log2input(self, antenna_log_index = None):

        if antenna_log_index == None:
            try:
                antenna_log_index = observation.antenna_log_index
            except AttributeError:
                log.exception('no antenna_log_index defined in observation.py')
                raise
        self.logs = [self.obscode + i + '.log' for i in antenna_log_index]

        self.starttime = VexSched.start()
                
        log2input.add_clock(self.logs, self.starttime, self.input)

            
    def log2tsys(self):
        #need to decide whether to take the antab or difx2fits route
        pass

    def log2comments(self):
        #not ready
        pass

    def machinegen(self):
        machinegen.thread_machine_gen(self.obscode, self.cluster)


    def calcif(self, calcif_options):

        self.calcif_options = calcif_options
        calcif.run_calc(self.calc)

    def go(self, np = None, mpipath = None, mpifxcorr_path = None, timeout = None):

        if np == None:
            np = mpifxcorr.check_threads(self.machine, self.input)['n_processes']

        self.np = np
        self.mpipath = mpipath
        self.mpifxcorr_path = mpifxcorr_path
        self.timeout = timeout
        
        mpifxcorr.run_mpifxcorr(self.obscode, np, mpipath, self.machine, mpifxcorr_path, self.input, timeout)

    def killdifx(self, killdifx_executable, killdifx_options, rsh):

        self.killdifx_executable = killdifx_executable
        self.killdifx_options = killdifx_options
        self.rsh = rsh

        killdifx.run_killall(self.machine, killdifx_executable, killdifx_options, rsh)


    def difx2fits(self, difx2fits_options = None, delete = None):

        self.delete = delete
        self.difx2fits_options = difx2fits_options

        difx2fits.run_difx2fits(self.obscode, self.fits_file, difx2fits_options, delete)


    def run_evn(self):
        pass


    def doall(self):
        self.vex2input()
        self.vex2calc()
        self.log2input()
        self.machinegen()
        self.calcif()
        self.killdifx()
        self.go()
        self.killdifx()
        self.difx2fits()

