"""utility classes/functions primarily related to regression testing of HOPS/fourfit"""

#core imports
from __future__ import print_function
from __future__ import division
from future import standard_library
standard_library.install_aliases()
from builtins import str
from past.utils import old_div
from builtins import range
from io import open
import os
import sys
import threading
import subprocess
import glob
import tempfile
import bisect
import shutil
#set up module level logger
import logging
hopstestb_logger = logging.getLogger(__name__)

#hops package python libs
import mk4b
import afiob

class FourFitThread(threading.Thread):
    """ a wrapper for the threading.Thread class for running fourfit instances, needed so we can get function return values """
    def __init__(self, group=None, target=None, name=None, args=(), kwargs=None):
        if kwargs == None:
            kwargs = {}
        super(FourFitThread, self).__init__(group=group, target=target, name=name)
        self.ret_value = []
        self.subargs = args
        self.subkwargs = kwargs
        if sys.version_info >= (3, 0): #deal with python3 change
            self.thread_key = '_target'
        else:
            self.thread_key = '_Thread__target'

    def run(self):
        """we override the run method to capture the return value"""
        if getattr(self, self.thread_key) != None:
            self.ret_value = getattr(self, self.thread_key)(*self.subargs, **self.subkwargs)

    def get_input_args(self):
        """ return list of arguments to the subprocess """
        return self.subargs

    def get_return_value(self):
        """ return the subprocess return value """
        return self.ret_value

    def as_string(self):
        """ return subprocess os-call as string """
        options_arg = ""
        if self.subargs[0] != "":
            options_arg = self.subargs[0]

        baseline_arg = ""
        if self.subargs[1] != "":
            baseline_arg = "-b " + self.subargs[1]

        control_arg = ""
        if self.subargs[2] == "":
            self.subargs[2] = ""
        else:
            control_arg = "-c " + self.subargs[2]

        root_file_path = self.subargs[3]
        set_commands = self.subargs[5]

        exe_arg = "fourfit" + " -m 4 " + options_arg + " " + baseline_arg + " " + control_arg + " " + root_file_path + " " + set_commands

        return exe_arg

def find_fringe_files(root_file_path, baseline='*', freq_band=''):
    """ this function returns a list of all the fringe files
    that share a directory with the root file, they can be sub-selected by
    frequency band or baseline using the optional parameters baseline and freq_band """

    (filepath, filename) = os.path.split(root_file_path)
    split_tokens = filename.split('.')
    if len(split_tokens ) != 0:
        timetag_ext = split_tokens[len(split_tokens) - 1]
    else:
        sys.exit("error: could not determine root file extension")
    match_string = filepath + "/" + baseline
    if freq_band == '':
        match_string = match_string + ".*.*." + timetag_ext
    else:
        match_string = match_string + "." + freq_band + ".*." + timetag_ext
    fringe_file_list = glob.glob(match_string)
    return fringe_file_list


def file_list_difference(original_file_list, current_file_list):
    """ returns the list of files which exist in the current_file_list but not in
    the original_file_list """
    return list( set(current_file_list).symmetric_difference(set(original_file_list)) )


def clean_up_test_fringe_files(original_file_list, current_file_list):
    """ removes all files in the current file list which were not in
    the original file list """
    files_to_remove = file_list_difference(original_file_list, current_file_list)
    for a in files_to_remove:
        if os.path.isfile(a):
            os.remove(a)

def clean_up_fringe_files(files_to_remove):
    """ remove files in the given list """
    for a in files_to_remove:
        os.remove(a)


def fourfit_generate_postscript(options, baseline, ps_file_path, control_file_path, root_file_path, verbose=False):
    """" this function calls fourfit to generate a post-script output file, the fringe file is not saved """
    current_env = os.environ.copy()
    if "-pt" in options:
        option_args = options
    else:
        option_args = options + " -pt"
    #format some command arguments to call fourfit
    postscript_arg = "-d diskfile:" + ps_file_path
    baseline_arg = "-b " + baseline
    control_arg = "-c " + control_file_path
    stdout_log = tempfile.TemporaryFile(mode="w") #dump stdout here
    stderr_log = tempfile.TemporaryFile(mode="w") #dump stderr here
    cmd_name="fourfit"
    exe_arg = cmd_name + " " + option_args + " " + postscript_arg + " " + baseline_arg + " " + control_arg + " " + root_file_path
    if verbose:
        print(exe_arg)
    new_thread = subprocess.Popen(exe_arg, shell=True, env=current_env, stdout=stdout_log, stderr=stderr_log)
    new_thread.wait()
    stdout_log.close()
    stderr_log.close()
    return


def fourfit_generate_fringe(options, baseline, control_file_path, root_file_path, verbose=True, set_commands="", fplot=False):
    """ this funciton calls fourfit to generate a fringe file, it returns the name
    of the file which it generates (fourfit options must not contain -t, -m, or -p!) """
    current_env = os.environ.copy()
    #assume there is no testing "-t" parameter in options suppressing the output
    #also need to ensure that the "-m" option is not set externally, as we need to set it to "-m 4" to get the name of the fringe file generated
    if "-m" in options:
        sys.exit("error: user not allowed to pass '-m' option to fourfit_generate_fringe()")
    if "-t" in options:
        sys.exit("error: user not allowed to pass '-t' option to fourfit_generate_fringe()")
    if "-pt" in options:
        sys.exit("error: user not allowed to pass '-pt' option to fourfit_generate_fringe()")
    if "-p" in options:
        sys.exit("error: user not allowed to pass '-p' option to fourfit_generate_fringe(), use fplot=True instead")
    #format some command arguments to call fourfit
    baseline_arg = ""
    if baseline != "":
        baseline_arg = "-b " + baseline

    control_arg = ""
    if control_file_path == "":
        control_arg = ""
    else:
        control_arg = "-c " + control_file_path
    stdout_log = tempfile.TemporaryFile(mode="w+b") #dump stdout here
    stderr_log = tempfile.TemporaryFile(mode="w+b") #dump stderr here
    cmd_name="fourfit"
    if fplot is True:
        cmd_name = cmd_name + " -p "
    if control_arg == "":
        exe_arg = cmd_name + " -m 4 " + options + " " + baseline_arg + " " + root_file_path + " " + set_commands
    else:
        exe_arg = cmd_name + " -m 4 " + options + " " + baseline_arg + " " + control_arg + " " + root_file_path + " " + set_commands
    if verbose:
        print(exe_arg)
    new_thread = subprocess.Popen(exe_arg, shell=True, env=current_env, stdout=stdout_log, stderr=stderr_log)
    new_thread.wait()
    #now read the stderr_log file to retrieve the name of the generated fringe files
    stderr_log.flush()
    stderr_log.seek(0)
    generated_file_blob = stderr_log.read()
    stdout_log.close()
    stderr_log.close()
    file_lines = generated_file_blob.splitlines()
    generated_file_list = []
    for x in file_lines:
        tokens = x.decode().split()
        if len(tokens) == 2:
            if tokens[0] == "fourfit:":
                if os.path.isfile(tokens[1]):
                    generated_file_list.append(tokens[1])
    return generated_file_list# generated_file_list


def run_fourfit(options, baseline, control_file_path, root_file_path, verbose=False):
    """ generic call to run fourfit with a set of options and particular baseline """
    current_env = os.environ.copy()
    #format some command arguments to call fourfit
    baseline_arg = "-b " + baseline
    control_arg = "-c " + control_file_path
    stdout_log = tempfile.TemporaryFile(mode="w") #dump stdout here
    stderr_log = tempfile.TemporaryFile(mode="w") #dump stderr here
    cmd_name="fourfit"
    exe_arg = cmd_name + " " + options + " " + baseline_arg + " " + control_arg + " " + root_file_path
    if verbose:
        print(exe_arg)
    new_thread = subprocess.Popen(exe_arg, shell=True, env=current_env, stdout=stdout_log, stderr=stderr_log)
    new_thread.wait()
    stdout_log.close()
    stderr_log.close()
    return


def generate_fringe_alist(root_file_path, output_file_path, fringe_file_list, version="6", verbose=False):
    """ generate an a-list file for all fringe files in a given list, associated with a single root file """
    current_env = os.environ.copy()
    if os.path.isfile(output_file_path):
        os.remove(output_file_path)
    stdout_log = tempfile.TemporaryFile(mode="w") #dump stdout here
    stderr_log = tempfile.TemporaryFile(mode="w") #dump stderr here
    cmd_name="balist"
    exe_arg = cmd_name + " -o " + output_file_path + " -v " + version + " " + root_file_path
    for x in fringe_file_list:
        exe_arg = exe_arg + " " + x
    if verbose:
        print(exe_arg)
    new_thread = subprocess.Popen(exe_arg, shell=True, env=current_env, stdout=stdout_log, stderr=stderr_log)
    new_thread.wait()
    stdout_log.close()
    stderr_log.close()

################################################################################

def generate_fringe_collection_alist(output_file_path, fringe_file_list, sort_list=True, version="6"):
    """ generate an a-list file for all fringe files in a given list, without root_files """
    current_env = os.environ.copy()
    if os.path.isfile(output_file_path):
        os.remove(output_file_path)
    stdout_log = tempfile.TemporaryFile(mode="w") #dump stdout here
    stderr_log = tempfile.TemporaryFile(mode="w") #dump stderr here
    cmd_name="balist"
    exe_arg = cmd_name + " -o " + output_file_path + " -v " + version + " "
    if sort_list:
        fringe_file_list.sort()
    for x in fringe_file_list:
        exe_arg = exe_arg + " " + x
    print(exe_arg)
    new_thread = subprocess.Popen(exe_arg, shell=True, env=current_env, stdout=stdout_log, stderr=stderr_log)
    new_thread.wait()
    stdout_log.close()
    stderr_log.close()

def execute_aedit_runfile(file_list, run_file_path, verbose=False):
    """ process data using aedit using the run-file feature to execute a list of commands """
    current_env = os.environ.copy()
    stdout_log = tempfile.TemporaryFile(mode="w") #dump stdout here
    stderr_log = tempfile.TemporaryFile(mode="w") #dump stderr here
    cmd_name="baedit"
    exe_arg = cmd_name + " -r " + run_file_path + " -f"
    for x in file_list:
        exe_arg = exe_arg + " " + x
    if verbose:
        print(exe_arg)
    new_thread = subprocess.Popen(exe_arg, shell=True, env=current_env, stdout=stdout_log, stderr=stderr_log)
    new_thread.wait()
    stdout_log.close()
    stderr_log.close()

def execute_aedit_command_string(command_string, verbose=False):
    """ process data using aedit using the -b (batch) option """
    current_env = os.environ.copy()
    stdout_log = tempfile.TemporaryFile(mode="w") #dump stdout here
    stderr_log = tempfile.TemporaryFile(mode="w") #dump stderr here
    cmd_name="baedit"
    exe_arg = cmd_name + " -b " + command_string
    if verbose:
        print(exe_arg)
    new_thread = subprocess.Popen(exe_arg, shell=True, env=current_env, stdout=stdout_log, stderr=stderr_log)
    new_thread.wait()
    stdout_log.close()
    stderr_log.close()


def check_snr_fringe(filename, snr_low, snr_high):
    """ extract the snr value from the type 208 record in the fringe file and
    check that it is in the allowed range """
    if os.path.isfile(filename):
        fileA = mk4b.mk4fringe(filename)
        snr_value = fileA.t208.contents.snr

        if (snr_value <= snr_high) and (snr_low <= snr_value):
            return True
        else:
            return False
    else:
        return False

def get_control_file_hash_from_fringe(filename):
    """ extract the hash value associated with the control file used to generate the given finge file from the type222"""
    value = 0
    if os.path.isfile(filename):
        fileA = mk4b.mk4fringe(filename)
        value = fileA.t222.contents.control_hash
    return value

def get_file_polarization_product(filename):
    """ usuing t203 and t205 determine the polarizations products associated with this file """
    if os.path.isfile(filename):
        ff = mk4b.mk4fringe(filename)
        n_channels = 16
        if ff.t205.contents.version_no != "00":
            n_channels = 64

        pol_error = False

        #this logic comes from alist, in summarize_mk4fringe.c, line 86
        refpol = ' '
        rempol = ' '
        for n in list(range(0,n_channels)):
            if ff.t205.contents.ffit_chan[n].ffit_chan_id != ' ':
                for j in list(range(0,4)):
                    channel_index = ff.t205.contents.ffit_chan[n].channels[j]
                    if not channel_index < 0:
                        if refpol == ' ':
                            refpol = ff.t203.contents.channels[channel_index].refpol
                        elif ff.t203.contents.channels[channel_index].refpol != refpol:
                            pol_error = True
                        if rempol == ' ':
                            rempol = ff.t203.contents.channels[channel_index].rempol
                        elif ff.t203.contents.channels[channel_index].rempol != rempol:
                            pol_error = True

        if pol_error:
            print("Error, inconsistent polarization products in file: ", filename)
            refpol = '*'
            rempol = '*'
            return refpol + rempol
        else:
            return refpol + rempol

def get_byte_value(char_array, index):
    if sys.version_info > (3,):
        return char_array[index]
    else:
        return ord(char_array[index])

def get_file_polarization_product_provisional(filename):
    """ determine the polarization product (passed as command line option to fourfit) that is
    associated with a given fringe file, this method is provisional"""
    pol_list = []
    if os.path.isfile(filename):
        ff = mk4b.mk4fringe(filename)
        offset = 64 #offset used to make it an alphanum char
        polstr = ff.t208.contents.unused1 #we stashed this data in here until we can find a better place for it
        passpol = get_byte_value(polstr,0) - offset
        parampol = get_byte_value(polstr,1) - offset
        polcode = 0

        #determine fringe polcode
        if parampol == 0:
            #fringe file was generated as part of an 'all' pol-prods collection
            polcode = passpol
            if polcode == 0:
                pol_list.append('XX')
            if polcode == 1:
                pol_list.append('YY')
            if polcode == 2:
                pol_list.append('XY')
            if polcode == 3:
                pol_list.append('YX')
        else:
            polcode = parampol
            #now determine which polarization products are present and
            #create a list of the polarization products which were included
            #in the fit of this fringe file
            if polcode & 0b00001 and not polcode & 0b10000:
                pol_list.append('XX')
            if polcode & 0b00010 and not polcode & 0b10000:
                pol_list.append('YY')
            if polcode & 0b00100 and not polcode & 0b10000:
                pol_list.append('XY')
            if polcode & 0b01000 and not polcode & 0b10000:
                pol_list.append('YX')
            if polcode == 31:
                pol_list.append('I')

        return pol_list

def get_polarization_products_present(corel_filename):
    """returns a list of polarization products which are available for this corel file """
    corr = mk4b.mk4corel(corel_filename)
    pp_set = set()
    #number of 'cross-corr' channels
    nindex = corr.t100.contents.nindex
    for n in list(range(0,nindex)):
        ref_chan_id = str( (corr.index[n].t101[0].ref_chan_id).decode() )
        rem_chan_id = str( (corr.index[n].t101[0].rem_chan_id).decode() )
        refpol = ref_chan_id[-1:].upper()
        rempol = rem_chan_id[-1:].upper()
        pp_set.add(refpol+rempol)
    return list(pp_set)

def get_required_pol_products(ff_pp_option):
    """ returns a list of the required pol-products needed to run the given fourfit (-P) option """
    all_possible_pp = []
    for p in ['X', 'Y', 'R', 'L']:
        for q in ['X', 'Y', 'R', 'L']:
            all_possible_pp.append( p+q )

    required_pp = set()
    for pp in all_possible_pp:
        if pp in ff_pp_option:
            required_pp.add(pp)

    if 'I' in ff_pp_option:
        required_pp = required_pp.union( set(['XX', 'YY', 'XY', 'YX']) )

    return list(required_pp)


def compare_fringe_files(filenameA, filenameB, verbose=True, pedantic=False, ignore_dates=True, abs_tol=1e-14, rel_tol=1e-6):
    """ performs a rough comparison of data in two fringe files (useful for regression testing) """
    if os.path.isfile(filenameA) and os.path.isfile(filenameB):
        fileA = mk4b.mk4fringe(filenameA)
        fileB = mk4b.mk4fringe(filenameB)

        check_results = []
        result_list = []

        #quick and dirty, make this more robust...
        if fileA.t200 and fileB.t200:
            t200a = fileA.t200.contents
            t200b = fileB.t200.contents
            t200result = t200a.approximately_equal(t200b, ignore_dates, verbose, abs_tol, rel_tol)
            check_results.append(t200result)
            result_list.append(200)

        if fileA.t201 and fileB.t201:
            t201a = fileA.t201.contents
            t201b = fileB.t201.contents
            t201result = t201a.approximately_equal(t201b, ignore_dates, verbose, abs_tol, rel_tol)
            check_results.append(t201result)
            result_list.append(201)


        if fileA.t202 and fileB.t202:
            t202a = fileA.t202.contents
            t202b = fileB.t202.contents
            t202result = t202a.approximately_equal(t202b, ignore_dates, verbose, abs_tol, rel_tol)
            check_results.append(t202result)
            result_list.append(202)

        if fileA.t203 and fileB.t203:
            t203a = fileA.t203.contents
            t203b = fileB.t203.contents
            t203result = t203a.approximately_equal(t203b, ignore_dates, verbose, abs_tol, rel_tol)
            check_results.append(t203result)
            result_list.append(203)

        #normally do not check the type 204s because they are only a record
        #about how fourfit was run (date, platform, etc.)
        if pedantic:
            if fileA.t204 and fileB.t204:
                t204a = fileA.t204.contents
                t204b = fileB.t204.contents
                t204result = t204a.approximately_equal(t204b, ignore_dates, verbose, abs_tol, rel_tol)
                check_results.append(t204result)
                result_list.append(204)

        if fileA.t205 and fileB.t205:
            t205a = fileA.t205.contents
            t205b = fileB.t205.contents
            t205result = t205a.approximately_equal(t205b, ignore_dates, verbose, abs_tol, rel_tol)
            check_results.append(t205result)
            result_list.append(205)

        #don't check the type 206's because the captured data we have is missing records
        #for channels >16,
        if pedantic:
            if fileA.t206 and fileB.t206:
                t206a = fileA.t206.contents
                t206b = fileB.t206.contents
                t206result = t206a.approximately_equal(t206b, ignore_dates, verbose, abs_tol, rel_tol)
                check_results.append(t206result)
                result_list.append(206)

        if fileA.t207 and fileB.t207:
            t207a = fileA.t207.contents
            t207b = fileB.t207.contents
            t207result = t207a.approximately_equal(t207b,ignore_dates, verbose, abs_tol, rel_tol)
            check_results.append(t207result)
            result_list.append(207)


        if fileA.t208 and fileB.t208:
            t208a = fileA.t208.contents
            t208b = fileB.t208.contents
            t208result = t208a.approximately_equal(t208b, ignore_dates,verbose, abs_tol, rel_tol)
            check_results.append(t208result)
            result_list.append(208)

        if fileA.t210 and fileB.t210:
            t210a = fileA.t210.contents
            t210b = fileB.t210.contents
            t210result = t210a.approximately_equal(t210b, ignore_dates, verbose, abs_tol, rel_tol)
            check_results.append(t210result)
            result_list.append(210)

        #now deal with the array data elements (type_212 and type_230)

        #check type_212 objects
        n212a = fileA.n212
        n212b = fileB.n212
        have212result = False
        t212result = True
        if n212a == n212b:
            if n212a != 0:
                have212result = True
                for x in list(range(n212a)):
                    if not fileA.t212[x].contents.approximately_equal(fileB.t212[x].contents, ignore_dates, verbose, abs_tol, rel_tol):
                        t212result = False
                        break
            else:
                have212result = False
        else:
            t212result = False
            have212result = True

        if have212result:
            check_results.append(t212result)
            result_list.append(212)

        #check type_230 objects
        n230a = fileA.n230
        n230b = fileB.n230
        have230result = False
        t230result = True
        if n230a == n230b:
            if n230a != 0:
                have230result = True
                for x in list(range(n230a)):
                    if not fileA.t230[x].contents.approximately_equal(fileB.t230[x].contents, ignore_dates, verbose, abs_tol, rel_tol):
                        t230result = False
                        break
            else:
                have230result = False
        else:
            t230result = False
            have230result = True

        if have230result:
            check_results.append(t230result)
            result_list.append(230)


        if verbose:
            print("type_200 data is equal?", t200result)
            print("type_201 data is equal?", t201result)
            print("type_202 data is equal?", t202result)
            print("type_203 data is equal?", t203result)
            if pedantic:
                print("type_204 data is equal?", t204result)
            print("type_205 data is equal?", t205result)
            if pedantic:
                print("type_206 data is equal?", t206result)
            print("type_207 data is equal?", t207result)
            print("type_208 data is equal?", t208result)
            print("type_210 data is equal?", t210result)

            if have212result:
                print("type_212 data is equal?", t212result)

            if have230result:
                print("type_230 data is equal?", t230result)

        for x in list(range( len(check_results) )):
            if not check_results[x]:
                return result_list[x] #files do not match, return id of first non-matching type in list

        return 0 #files match within tolerance

    else:
        if verbose:
            print("file error")
        return 1


def compare_alist_files(filenameA, filenameB, verbose=True, pedantic=False, exit_on_first_neq=True, abs_tol=1e-14, rel_tol=1e-6):
    """performs rough comparison check of two afiles """
    if os.path.isfile(filenameA) and os.path.isfile(filenameB):
        fileA = afiob.alist(filenameA)
        fileB = afiob.alist(filenameB)
    else:
        return 2

    doFormatsMatch = True
    check_results = []
    result_list = []

    #first check that we have the same number of lines with the approriate types
    if fileA.nroot != fileB.nroot:
        doFormatsMatch = False
    if fileA.ncorel != fileB.ncorel:
        doFormatsMatch = False
    if fileA.nfringe != fileB.nfringe:
        doFormatsMatch = False
    if fileA.ntriangle != fileB.ntriangle:
        doFormatsMatch = False
    if fileA.nquad != fileB.nquad:
        doFormatsMatch = False

    if not doFormatsMatch:
        return 1
    else:
        have_root_result = False
        root_result = True
        if fileA.nroot != 0:
            have_root_result = True
            for x in list(range(fileA.nroot)):
                if not fileA.rootdata[x].approximately_equal( fileB.rootdata[x], pedantic, verbose, abs_tol, rel_tol):
                    root_result = False
                    if exit_on_first_neq:
                        break
        if have_root_result:
            check_results.append(root_result)
            result_list.append('root')

        have_corel_result = False
        corel_result = True
        if fileA.ncorel != 0:
            have_corel_result = True
            for x in list(range(fileA.ncorel)):
                if not fileA.coreldata[x].approximately_equal( fileB.coreldata[x], pedantic, verbose, abs_tol, rel_tol):
                    corel_result = False
                    if exit_on_first_neq:
                        break
        if have_corel_result:
            check_results.append(corel_result)
            result_list.append('corel')

        have_fringe_result = False
        fringe_result = True
        print("file has: ", fileA.nfringe, " fringes")
        if fileA.nfringe != 0:
            have_fringe_result = True
            for x in list(range(fileA.nfringe)):
                if not fileA.fringedata[x].approximately_equal( fileB.fringedata[x], pedantic, verbose, abs_tol, rel_tol):
                    fringe_result = False
                    if verbose:
                        print("mismatch on " + fileA.fringedata[x].baseline + " baseline fringe:", fileA.fringedata[x].scan_id + "/" + fileA.fringedata[x].source + "." + fileA.fringedata[x].root_id)
                    if exit_on_first_neq:
                        break
        if have_fringe_result:
            check_results.append(fringe_result)
            result_list.append('fringe')

        have_triangle_result = False
        triangle_result = True
        if fileA.ntriangle != 0:
            have_triangle_result = True
            for x in list(range(fileA.ntriangle)):
                if not fileA.triangledata[x].approximately_equal( fileB.triangledata[x], pedantic, verbose, abs_tol, rel_tol):
                    triangle_result = False
                    if exit_on_first_neq:
                        break
        if have_triangle_result:
            check_results.append(triangle_result)
            result_list.append('triangle')

        have_quad_result = False
        quad_result = True
        if fileA.nquad != 0:
            have_quad_result = True
            for x in list(range(fileA.nquad)):
                if not fileA.quaddata[x].approximately_equal( fileB.quaddata[x], pedantic, verbose, abs_tol, rel_tol):
                    quad_result = False
                    if exit_on_first_neq:
                        break
        if have_quad_result:
            check_results.append(quad_result)
            result_list.append('quad')

        for x in list(range( len(check_results) )):
            if not check_results[x]:
                if verbose:
                    print("files fail to match for lines containing a", result_list[x], "type.")
                return 1 #files do not match

    #made it here so files must be ok
    return 0

################################################################################


def compare_alist_files_field(filenameA, filenameB, field_name, verbose=True, exit_on_first_neq=True, pedantic=False, abs_tol=1e-14, rel_tol=1e-6):
    """performs a very rough comparison check for only one field/variable/column in two afiles"""
    if os.path.isfile(filenameA) and os.path.isfile(filenameB):
        fileA = afiob.alist(filenameA)
        fileB = afiob.alist(filenameB)
    else:
        return 2

    doFormatsMatch = True
    check_results = []
    result_list = []

    #first check that we have the same number of lines with the approriate types
    if fileA.nroot != fileB.nroot:
        doFormatsMatch = False
    if fileA.ncorel != fileB.ncorel:
        doFormatsMatch = False
    if fileA.nfringe != fileB.nfringe:
        doFormatsMatch = False
    if fileA.ntriangle != fileB.ntriangle:
        doFormatsMatch = False
    if fileA.nquad != fileB.nquad:
        doFormatsMatch = False

    if not doFormatsMatch:
        return 1
    else:
        have_root_result = False
        root_result = True
        if fileA.nroot != 0:
            have_root_result = True
            for x in list(range(fileA.nroot)):
                if not fileA.rootdata[x].approximately_equal_field( fileB.rootdata[x], field_name, pedantic, verbose, abs_tol, rel_tol):
                    root_result = False
                    if exit_on_first_neq:
                        break
        if have_root_result:
            check_results.append(root_result)
            result_list.append('root')

        have_corel_result = False
        corel_result = True
        if fileA.ncorel != 0:
            have_corel_result = True
            for x in list(range(fileA.ncorel)):
                if not fileA.coreldata[x].approximately_equal_field( fileB.coreldata[x], field_name, pedantic, verbose, abs_tol, rel_tol):
                    corel_result = False
                    if exit_on_first_neq:
                        break
        if have_corel_result:
            check_results.append(corel_result)
            result_list.append('corel')

        have_fringe_result = False
        fringe_result = True
        if fileA.nfringe != 0:
            have_fringe_result = True
            for x in list(range(fileA.nfringe)):
                if not fileA.fringedata[x].approximately_equal_field( fileB.fringedata[x], field_name, pedantic, verbose, abs_tol, rel_tol):
                    fringe_result = False
                    if verbose:
                        print("mismatch on " + fileA.fringedata[x].baseline + " baseline fringe:", fileA.fringedata[x].scan_id + "/" + fileA.fringedata[x].source + "." + fileA.fringedata[x].root_id)
                    if exit_on_first_neq:
                        break
        if have_fringe_result:
            check_results.append(fringe_result)
            result_list.append('fringe')

        have_triangle_result = False
        triangle_result = True
        if fileA.ntriangle != 0:
            have_triangle_result = True
            for x in list(range(fileA.ntriangle)):
                if not fileA.triangledata[x].approximately_equal_field( fileB.triangledata[x], field_name, pedantic, verbose, abs_tol, rel_tol):
                    triangle_result = False
                    if exit_on_first_neq:
                        break
        if have_triangle_result:
            check_results.append(triangle_result)
            result_list.append('triangle')

        have_quad_result = False
        quad_result = True
        if fileA.nquad != 0:
            have_quad_result = True
            for x in list(range(fileA.nquad)):
                if not fileA.quaddata[x].approximately_equal_field( fileB.quaddata[x], field_name, pedantic, verbose, abs_tol, rel_tol):
                    quad_result = False
                    if exit_on_first_neq:
                        break
        if have_quad_result:
            check_results.append(quad_result)
            result_list.append('quad')

        for x in list(range( len(check_results) )):
            if not check_results[x]:
                if verbose:
                    print("files fail to match for lines containing a", result_list[x], "type.")
                return 1 #files do not match

    #made it here so files must be ok
    return 0


###############################################################################
#some utilities for processing larger collections of data


def recursive_find_root_files(base_directory, sort_list=True, exclude_list=None, max_depth=None):
    """ returns a list of all the root files found in any directory (up to max_depth) under the given base_directory"""
    if exclude_list == None:
        exclude_list=['prepass', 'scratch']

    root_file_list = []
    #exlude and root files that might exist under a directory with the word 'prepass', etc in it
    exclude = set(exclude_list)
    assert os.path.isdir(base_directory)
    for current_root, subdirectories, files in os.walk(base_directory):
        current_depth = current_root[len(base_directory) + len(os.path.sep):].count(os.path.sep)
        keep_going = True
        if max_depth is None:
            keep_going = True
        else:
            if current_depth < max_depth:
                keep_going = True
            else:
                keep_going = False
        if keep_going is True:
            subdirectories[:] = [d for d in subdirectories if not any(e in d for e in exclude) ]
            for filename in files:
                #look for root files using some simple checks
                if filename.count('.') == 1: #check that there is one dot in the filename base
                    extension = os.path.splitext(filename)[1][1:].strip() #get the file extension
                    if len(extension) == 6:     #check that the extension has a length of 6 chars
                        full_name = os.path.join(current_root, filename) # create full path
                        filesize_kb = old_div(( os.path.getsize(full_name) ),1024.0)
                        if filesize_kb < 500: #more than 500kb is not likely to be a root file
                            #finally we check that the characters "VEX" are present in the first line of the file
                            tmp_file = open(full_name, 'r')
                            firstline = tmp_file.readline()
                            if "VEX" in firstline:
                                #very likely we have an actual root file, so add to list
                                root_file_list.append( os.path.abspath(full_name) )
        else:
            subdirectories[:] = []

    if sort_list is True:
        return sorted(root_file_list)
    else:
        return root_file_list

################################################################################

def recursive_find_root_files_matching_source(base_directory, source_name, sort_list=True, exclude_list=None):
    """ locate all the root (OVEX) files under the base_directory """
    if exclude_list == None:
        exclude_list=['prepass', 'scratch']

    root_file_list = []
    #exlude and root files that might exist under a directory with the word 'prepass', etc in it
    exclude = set(exclude_list)
    assert os.path.isdir(base_directory)
    for current_root, subdirectories, files in os.walk(base_directory):
        subdirectories[:] = [d for d in subdirectories if not any(e in d for e in exclude) ]
        for filename in files:
            #look for root files using some simple checks
            if filename.count('.') == 1: #check that there is one dot in the filename base
                extension = os.path.splitext(filename)[1][1:].strip() #get the file extension
                if len(extension) == 6:     #check that the extension has a length of 6 chars
                    full_name = os.path.join(current_root, filename) # create full path
                    filesize_kb = old_div(( os.path.getsize(full_name) ),1024.0)
                    if filesize_kb < 500: #more than 500kb is not likely to be a root file
                        #finally we check that the characters "VEX" are present in the first line of the file
                        tmp_file = open(full_name, 'r')
                        firstline = tmp_file.readline()
                        if "VEX" in firstline:
                            #now check if the source name is in the file name
                            if source_name in full_name:
                                root_file_list.append( os.path.abspath(full_name) )
    if sort_list is True:
        return sorted(root_file_list)
    else:
        return root_file_list

def recursive_find_station_files(base_directory):
    """ locate all of the station data files (type-3) under the base_directory """
    station_file_list = []
    assert os.path.isdir(base_directory)
    for current_root, subdirectories, files in os.walk(base_directory):
        for filename in files:
            abs_filename = os.path.abspath(filename)
            filename_base = os.path.split(abs_filename)[1]
            #look for root files using some simple checks
            if filename_base.count('.') == 2 : #check that there are two dots in the filename base
                stcode = filename_base.split('.')[0]
                if len(stcode)==1: #make sure leading section of file name is 1-char station id
                    full_name = os.path.join(current_root, filename)
                    empty = filename_base.split('.')[1]
                    if len(empty) == 0: #make sure there is nothing between the two '.'
                        extension = filename_base.split('.')[2] #get the file extension (root_id)
                        if len(extension) == 6:     #check that the extension has a length of 6 chars
                            station_file_list.append(  os.path.abspath(full_name) ) #probably a station file
    return station_file_list


def recursive_find_fringe_files(base_directory, include_autos=False, exclude_list=None):
    """r eturns a list of all the fringe (type-2) files found in any directory under the base_directory """
    if exclude_list == None:
        exclude_list=['prepass', 'scratch']
    fringe_file_list = []
    #exlude and root files that might exist under a directory with the word 'prepass',etc in it
    exclude = set(exclude_list)
    assert os.path.isdir(base_directory)
    for current_root, subdirectories, files in os.walk(base_directory):
        subdirectories[:] = [d for d in subdirectories if not any(e in d for e in exclude) ]
        for filename in files:
            abs_filename = os.path.abspath(filename)
            filename_base = os.path.split(abs_filename)[1]
            #look for root files using some simple checks
            if filename_base.count('.') == 3: #check that there are three dots in the filename base
                bline = filename_base.split('.')[0]
                if len(bline)==2: #make sure leading section of file name is 2-char baseline
                    full_name = os.path.join(current_root, filename)
                    if (include_autos is True) or (bline[0] != bline[1]): #check that this is a cross correlation if autos excluded
                        extension = filename_base.split('.')[3] #get the file extension (root_id)
                        if len(extension) == 6:     #check that the extension has a length of 6 chars
                            fringe_file_list.append(  os.path.abspath(full_name) ) #probably a fringe file
    return fringe_file_list


def recursive_find_corel_files(base_directory, include_autos=False, exclude_list=None):
    """returns a list of all the corel (type-1) files found in any directory under the current one """
    if exclude_list == None:
        exclude_list=['prepass', 'scratch']
    corel_file_list = []
    #exlude and root files that might exist under a directory with the word 'prepass', etc in it
    exclude = set(exclude_list)
    assert os.path.isdir(base_directory)
    for current_root, subdirectories, files in os.walk(base_directory):
        subdirectories[:] = [d for d in subdirectories if not any(e in d for e in exclude) ]
        for filename in files:
            abs_filename = os.path.abspath(filename)
            filename_base = os.path.split(abs_filename)[1]
            #look for corel files using some simple checks
            if filename_base.count('.') == 2: #check that there are two dots in the filename base
                bline = filename_base.split('.')[0]
                if len(bline)==2: #make sure leading section of file name is 2-char baseline
                    full_name = os.path.join(current_root, filename)
                    if (include_autos is True) or (bline[0] != bline[1]): #check that this is a cross correlation if autos excluded
                        extension = filename_base.split('.')[2] #get the file extension (root_id)
                        if len(extension) == 6:     #check that the extension has a length of 6 chars
                            corel_file_list.append(  os.path.abspath(full_name) ) #probably a corel file
    return corel_file_list


################################################################################

def batch_fourfit_generate_fringe(options, baseline, control_file_path, root_file_list, set_commands=""):
    """ runs fourfit over a list of root files, for a given control file, options, baseline, and commands
    returns a list of the generated fringe files """
    generated_frige_files = []
    for rf in root_file_list:
        ff_list = fourfit_generate_fringe(options, baseline, control_file_path, rf, set_commands)
        if len(ff_list) != 0:
            for ff in ff_list:
                generated_frige_files.append(ff)
    return generated_frige_files


################################################################################


def batch_fourfit_generate_fringe_parallel(options_list, baseline_list, control_file_path, root_file_list, set_commands="", max_num_processes=1):
    """ runs independent fourfit jobs in parallel on a list of files using as many cores as allowed
    the organization of the work load is multiplicative across (options_list X baseline_list X root_file_list)
    for example if you have options_list=["-P XX", "-P YY"], baseline_list=["GE"]
    and the root_file_list=["filaA", "fileB"], this means you will end up running
    four processes in total (2 polarization options X 1 baseline X two files)
    the same control file and parameters in 'set_commands' are applied to every process """

    verbosity = False
    show_plot = False

    #first determine the total number of jobs we need to run
    n_opts = len(options_list)
    n_baselines = len(baseline_list)
    n_files = len(root_file_list)
    n_total = n_opts*n_baselines*n_files

    #easiest to just construct a list arg lists for each process we need to run
    arg_list = []
    for nf in list(range(0,n_files)):
        for nb in list(range(0,n_baselines)):
            for no in list(range(0,n_opts)):
                arg_list.append([options_list[no], baseline_list[nb], control_file_path, root_file_list[nf], verbosity, set_commands, show_plot])

    print("Executing " + str(n_total) + " fourfit processes, using up to " + str(max_num_processes) + " processes at once.")

    threads = []
    generated_fringe_files = []
    #generate new threads, and monitor until all data is processed
    while threads or arg_list:

        #spawn a new thread if we are not using the max number of processes and there are still files to process
        if (len(threads) < max_num_processes) and (len(arg_list) != 0 ):
            t_args = arg_list.pop()
            t = FourFitThread(target=fourfit_generate_fringe, args=t_args)
            t.setDaemon(True)
            t.start()
            threads.append(t)

        #we have already spawned the max number of process (or there is no data left to run on) so just
        #monitor the running processes and remove them once they are finished
        else:
            for thread in threads:
                if not thread.isAlive():
                    generated_files = thread.get_return_value()
                    #generated_files = thread.join()
                    generated_fringe_files.extend( generated_files )
                    threads.remove(thread)
                    #print("Finished process!")
    return generated_fringe_files

################################################################################
################################################################################
#some afile procesing utilities, needs to be moved to a separate python lib
################################################################################
################################################################################

def get_file_fringelines(filename):
    """get a list of all the fringe line summaries in a afile """
    results=[]

    if os.path.isfile(filename):
        fileA = afiob.alist(filename)
    else:
        return results

    if fileA.nfringe != 0:
        for x in list(range(fileA.nfringe)):
            results.append(fileA.fringedata[x])
    return results

def get_rootid(afile_data_element):
    """ simply returns the root id associated with an afile line """
    return afile_data_element.root_id


def sort_by_rootid(afile_line_list):
    """ sort list of afile data-lines by root id """
    return sorted(afile_line_list, key=get_rootid)

def match_by_rootid(afile_line_listA, afile_line_listB):
    """ function to pair up corresponding measurement lines in two separate afiles
    which may have been processed differently and not necessarily in the same order """

    list_of_matched_elements = []

    list_a = sort_by_rootid(afile_line_listA)
    list_b = sort_by_rootid(afile_line_listB)

    id_index_pair_a = []
    for x in list(range(len(list_a))):
        id_index_pair_a.append( (list_a[x].root_id, x) )

    id_index_pair_b = []
    for x in list(range(len(list_b))):
        id_index_pair_b.append( (list_b[x].root_id, x) )

    #now search for the elements which match according to root id
    for x in id_index_pair_a:
        position = bisect.bisect_left(id_index_pair_b, x)
        if position == 0:
            if x[0] == id_index_pair_b[position][0]:
                #make the tuple
                tup = (list_a[ x[1] ], list_b[ id_index_pair_b[position][1] ] )
                list_of_matched_elements.append(tup)
        elif position == len(id_index_pair_b) or position == (len(id_index_pair_b) - 1):
            pos = (len(id_index_pair_b) - 1)
            if x[0] == id_index_pair_b[pos][0]:
                #make the tuple
                tup = (list_a[ x[1] ], list_b[ id_index_pair_b[pos][1] ] )
                list_of_matched_elements.append(tup)
        else:
            #need to check the element at the same position as well
            #as both the elements in front and behind
            if x[0] == id_index_pair_b[position][0]:
                #make the tuple
                tup = (list_a[ x[1] ], list_b[ id_index_pair_b[position][1] ] )
                list_of_matched_elements.append(tup)
            elif x[0] == id_index_pair_b[position-1][0]:
                #make the tuple
                tup = (list_a[ x[1] ], list_b[ id_index_pair_b[position-1][1] ] )
                list_of_matched_elements.append(tup)
            elif x[0] == id_index_pair_b[position+1][0]:
                #make the tuple
                tup = (list_a[ x[1] ], list_b[ id_index_pair_b[position+1][1] ] )
                list_of_matched_elements.append(tup)

    return list_of_matched_elements


def extract_list_field(element_list, field_name):
    """pulls out a particular element from all the objects in a list, and returns them in a list """
    results = []
    for x in element_list:
        temp_result = getattr(x, field_name)
        if not temp_result is None:
            results.append(temp_result)
    return results


def extract_alist_field(filenameA, field_name):
    """extracts a single column from an a-list, by its field name (see afiob.py)
    entries are stored in row (line) order """
    results = []

    NonObject = None

    if os.path.isfile(filenameA):
        fileA = afiob.alist(filenameA)
    else:
        return results

    have_results = False

    if fileA.nroot != 0:
        for x in list(range(fileA.nroot)):
            temp_result = getattr(fileA.rootdata[x], field_name, NonObject)
            if temp_result is None:
                break
            else:
                have_results = True
                results.append( temp_result )
        if have_results:
            return results

    if fileA.ncorel != 0:
        for x in list(range(fileA.ncorel)):
            temp_result = getattr(fileA.coreldata[x], field_name, NonObject)
            if temp_result is None:
                break
            else:
                have_results = True
                results.append( temp_result )
        if have_results:
            return results

    if fileA.nfringe != 0:
        for x in list(range(fileA.nfringe)):
            temp_result = getattr(fileA.fringedata[x], field_name, NonObject)
            if temp_result is None:
                break
            else:
                have_results = True
                results.append( temp_result )
        if have_results:
            return results

    if fileA.ntriangle != 0:
        for x in list(range(fileA.ntriangle)):
            temp_result = getattr(fileA.triangledata[x], field_name, NonObject)
            if temp_result is None:
                break
            else:
                have_results = True
                results.append( temp_result )
        if have_results:
            return results

    if fileA.nquad != 0:
        for x in list(range(fileA.nquad)):
            temp_result = getattr(fileA.quaddata[x], field_name, NonObject)
            if temp_result is None:
                break
            else:
                have_results = True
                results.append( temp_result )
        if have_results:
            return results

    return results


def extract_alist_fringe_line_pairs(filenameA, filenameB):
    """function to pull lines from two afiles, match them by the root_id,
    then return them in a list of pairs"""

    #get fringe lines from each file
    fringesA = get_file_fringelines(filenameA)
    fringesB = get_file_fringelines(filenameB)

    #sort them by root id
    fringesAsorted = sort_by_rootid(fringesA)
    fringesBsorted = sort_by_rootid(fringesB)

    #match the corresponding lines from each file by root_id
    matched_lines = match_by_rootid(fringesAsorted, fringesBsorted)

    return matched_lines


def extract_alist_fringe_field_pairs(list_of_pairs, field_name):
    """function to pull out the field of interest from a list of matched line pairs"""
    #extract the field of interest
    results = []
    NonObject = None
    #split the matched lines back into two lists
    for x in list_of_pairs:
        temp_resultA = getattr(x[0], field_name, NonObject)
        temp_resultB = getattr(x[1], field_name, NonObject)
        if temp_resultA is None or temp_resultB is None:
            break
        else:
            tup = [temp_resultA, temp_resultB]
            results.append(tup)
    return results

def extract_alist_fringe_field_pairs_from_file( filenameA, filenameB, field_name):
    """ function to pull lines from two afiles, match them by the root_id,
    then extract the field/column of interest, then return them in a list of pairs """
    #(combines the above two functions in one call)
    matched_list = extract_alist_fringe_line_pairs(filenameA, filenameB)
    return extract_alist_fringe_field_pairs(matched_list, field_name)


def filter_alist_fringe_field_pairs(list_of_pairs, field_name, field_lower_limit, field_upper_limit):
    """function to filter out certain afile line-pairs based on some allowed range on
    on a particular data field (numerical data only at the moment) """
    results = []
    NonObject = None
    #check the field value for each element of the pair is in the specified range
    for x in list_of_pairs:
        temp_resultA = getattr(x[0], field_name, NonObject)
        temp_resultB = getattr(x[1], field_name, NonObject)
        if temp_resultA is None or temp_resultB is None:
            break
        else:
            if temp_resultA >= field_lower_limit and temp_resultA <= field_upper_limit:
                if temp_resultB >= field_lower_limit and temp_resultB <= field_upper_limit:
                    tup = [ x[0], x[1] ]
                    results.append(tup)
    return results

def pairwise_difference(list_of_pairs):
    """take the difference between each element in a list of paired data"""
    results = []
    for x in list_of_pairs:
        diff = x[0] - x[1]
        results.append(diff)
    return results

#some utilities needed to make scratch directories for fringe fitting work
#this is needed so we can fringe fit without polluting the original data directory
#and we so we do not need to completely replicate the data


def ignore_files(directory, directory_contents):
    """this function is to be passed to shutil.copytree in order to ignore regular files
    as well as directories which contain a specific pattern (scratch or prepass) """
    objects_to_ignore = []
    for obj in directory_contents:
        if os.path.isfile( os.path.join(directory, obj) ):
            objects_to_ignore.append(obj)
        elif os.path.isdir( os.path.join(directory, obj) ):
            if 'scratch' in os.path.join(directory, obj) or 'prepass' in os.path.join(directory, obj) :
                objects_to_ignore.append(obj)
    return objects_to_ignore

def mirror_directory_with_symlinks(source_root, dest_root, mk4types_only=True, exclude_list=None):
    """ mirror the files in one directory to a scratch data directory containing only symlinks """
    if exclude_list == None:
        exclude_list=['prepass', 'scratch']
    assert os.path.isdir(source_root)
    #check that the destination directory doesn't already exists
    if not os.path.exists(dest_root):
        #first we copy the directory structure
        shutil.copytree(source_root, dest_root, ignore=ignore_files)
        #then we recursively create symlinks for each file from source to dest
        #all directories which match the exclude pattern will not have symlinks placed in them
        for current_root, subdirectories, files in os.walk(source_root):
            subdirectories[:] = [d for d in subdirectories if not any(e in d for e in exclude_list) ]
            for filename in files:
                if mk4types_only is True:
                    #only create symlink if this is a mk4type file (type-1, type-2, type-3, or ovex)
                    full_name = os.path.join(current_root, filename)
                    fileroot, fileext = os.path.splitext(full_name)
                    if len(fileext.strip('. ')) == 6: #assumption is that all mk4type files have a 6-char 'root' code
                        target_abspath = os.path.abspath(full_name)
                        target_relpath = os.path.relpath(target_abspath, source_root)
                        #now we create a symlink from the file to dest_root/file_relpath
                        link_path = os.path.abspath( os.path.join(dest_root, target_relpath) )
                        os.symlink(target_abspath, link_path)
                else:
                    #create symlink to this file regardless
                    full_name = os.path.join(current_root, filename)
                    target_abspath = os.path.abspath(full_name)
                    target_relpath = os.path.relpath(target_abspath, source_root)
                    #now we create a symlink from the file to dest_root/file_relpath
                    link_path = os.path.abspath( os.path.join(dest_root, target_relpath) )
                    os.symlink(target_abspath, link_path)
