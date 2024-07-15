#!/usr/bin/env python
import os
import re
import subprocess
import argparse
import shutil
from astropy.io import fits 
import warnings
import filecmp
import requests
import itertools
import glob


def get_testdir():
  filepath = __file__
  filepath = filepath[:-11]
  return filepath

def pre_checks():
  locate = None
  locate = shutil.which('mpifxcorr')
  if (locate == None):
    print('mpifxcorr not found please make sure DiFX is installed')
    quit()
  locate = shutil.which('generateVDIF')
  if (locate == None):
    print('generateVDIF not found, please make sure it is installed.')  
    quit()
  locate = shutil.which('vdifsim')
  if (locate == None):
    print('vdifsim not found, please make sure it is installed.')
    quit()



def generate_vdifsim_config(cores):
  filepath = os.path.expanduser('~') 
  #print(filepath)
  filename = filepath + "/.vdifsim" 
  fh = open(filename,"w")

  core_list = cores.split(",")

  filecontents = "# Processing computers\n"
  for item in core_list:
    filecontents = filecontents + item + "\n"


  filecontents = filecontents + "\n# data destinations\n"
  filecontents = filecontents + "BR /fred/oz168/mdutka/DiFX_TRUNK_09-19-2023/tests/DiFXtest_vdifsim/testdata/ localhost\n"
  filecontents = filecontents + "FD /fred/oz168/mdutka/DiFX_TRUNK_09-19-2023/tests/DiFXtest_vdifsim/testdata/ localhost\n"
  filecontents = filecontents + "HN /fred/oz168/mdutka/DiFX_TRUNK_09-19-2023/tests/DiFXtest_vdifsim/testdata/ localhost\n"
  filecontents = filecontents + "KP /fred/oz168/mdutka/DiFX_TRUNK_09-19-2023/tests/DiFXtest_vdifsim/testdata/ localhost\n"
  filecontents = filecontents + "LA /fred/oz168/mdutka/DiFX_TRUNK_09-19-2023/tests/DiFXtest_vdifsim/testdata/ localhost\n"
  filecontents = filecontents + "MK /fred/oz168/mdutka/DiFX_TRUNK_09-19-2023/tests/DiFXtest_vdifsim/testdata/ localhost\n"
  filecontents = filecontents + "NL /fred/oz168/mdutka/DiFX_TRUNK_09-19-2023/tests/DiFXtest_vdifsim/testdata/ localhost\n"
  filecontents = filecontents + "OV /fred/oz168/mdutka/DiFX_TRUNK_09-19-2023/tests/DiFXtest_vdifsim/testdata/ localhost\n"
  filecontents = filecontents + "PT /fred/oz168/mdutka/DiFX_TRUNK_09-19-2023/tests/DiFXtest_vdifsim/testdata/ localhost\n"
  filecontents = filecontents + "SC /fred/oz168/mdutka/DiFX_TRUNK_09-19-2023/tests/DiFXtest_vdifsim/testdata/ localhost\n"
  

  fh.write(filecontents)
  fh.close()
  

# For now just set threads to 1 per machine to get repeatable results 
def set_num_threads(testname):

  current_directory = get_testdir()
  working_directory = current_directory + "/" + testname + "/"
  v2d_filename = working_directory + testname + ".v2d"
  print(v2d_filename)
  with open(v2d_filename,'r') as file:
    content = file.read()
    content = content.replace("nThread = 12","nThread = 1");
  with open(v2d_filename,'w') as file:
    file.write(content)

def generate_v2d(testname):


  current_directory = get_testdir()
  working_directory = current_directory + "/" + testname + "/"
  testdata_dir = current_directory + "/testdata"

  args = "oms2v2d --sim " + testname + ".oms "
  subprocess.call(args,cwd=working_directory,shell=True,stdout=subprocess.DEVNULL,stderr=subprocess.STDOUT)
  set_num_threads(testname)
  return



def generate_filelist(testname):

  filepath = get_testdir()

  current_directory = get_testdir()
  working_directory = current_directory + "/" + testname + "/"
  testdata_dir = current_directory + "/testdata"
  if (testname[-3:] == "gpu"):
    basename = testname[:-4]  
    args = "makesimfilelist " + testname + ".vex " + testdata_dir + " --name=" + basename
    subprocess.call(args,cwd=working_directory,shell=True,stdout=subprocess.DEVNULL,stderr=subprocess.STDOUT)
    contents = os.listdir(working_directory)
    for item in contents:
      if (item[-9:] == ".filelist"):
        filelist_name = working_directory + item  
        str1 = testname[:-4] + "."
        str2 = testname + "." 
        new_filelist_name = filelist_name.replace(str1,str2)
        os.rename(filelist_name,new_filelist_name)
  else:
    args = "makesimfilelist " + testname + ".vex " + testdata_dir
    subprocess.call(args,cwd=working_directory,shell=True,stdout=subprocess.DEVNULL,stderr=subprocess.STDOUT)
  return 

def get_results_and_setup_files():
  current_directory = get_testdir()
  tarball = current_directory + "/tests.tgz"
  testdata_dir = current_directory + "/testdata"

  if (os.path.isdir(testdata_dir) == False):
    testdata_dir = current_directory + "/testdata"
    os.mkdir(testdata_dir)

  #print(tarball)
  if (os.path.isfile(tarball)):
    return
  else:
    url = "https://github.com/difx/difx-data/raw/main/tests.tgz"
    try:
      r = requests.get(url, allow_redirects=True)
      filename = current_directory + "/tests.tgz"
      open(filename,'wb').write(r.content)
    except:
      print("Error downloading setup files and initial benchmark results")


def filter_auto_correlations(fits_filename):


  warnings.resetwarnings() 
  warnings.filterwarnings('ignore',category=UserWarning,append=True)  
  warnings.filterwarnings('ignore', category=RuntimeWarning, append=True)
  hdulist = fits.open(fits_filename)
  tbdata = hdulist[8].data
  hdulist.close()  


  mask = (tbdata['BASELINE'] % 257) != 0
  new_tbdata = tbdata[mask] 

  fits.update(fits_filename,new_tbdata,8)
  hdulist = fits.open(fits_filename)
  tbdata = hdulist[8].data
  hdulist.close()
  warnings.resetwarnings()
  warnings.filterwarnings('always',category=UserWarning,append=True)
  warnings.filterwarnings('ignore', category=RuntimeWarning, append=True)

def get_binary_files(directory):
  input_files = []
  contents = os.listdir(directory)

  difx_dirs = []
  for item in contents:
    if (item[-5:] == '.difx'):
      difx_dirs.append(item)
    if (item[-6:] == '.input'):
      path_and_input_file = directory + "/" + item
      input_files.append(path_and_input_file)
  difx_binary_files = []
  for difx_dir in difx_dirs:
    difx_dir = directory + "/" + difx_dir
    difx_dir_contents = os.listdir(difx_dir)
    for item in difx_dir_contents:
      if (item[:4] == 'DIFX'):
        path_and_file = difx_dir + "/" + item
        difx_binary_files.append(path_and_file)
  return difx_binary_files,input_files


def get_fits_file(directory):
  contents = os.listdir(directory)
  for item in contents:
    if (item[-5:] == '.FITS'):
      file_and_path = directory + item
      return file_and_path

def get_im_file(directory):
  contents = os.listdir(directory)
  for item in contents:
    if (item[-3:] == '.im'):
      file_and_path = directory + item
      return file_and_path

def get_input_files(directory):
    contents = os.listdir(directory)
    input_files = [] 
    for item in contents:
      if (item[-6:] == '.input'):
        input_files.append(item)
    return(input_files)

def get_machine_files(directory):
    contents = os.listdir(directory)
    input_files = []
    for item in contents:
      if (item[-9:] == '.machines'):
        input_files.append(item)
    return(input_files)


def get_output_dirs(directory):
   contents = os.listdir(directory)
   output_dirs = [] 
   for item in contents:
     if (item[-5:] == '.difx'):
       output_dirs.append(item)
   #print(output_dirs)
   return(output_dirs)

def rm_output_files(testname):
  current_directory = os.getcwd()
  working_directory = current_directory + "/" + testname + "/"
  contents = os.listdir(working_directory)
  for item in contents:
      if (item[-4:] != '.vex' and item[-4:] != '.oms' and item[-7:] != ".config" and item != 'benchmark_results' and item[-8:] != ".threads"):
        fp_item = working_directory + item 
        if (item[-5:] == '.difx'):
          #print(fp_item)
          shutil.rmtree(fp_item)
        else:
          os.remove(fp_item)
  #quit()


def run_vex2difx(testname):
  current_directory = os.getcwd()
  working_directory = current_directory + "/" + testname + "/"
  vex2difxlogfile = working_directory + "/vex2difx.log"
  vex2difxerrfile = working_directory + "/vex2difx.error"
  f_vex2difxlog = open(vex2difxlogfile,"w")
  f_vex2difxerr = open(vex2difxerrfile,"w")

  arg = "vex2difx " + testname + ".v2d"

  try:
    proc =subprocess.run(arg,cwd=working_directory,shell=True,stdout=f_vex2difxlog,stderr=f_vex2difxerr,check=True)
  except subprocess.CalledProcessError as e:
    print("vex2difx failed check log files:")
    print(vex2difxlogfile)
    print(vex2difxerrfile)
    print()
    raise()

def run_difxcalc(testname):
  current_directory = os.getcwd()
  working_directory = current_directory + "/" + testname + "/"
  difxcalclogfile = working_directory + "/difxcalc.log"
  difxcalcerrfile = working_directory + "/difxcalc.error"
  f_difxcalclog = open(difxcalclogfile,"w")
  f_difxcalcerr = open(difxcalcerrfile,"w")

  if (testname == "rdv70" or testname == "v252f" or testname == "rdv70-gpu" or testname == "v252f-gpu") : 
    arg = "difxcalc *.calc"
  else:
    arg = "difxcalc *.calc" 

  
 
  proc = subprocess.run(arg,cwd=working_directory,shell=True,stdout=f_difxcalclog,stderr=f_difxcalcerr,check=True)
  contents = os.listdir(working_directory)
  im_filename = "" 
  for item in contents:
    if (item[-3:] == ".im"):
      im_filename = item

  if not im_filename:
    print("difxcalc failed check log files:")
    print(difxcalclogfile)
    print(difxcalcerrfile)
    quit()

def rm_test_data():
  testdir = get_testdir()
  testdata_dir = testdir + "/testdata"
  contents = os.listdir(testdata_dir)
  for item in contents:
    
    file_path = os.path.join(testdata_dir,item)
    os.unlink(file_path)


def run_vdifsim(testname):

  seed = "7276"   
 
  current_directory =  get_testdir()
  working_directory = current_directory + "/" + testname + "/"
  config_file = working_directory + "src_ant.config"
  vdifsimlogfile = working_directory + "/vdifsim.log"
  vdifsimerrfile = working_directory + "/vdifsim.error"
  f_vdifsimlog = open(vdifsimlogfile,"w")
  f_vdifsimerr = open(vdifsimerrfile,"w")

  #arg = "runvdifsim --seed " + seed + " " + testname + "_*.input" 
  arg = "vdifsim -s " + seed + " -c " + config_file + " "  + testname + "_*.input"
  #print(arg)
  try:
    proc = subprocess.run(arg,cwd=working_directory,shell=True,stdout=f_vdifsimlog,stderr=f_vdifsimerr,check=True)
  except subprocess.CalledProcessError as e:
    print("vdifsim failed check log files: ")
    print(vdifsimlogfile)
    print(vdifsimerrfile)
    raise()


def run_mpifxcorr(testname, numcores):
  
  print("Running test " + testname)
  
  current_directory = os.getcwd() 
  working_directory = current_directory + "/" + testname + "/"

  input_files = get_input_files(testname)
  machine_files = get_machine_files(testname)

  for (machine_file, input_file) in zip(machine_files, input_files):
    arg = "mpirun -machinefile " + machine_file + " -np " + str(numcores) + " mpifxcorr " + input_file
    difxlogfile = working_directory + "/mpifxcorr.log"
    difxerrfile = working_directory + "/mpifxcorr.error"
    f_difxlog = open(difxlogfile,"w")
    f_difxerr = open(difxerrfile,"w")
    try:
      proc = subprocess.run(arg,cwd=working_directory,shell=True,stdout=f_difxlog,stderr=f_difxerr,check=True)
    except subprocess.CalledProcessError as e:
      print("mpifxcorr failed check log files:")
      print(difxlogfile)
      print(difxerrfile) 
      print()
      return

def run_mpifxcorr_gpumode(testname, numcores):
  
  print("Running test " + testname)

  current_directory = os.getcwd()
  working_directory = current_directory + "/" + testname + "/"

  input_files = get_input_files(testname)
  machine_files = get_machine_files(testname)

  for (machine_file, input_file) in zip(machine_files, input_files):
    arg = "mpirun -machinefile " + machine_file + " -np " + str(numcores) + " mpifxcorr --usegpu " + input_file
    difxlogfile = working_directory + "/mpifxcorr.log"
    difxerrfile = working_directory + "/mpifxcorr.error"
    f_difxlog = open(difxlogfile,"w")
    f_difxerr = open(difxerrfile,"w")
    try:
      proc = subprocess.run(arg,cwd=working_directory,shell=True,stdout=f_difxlog,stderr=f_difxerr,check=True)
    except subprocess.CalledProcessError as e:
      print("mpifxcorr failed check log files:")
      print(difxlogfile)
      print(difxerrfile)
      print()
      return



 
def run_difx2fits(testname):

  testdir = get_testdir()
  working_directory = testdir + "/" + testname

  difx2fitslogfile = working_directory + "/difx2fits.log"
  difx2fitserrfile = working_directory + "/difx2fits.error"
  f_difx2fitslog = open(difx2fitslogfile,"w")
  f_difx2fitserr = open(difx2fitserrfile,"w")

  output_dirs = get_output_dirs(working_directory)
  

  for item in output_dirs:
    arg = "difx2fits " + item[:-5]    
    #print(arg)
    try:
      proc = subprocess.run(arg,cwd=working_directory,shell=True,stdout=f_difx2fitslog,stderr=f_difx2fitserr,check=True)
    except subprocess.CalledProcessError as e:
      print("difx2fits failed check log files:")
      print(difx2fitslogfile)
      print(difx2fitserrfile)
      print()
      raise()

def is_testdata_empty(testname):
  testdir = get_testdir()
  testdata_dir = testdir + "/testdata"
  testdata_root = testname.upper()
  testdata_root = testdata_dir + "/" + testdata_root + "*"
  testdata_files =  glob.glob(testdata_root)
  if (len(testdata_files) == 0):
    return True
  else:
    return False



def compare_results(testname, abstol, reltol):

  print("Comparing results for test " + testname)

  # results list [Binary file same as benchmark?,FITS file same as benchmark?]
  results = ["",True]

  current_directory = os.getcwd()
  working_directory = current_directory + "/" + testname + "/"   
  #print(working_directory)
  binfiles,inputfiles = get_binary_files(working_directory)
  #print("*******")
  #print(binfiles[0])
  if (len(binfiles) == 0):
    results[0] = "FAILED"
    results[1] = False
    return(results)

  binfile_stat = os.stat(binfiles[0])
  binfile_size = binfile_stat.st_size
  #print(binfile_size)
  if (binfile_size < 1): 
    results[0] = "FAILED"
    results[1] = False
    return(results)

  binfile = binfiles[0] 
  inputfile = inputfiles[0]
  results_directory = working_directory + "/benchmark_results/" 
  binfiles2,inputfiles2 = get_binary_files(results_directory) 
  benchmark_binfile = binfiles2[0]
  arg = "diffDiFX.py " + binfile + " " + benchmark_binfile + " " + "-i " + inputfile + " > binary_diff.log"   
  proc = subprocess.run(arg,cwd=working_directory,shell=True,check=True) 
  fits_file = get_fits_file(working_directory)
  fits_file_benchmark = get_fits_file(results_directory)
 # print(fits_file)
 # print(fits_file_benchmark)
 # quit()



  # filter autocorrelations out of fits file
  #print(fits_file)
  filter_auto_correlations(fits_file) 


  hdu1 = fits.open(fits_file)
  hdu2 = fits.open(fits_file_benchmark) 

  warnings.resetwarnings()
  warnings.filterwarnings('ignore', category=UserWarning, append=True) 

  fd = fits.FITSDiff(hdu1, hdu2, ignore_keywords=['DATE-MAP','CDATE','HISTORY'], atol=abstol , rtol=reltol)  
  diff_fitslog = working_directory + "/fits_diff.log"  
  output = fd.report(fileobj=diff_fitslog, indent=0, overwrite=True) 
  results[1] = fd.identical
  hdu1.close()
  hdu2.close()
  #warnings.resetwarnings()
  #warnings.filterwarnings('always', category=UserWarning, append=True)
  #print("comparing fits_file = " + fits_file + " VS. benchmark fits file = " + fits_file_benchmark)
  #print(fd.identical)
  #print(output)

  fp_binary_diff_file = working_directory + 'binary_diff.log'
  #print(fp_binary_diff_file)
  with open(fp_binary_diff_file, encoding="utf8", errors='ignore') as f:
  #with open(fp_binary_diff_file) as f:
    last_line = f.readlines()[-1]
  last_line = str(last_line)
  last_line = last_line.strip('\n')
  results[0] = last_line

  return(results)

def compare_results_gpu_v_cpu(testname, abstol, reltol):

  print("Comparing results for test " + testname)

  # results list [Binary file same as benchmark?,FITS file same as benchmark?]
  results = ["",True]

  current_directory = os.getcwd()

  

  #usb-gpu-vs-cpu

  substring = "-vs-cpu" 
  testname_gpu = testname.replace(substring,"")  
  substring = "-gpu-vs-cpu"  
  testname_cpu = testname.replace(substring,"")



  
  working_directory = current_directory + "/" + testname_gpu + "/"   
  #print(working_directory)
  binfiles,inputfiles = get_binary_files(working_directory)

  #print("*******")
  #print(binfiles[0])
  if (len(binfiles) == 0):
    results[0] = "FAILED"
    results[1] = False
    return(results)

  binfile_stat = os.stat(binfiles[0])
  binfile_size = binfile_stat.st_size
  #print(binfile_size)
  if (binfile_size < 1):
    results[0] = "FAILED"
    results[1] = False
    return(results)



  #print(binfiles)
  binfile = binfiles[0] 
  inputfile = inputfiles[0]
 
  results_directory = current_directory + "/" + testname_cpu + "/benchmark_results/"
  binfiles2,inputfiles2 = get_binary_files(results_directory) 
  benchmark_binfile = binfiles2[0]

  #print(working_directory)
  #print(benchmark_binfile)
  #print()
 # print(binfile)
 # print(benchmark_binfile)
 # print()
  arg = "diffDiFX.py " + binfile + " " + benchmark_binfile + " " + "-i " + inputfile + " > binary_diff_vs_cpu.log"   
  proc = subprocess.Popen(arg,cwd=working_directory,shell=True) 
  proc.wait() 
  fits_file = get_fits_file(working_directory)
  fits_file_benchmark = get_fits_file(results_directory)
  
  # filter autocorrelations out of fits file
  filter_auto_correlations(fits_file) 
  
 # print()
 # print(fits_file)
 # print(fits_file_benchmark)
 # print() 

  hdu1 = fits.open(fits_file)
  hdu2 = fits.open(fits_file_benchmark) 

  warnings.resetwarnings()
  warnings.filterwarnings('ignore', category=UserWarning, append=True) 
  warnings.filterwarnings('ignore', category=RuntimeWarning, append=True)
  fd = fits.FITSDiff(hdu1, hdu2, ignore_keywords=['DATE-MAP','CDATE','HISTORY'], atol=abstol , rtol=reltol)  
  diff_fitslog = working_directory + "/fits_diff_vs_cpu.log"  
  output = fd.report(fileobj=diff_fitslog, indent=0, overwrite=True) 
  results[1] = fd.identical
  hdu1.close()
  hdu2.close()
  warnings.resetwarnings()
  warnings.filterwarnings('always', category=UserWarning, append=True)
  warnings.filterwarnings('always', category=RuntimeWarning, append=True)
  #print("comparing fits_file = " + fits_file + " VS. benchmark fits file = " + fits_file_benchmark)
  #print(fd.identical)
  #print(output)

  fp_binary_diff_file = working_directory + 'binary_diff_vs_cpu.log'
  #print(fp_binary_diff_file)
  with open(fp_binary_diff_file, encoding="utf8", errors='ignore') as f:
  #with open(fp_binary_diff_file) as f:
    last_line = f.readlines()[-1]
  last_line = str(last_line)
  last_line = last_line.strip('\n')
  results[0] = last_line

  return(results)



def compare_im_files(testname):
  current_directory = os.getcwd()
  working_directory = current_directory + "/" + testname + "/"
  results_directory = working_directory + "/benchmark_results/"

  im_file = get_im_file(working_directory)
  im_file_benchmark = get_im_file(results_directory)

  if (im_file == None):
    return

  if (im_file_benchmark == None):
    return  

  if (filecmp.cmp(im_file,im_file_benchmark) == False):
    output = "Warning model file " + im_file + " differs from benchmark. difxcalc version could differ from benchmark."
    print(output) 


def update_test(testname):
  print("Updating test " + testname)

  

  current_directory = os.getcwd()
  working_directory = current_directory + "/" + testname + "/"
  results_directory = working_directory + "/benchmark_results/" 
  binfiles,inputfiles = get_binary_files(working_directory)

  # skip if difx has not run succesfully
  if len(binfiles)==0:
    return

  binfile_stat = os.stat(binfiles[0])
  binfile_size = binfile_stat.st_size
  if (binfile_size < 1):
    return
  
  current_binfile = binfiles[0]

  binfiles,inputfiles = get_binary_files(results_directory)
  results_binfile = binfiles[0]

  
  #print(current_binfile)  

  fits_file = get_fits_file(working_directory)
  fits_file_benchmark = get_fits_file(results_directory) 
  #print()
  #print(fits_file)


  #print(fits_file_benchmark)
  #print(current_binfile)
  os.remove(fits_file_benchmark)
  os.remove(results_binfile)
  args = "cp " + fits_file + " " + fits_file_benchmark
  subprocess.call(args,shell=True)
  #shutil.copy2(fits_file,fits_file_benchmark)
  shutil.copy2(current_binfile,results_binfile) 

  im_file = get_im_file(working_directory)
  im_file_benchmark = get_im_file(results_directory)
  os.remove(im_file_benchmark) 
  shutil.copy2(im_file,im_file_benchmark)

def repackage_tests(testnames):
  print("repacking tarball")
  current_directory = os.getcwd()
  tar_command = "tar -zcvf tests.tgz"
  #print("repacked") 
  for testname in testnames:
    
    working_directory = testname + "/"
    contents = os.listdir(working_directory) 
    for item in contents:
      if (item[-4:] == '.vex' or item[-4:] == '.oms' or item[-7:] == ".config" or item == 'benchmark_results'):
        #print(item)
        if (item == 'benchmark_results'):
          tar_command = tar_command + " " + working_directory + "/benchmark_results/*" 
        else:
          tar_command = tar_command + " " + working_directory + item
    #end for loop
  subprocess.call(tar_command,shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)



def unpackage_tests(testnames):

  print("Unpacking test tarball.")
  
  unpack = False
  
  for testname in testnames:
    if (os.path.isdir(testname) == False):
      unpack = True
  if (unpack):
    subprocess.call("tar -zxvf tests.tgz",shell=True, stdout=subprocess.DEVNULL, stderr=subprocess.STDOUT)

def create_test_data(testname):
 
  print("Creating test data")
  



def display_test_results(passfail):

  print()
  print()
  print("FITS FILE COMPARISON TEST RESULTS:")
  print()
  for testname in passfail:
    fits_result = "FAIL"
    if (passfail[testname][1]): fits_result = "PASS"
    string = "{0:33}{1}".format(testname, fits_result)
    print(string)

  print()
  print()
  print("BINARY FILE COMPARISON TEST RESULTS:")
  print()
  for testname in passfail:
    binary_result = passfail[testname][0]
    string = "{0:33}{1}".format(testname,binary_result)
    print(string)
  print()
  print()
  print('For more information please inspect the full log files located within each test directory *testname*/binary_diff.log  *testname*/fits_diff.log') 
  
  


def main():

  parser = argparse.ArgumentParser()
  parser.add_argument("-g","--generateVDIF", help="Generate VDIF data? (yes/[no])",default="no")
  parser.add_argument("-u","--updatetest", help="Update the default test results (yes/[no])",default="no") 
  parser.add_argument("-a","--abstol",help="Absolute tolerance for FITS file comparison (default = 1e-6)",default=1e-6)
  parser.add_argument("-r","--reltol",help="Relative tolerance for FITS file comparison (default = 0.0)",default=0.00)
  parser.add_argument("-d","--download",help="Download and run DiFX on real VLBI data? (yes/[no])",default="no")
  parser.add_argument("-t","--testgpu",help="run difx in gpu mode and compare results with benchmark (yes/[no])",default="no")
  parser.add_argument("-i","--usebenchmarkimfile",help="Use the benchmark .im file from a previous run rather than running difxcalc (yes/[no])",default="no") 
  parser.add_argument("-c","--cores",help="Comma deliminated list of the processing cores used for data simulation and correlation (default = localhost,localhost,localhost,localhost,localhost,localhost,localhost,localhost,localhost,localhost)",default="localhost,localhost,localhost,localhost,localhost,localhost,localhost,localhost,localhost,localhost")


  input_args = parser.parse_args()
  generateVDIF = input_args.generateVDIF
  generateVDIF = generateVDIF.upper()
  updatetest = input_args.updatetest
  updatetest = updatetest.upper()
  reltol = float(input_args.reltol)
  abstol = float(input_args.abstol)
  download = input_args.download
  download = download.upper()
  testgpu = input_args.testgpu
  testgpu = testgpu.upper()
  usebenchmarkimfile = input_args.usebenchmarkimfile
  usebenchmarkimfile = usebenchmarkimfile.upper()
  cores = input_args.cores

  # Number of stations + 2 is the number of coress needed, need head node
  cores_list = cores.split(",")
  numcores = len(cores_list) + 2

  # Check basic installation/compatability issues
  pre_checks()

  # generate vdifsim config file
  generate_vdifsim_config(cores)
 

  # Grab benchmark results and setup files 
  get_results_and_setup_files()

  # list of test names
  test_name_list = ["usb-2-station","usb-4-band-2-station","usb-10-station"]
  gpu_compatable_test_name_list = ["usb-2-station-gpu","usb-4-band-2-station-gpu","usb-10-station-gpu"]

  # Dictionary with pass/fail status of each test {"test name" : [binary file same (results), FITS file same (true/false)]} 
  passfail = {"usb-2-station":["",True],"usb-4-band-2-station":["",True],"usb-10-station":["",True]}



  # Add gpu mode tests to list of tests to be run 
  if (testgpu == "YES"):
    test_name_list.extend(gpu_compatable_test_name_list)
     


  if (testgpu == "YES"):
    for item in gpu_compatable_test_name_list:
      passfail[item] = ["",True]
      new_key = item + "-vs-cpu"
      passfail[new_key] = ["",True]

  # Unpack tests if they haven't been already
  unpackage_tests(test_name_list)

  if (download == "YES"):
    get_real_data()
   
  if (generateVDIF == "YES"):
   rm_test_data()
  
  # Run DiFX on all compatable tests 
  for testname in test_name_list:
    rm_output_files(testname)
    generate_v2d(testname)
    #quit()
    generate_filelist(testname)    
    #quit()
    run_vex2difx(testname)
    #quit()
    if (usebenchmarkimfile == "YES"):
      # copy benchmark im file to working directory
      current_directory = os.getcwd()
      working_directory = current_directory + "/" + testname + "/"
      results_directory = working_directory + "/benchmark_results/" 
      benchmark_im_file = get_im_file(results_directory) 
      shutil.copy2(benchmark_im_file, working_directory)
    else:
      run_difxcalc(testname)
    if (testname[-3:] != "gpu"):
      if (is_testdata_empty(testname) or generateVDIF == "YES"):
          print("running vdifsim")
          run_vdifsim(testname) 
    if (testname[-3:] == "gpu"):  
      run_mpifxcorr_gpumode(testname, numcores)
    else:
      run_mpifxcorr(testname, numcores)
    run_difx2fits(testname)

  # compare .im files
  for testname in test_name_list:
    compare_im_files(testname)
    

  # Run comparison with the results, gpu comparisons also compare gpu vs. cpu results
  for key in passfail: 
    if (key[-7:] == "-vs-cpu"):
      passfail[key] = compare_results_gpu_v_cpu(key,abstol,reltol)
    else:
      passfail[key] = compare_results(key,abstol,reltol)
    
  # Always want to make sure the gpu test gets added to the tar ball even if it wasn't run
  # this way the setup files and directory will be there if it is run in the future
  if (testgpu == "NO"):
    test_name_list.extend(gpu_compatable_test_name_list)  

  if (updatetest == "YES"):
    for testname in test_name_list:
      update_test(testname)
    repackage_tests(test_name_list)

  display_test_results(passfail)

if __name__ == "__main__":
    main()



