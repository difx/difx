#!/usr/bin/env python
import os
import re
import subprocess
import argparse
import shutil
from astropy.io import fits 


parser = argparse.ArgumentParser()
parser.add_argument("--generateVDIF", help="Generate VDIF data? (yes/[no])",default="no")
parser.add_argument("--updatetest", help="Update the default test results (yes/[no])",default="no") 
input_args = parser.parse_args()
generateVDIF = input_args.generateVDIF
generateVDIF = generateVDIF.upper()
updatetest = input_args.updatetest
updatetest = updatetest.upper()


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

def rm_output_files(testname):
  current_directory = os.getcwd()
  working_directory = current_directory + "/" + testname + "/"
  contents = os.listdir(working_directory)
  for item in contents:
     if (item[-4:] != '.v2d' and item[-4:] != '.vex' and item != 'machines' and item != 'benchmark_results'):
        fp_item = working_directory + item 
        if (item[-5:] == '.difx'):
          #print(fp_item)
          shutil.rmtree(fp_item)
        else:
          os.remove(fp_item)
  #quit()


def runtest(testname):
  current_directory = os.getcwd() 
  working_directory = current_directory + "/" + testname + "/"
  arg = "vex2difx test-" + testname + ".v2d"
  proc =subprocess.Popen(arg,cwd=working_directory,shell=True)
  proc.wait()

  arg = "difxcalc test-" + testname + ".calc", 
  subprocess.Popen(arg,cwd=working_directory,shell=True)
  proc.wait()
  arg = "mpirun -machinefile machines -np 4 mpifxcorr test-" + testname + ".input"

  mpifxcorr_done = False
  # DiFX fails sometimes, re-run until it succeeds (need better fix here...)
  while (mpifxcorr_done == False):
    proc = subprocess.call(arg,cwd=working_directory,shell=True)
    contents = os.listdir(working_directory)
    for item in contents: 
      if (item[-5:] == '.difx'):
        mpifxcorr_done = True
  #proc.wait()
  arg = "difx2fits test-" + testname
  proc = subprocess.Popen(arg,cwd=working_directory,shell=True)
  proc.wait()
 # quit()


def compare_results(testname):

  # results list [Binary file same as benchmark?,FITS file same as benchmark?]
  results = [True,True]

  current_directory = os.getcwd()
  working_directory = current_directory + "/" + testname + "/"   
  #print(working_directory)
  binfiles,inputfiles = get_binary_files(working_directory)
  #print(binfiles)
  binfile = binfiles[0] 
  inputfile = inputfiles[0]
  results_directory = working_directory + "/benchmark_results/" 
  binfiles2,inputfiles2 = get_binary_files(results_directory) 
  benchmark_binfile = binfiles2[0]
  arg = "diffDiFX.py " + binfile + " " + benchmark_binfile + " " + "-i " + inputfile + " > binary_diff.log"   
  proc = subprocess.Popen(arg,cwd=working_directory,shell=True) 
  proc.wait()
  fits_file = get_fits_file(working_directory)
  fits_file_benchmark = get_fits_file(results_directory)
  #print(fits_file)
  hdu1 = fits.open(fits_file)
  hdu2 = fits.open(fits_file_benchmark) 
  fd = fits.FITSDiff(hdu1, hdu2, ignore_keywords=['DATE-MAP','CDATE','HISTORY'])  
  diff_fitslog = working_directory + "/fits_diff.log"  
  output = fd.report(fileobj=diff_fitslog, indent=0, overwrite=True) 
  results[1] = fd.identical
  print("comparing fits_file = " + fits_file + " VS. benchmark fits file = " + fits_file_benchmark)
  print(fd.identical)
  #print(output)
  return(results)

def update_test(testname):
  current_directory = os.getcwd()
  working_directory = current_directory + "/" + testname + "/"
  results_directory = working_directory + "/benchmark_results/" 
  binfiles,inputfiles = get_binary_files(results_directory)
  results_binfile = binfiles[0]
  binfiles,inputfiles = get_binary_files(working_directory)
  current_binfile = binfiles[0]
  #print(results_binfile)
  #print(current_binfile)  
  fits_file = get_fits_file(working_directory)
  fits_file_benchmark = get_fits_file(results_directory) 
  #print()
  #print(fits_file)
  #print(fits_file_benchmark)
  os.remove(fits_file_benchmark)
  os.remove(results_binfile)
  args = "cp " + fits_file + " " + fits_file_benchmark
  subprocess.call(args,shell=True)
  #shutil.copy2(fits_file,fits_file_benchmark)
  shutil.copy2(current_binfile,results_binfile) 


def repackage_tests(testnames):
  current_directory = os.getcwd()
  tar_command = "tar -zcvf tests.tgz"
 
  for testname in testnames:
    working_directory = testname + "/"
    tar_command = tar_command + " " + working_directory + "/benchmark_results/*" + " " + working_directory + "test-" + testname + ".vex "+ working_directory + "test-" + testname + ".v2d " + working_directory + "machines"    
     #end for loop
  subprocess.call(tar_command,shell=True)


if (generateVDIF == "YES"):
  # Create Synthetic VDIF Data
  DURATION=10
  
  SEED1=38573
  SEED2=58573
  
  # USB Real 
  args = "generateVDIF -seed="+str(SEED1)+" -w 4 -b 2 -C 1 -l "+str(DURATION)+" -noise -amp2 0.05 -tone2 1.5 -year 2020 -dayno 100 -time 07:00:00 testdata/TEST1.vdif"
  #print(args)
  subprocess.call(args,shell=True)
  args = "generateVDIF -seed="+str(SEED2)+" -w 4 -b 2 -C 1  -l " + str(DURATION) + " -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 testdata/TEST2-usb.vdif"
  #print(args)
  subprocess.call(args,shell=True)
  
  
  ## LSB Real
  args = "generateVDIF -seed="+str(SEED2)+" -w 4 -b 2 -C 1  -l " + str(DURATION) + " -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 -lsb testdata/TEST2-lsb.vdif"
  #print(args)
  subprocess.call(args,shell=True)
  
  
  ## Complex (single side band)
  args = "generateVDIF -seed="+str(SEED1)+" -w 4 -b 2 -C 1  -l "+str(DURATION)+" -noise -amp2 0.05 -tone2 1.5 -year 2020 -dayno 100 -time 07:00:00 -complex testdata/TEST1-complex-usb.vdif"
  #print(args)
  subprocess.call(args,shell=True)
  args = "generateVDIF -seed="+str(SEED2)+" -w 4 -b 2 -C 1  -l "+str(DURATION)+" -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 -hilbert testdata/TEST2-complex-usb.vdif"
  #print(args)
  subprocess.call(args,shell=True)
  
  args = "generateVDIF -seed="+str(SEED2)+" -w 4 -b 2 -C 1  -l "+str(DURATION)+" -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 -hilbert testdata/TEST2-complex-usb.vdif"
  #print(args)
  subprocess.call(args,shell=True)
  args = "generateVDIF -seed="+str(SEED2)+" -w 4 -b 2 -C 1  -l "+str(DURATION)+" -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 -hilbert -lsb testdata/TEST2-complex-lsb.vdif"
  #print(args)
  subprocess.call(args,shell=True)
  
  ## Complex (double side band)
  args = "generateVDIF -seed="+str(SEED2)+" -w 4 -b 2 -C 1  -l "+str(DURATION)+" -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 -hilbert -doublesideband testdata/TEST2-dsb-usb.vdif"
  #print(args)
  subprocess.call(args,shell=True)
  args = "generateVDIF -seed="+str(SEED2)+" -w 4 -b 2 -C 1  -l "+str(DURATION)+" -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 -hilbert -doublesideband -lsb testdata/TEST2-dsb-lsb.vdif"
  #print(args)
  subprocess.call(args,shell=True)
# end if (generateVDIF == "YES"):  


# Run test correlations
test_name_list = ["complex-complex","lsb","lsb-complex","lsb-dsb","usb","usb-complex","usb-dsb"]

# Dictionary with pass/fail status of each test {"test name" : [binary file same (true/false), FITS file same (true/false)]}
passfail = {"complex-complex":[True,True],"lsb":[True,True],"lsb-complex":[True,True],"lsb-dsb":[True,True],"usb":[True,True],"usb-complex":[True,True],"usb-dsb":[True,True]}

#  
for testname in test_name_list:
  rm_output_files(testname)
  runtest(testname)

# Run comparison with the results
for testname in test_name_list:
  compare_results(testname)

if (updatetest == "YES"):
  for testname in test_name_list:
    update_test(testname)
  repackage_tests(test_name_list)

#repackage_tests(test_name_list)



