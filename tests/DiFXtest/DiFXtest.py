#!/usr/bin/env python3
import os
import re
import subprocess
import argparse
import shutil
from astropy.io import fits 
import warnings
import filecmp
import requests

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

def get_results_and_setup_files():
  current_directory = os.getcwd()
  url = "https://github.com/Ramenth86/DiFXtest.py-benchmark-results/raw/main/tests.tgz"
  r = requests.get(url, allow_redirects=True)
  filename = current_directory + "/tests.tgz"
  open(filename,'wb').write(r.content)


def filter_auto_correlations(fits_filename):

  hdulist = fits.open(fits_filename)
  tbdata = hdulist[8].data
  hdulist.close()  


  mask = (tbdata['BASELINE'] % 257) != 0
  new_tbdata = tbdata[mask] 

  fits.update(fits_filename,new_tbdata,8)
  hdulist = fits.open(fits_filename)
  tbdata = hdulist[8].data
  hdulist.close()

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

def rm_output_files(testname):
  current_directory = os.getcwd()
  working_directory = current_directory + "/" + testname + "/"
  contents = os.listdir(working_directory)
  for item in contents:
     if (item[-4:] != '.v2d' and item[-4:] != '.vex' and item != 'machines' and item != 'benchmark_results'  and \
         item[-6:] != '.mark4' and item[-5:] != '.vlba' and item != 'README' and item[-4:] != '.lba' and       
         item[-4:] != '.m5a' and item[-9:] != '.filelist' and item[-4:] != '.tar' and item[-8:] != ".threads"):
        fp_item = working_directory + item 
        if (item[-5:] == '.difx'):
          #print(fp_item)
          shutil.rmtree(fp_item)
        else:
          os.remove(fp_item)
  #quit()


def get_real_data():

  current_directory = os.getcwd()
 
  
  # Using fixed machines file with local host, could cause issues if there are not enough cores 
  # on localhost 8 cores needed for rdv70 and v252

  # Download data
   
  working_directory = current_directory + "/rdv70/"
  rdv70tar_fp = working_directory + "rdv70.tar"

  if (os.path.exists(rdv70tar_fp) == False):  
    if (os.path.exists(working_directory) == False):  
      mkdir_arg = "mkdir " + working_directory
      subprocess.call(mkdir_arg,shell=True)
    arg2 = "wget ftp://ftp.mpifr-bonn.mpg.de/vlbiarchive/DiFX_testdata/rdv70.tar ."
    proc2 = subprocess.Popen(arg2,cwd=working_directory,shell=True)
    proc2.wait()

  working_directory = current_directory + "/v252f/"  
  v252ftar_fp = working_directory + "v252f.tar"
  if (os.path.exists(v252ftar_fp) == False):
    if (os.path.exists(working_directory) == False):  
      mkdir_arg = "mkdir " + working_directory
      subprocess.call(mkdir_arg,shell=True)
    arg1 = "wget ftp://ftp.mpifr-bonn.mpg.de/vlbiarchive/DiFX_testdata/v252f.tar ."
    proc1 = subprocess.Popen(arg1,cwd=working_directory,shell=True)
    proc1.wait()
 
  # Set up v252f input files for DiFX
  working_directory = current_directory + "/v252f/"
  v252fv2d_fp = working_directory + "test-v252f.v2d" 
  if (os.path.exists(v252fv2d_fp) == False):
    arg = "tar -xf v252f.tar"
    proc = subprocess.Popen(arg,cwd=working_directory,shell=True)
    proc.wait()
    arg = "rm  -r reference*" 
    proc = subprocess.Popen(arg,cwd=working_directory,shell=True)
    proc.wait()
    filelists = ["at.filelist","cd.filelist","hh.filelist","ho.filelist","mp.filelist","pa.filelist"]  
    for filelist in filelists:
      filelist = working_directory + filelist
      fl = open(filelist,'r') 
      lines = fl.readlines()
      fl.close()
      ii = 0 
      for datafile in lines: 
        if (datafile.isspace() == False):
          lines[ii] = (working_directory + datafile).strip()   
        ii = ii+1
      fl = open(filelist,'w+')
      for datafile in lines:
         fl.write("%s\n" % datafile)     
      fl.close()

    # Remove unpacked vex file the online version is in dos format and not every 
    # system had a converter so a preconveted format one is bundeled with DiFXtest
    arg = "rm v252f.vex" 
    proc = subprocess.Popen(arg,cwd=working_directory,shell=True)
    proc.wait()

    arg = "mv example.v2d test-v252f.v2d"
    proc = subprocess.Popen(arg,cwd=working_directory,shell=True)
    proc.wait()
    vd2_fp = open(v252fv2d_fp,'r')
    v2d_contents = vd2_fp.read() 
    v2d_contents = v2d_contents.replace("vex = v252f.vex","vex = test-v252f.vex")
    vd2_fp.close()
    #print(v2d_contents)
    v2d_fp = open(v252fv2d_fp,"w+")
    v2d_fp.write(v2d_contents)
    vd2_fp.close()  
    #endif os.path.exists(v252fv2d_fp) == False

  # Setup v252f gpu test case
  arg  = "cp test-v252f.v2d ../v252f-gpu/test-v252f-gpu.v2d"
  proc = subprocess.Popen(arg,cwd=working_directory,shell=True)
  proc.wait()
  arg  = "cp *.filelist ../v252f-gpu"
  proc = subprocess.Popen(arg,cwd=working_directory,shell=True)
  proc.wait()
  working_directory = current_directory + "/v252f-gpu/"
  v2d_gpu = working_directory + "test-v252f-gpu.v2d"
  v2d_fp = open(v2d_gpu,'r')
  v2d_fp.seek(0)
  v2d_contents = v2d_fp.read()  
  v2d_fp.close()

  v2d_contents = v2d_contents.replace("vex = test-v252f.vex","vex = test-v252f-gpu.vex")
  print(v2d_contents)
  v2d_fp = open(v2d_gpu,"w+")
  v2d_fp.write(v2d_contents)
  v2d_fp.close()

  # Set up rdv70 input files for DiFX
  working_directory = current_directory + "/rdv70/" 
  rdv70v2d_fp = working_directory + "test-rdv70.v2d"
  #print(os.path.exists(rdv70v2d_fp))
  #quit()
  if (os.path.exists(rdv70v2d_fp) == False):
     
    arg = "tar -xf rdv70.tar"
    proc = subprocess.Popen(arg,cwd=working_directory,shell=True)
    proc.wait()
    arg = "rm  -r reference*" 
    proc = subprocess.Popen(arg,cwd=working_directory,shell=True)
    proc.wait()
    
    # insert full path in .v2d file`  
    v2dfile = working_directory + "example.v2d"
    v2d_fp = open(v2dfile,"r")
    v2d_contents = v2d_fp.read()
    v2d_fp.close()
    #print(v2d_contents)
    data_filenames = ["RDV70-KK-00205015.mark4","RDV70-KP-00205015.vlba","RDV70-LA-00205015.vlba","RDV70-NL-00205015.vlba","RDV70-NY-00205015.mark4","RDV70-ON-00205015.mark4"]
    for data_filename in data_filenames:
      data_filename_fp = working_directory + data_filename
      v2d_contents = v2d_contents.replace(data_filename,data_filename_fp)
    #print(v2d_contents)
    v2d_fp = open(v2dfile,"w+")
    v2d_fp.write(v2d_contents)
    v2d_fp.close()
    
    # Remove unpacked vex file the online version is in dos format and not every 
    # system had a converter so a preconveted format one is bundeled with DiFXtest
    arg = "rm rdv70.vex"  
    proc = subprocess.Popen(arg,cwd=working_directory,shell=True)
    proc.wait()

   
    arg = "mv example.v2d test-rdv70.v2d"
    proc = subprocess.Popen(arg,cwd=working_directory,shell=True)
    proc.wait()
    rdv70v2d = working_directory + "test-rdv70.v2d"
    vd2_fp = open(rdv70v2d,'r')
    v2d_contents = vd2_fp.read()
    v2d_contents = v2d_contents.replace("vex = rdv70.vex","vex = test-rdv70.vex")
    vd2_fp.close()
    v2d_fp = open(rdv70v2d,"w+")
    v2d_fp.write(v2d_contents)
    vd2_fp.close()
    # endif os.path.exists(rdv70v2d_fp) == False     
    # Setup rdv70 gpu test case
  #print("copying...")
  #print(working_directory)
  arg  = "cp test-rdv70.v2d ../rdv70-gpu/test-rdv70-gpu.v2d"
  proc = subprocess.Popen(arg,cwd=working_directory,shell=True) 
  proc.wait() 
  working_directory = current_directory + "/rdv70-gpu/" 
  rdv70v2d = working_directory + "test-rdv70-gpu.v2d"
  print(rdv70v2d)
  v2d_fp_gpu = open(rdv70v2d,'r')
  v2d_fp_gpu.seek(0)
  v2d_contents = v2d_fp_gpu.read()
 
   
  v2d_contents = v2d_contents.replace("vex = test-rdv70.vex","vex = test-rdv70-gpu.vex")
  v2d_fp_gpu.close()
  #print(v2d_contents)  
  v2d_fp_gpu = open(rdv70v2d,"w+")
  v2d_fp_gpu.write(v2d_contents)
  v2d_fp_gpu.close()


def run_vex2difx(testname):
  current_directory = os.getcwd()
  working_directory = current_directory + "/" + testname + "/"
  vex2difxlogfile = working_directory + "/vex2difx.log"
  vex2difxerrfile = working_directory + "/vex2difx.error"
  f_vex2difxlog = open(vex2difxlogfile,"w")
  f_vex2difxerr = open(vex2difxerrfile,"w")

  arg = "vex2difx test-" + testname + ".v2d"

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
    arg = "difxcalc test-" + testname + "_1.calc"
  else:
    arg = "difxcalc test-" + testname + ".calc", 

  
 
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
     

def runtest(testname):
  
  print("Running test " + testname)
  
  current_directory = os.getcwd() 
  working_directory = current_directory + "/" + testname + "/"



  if (testname == "rdv70" or testname == "v252f") :
    arg = "mpirun -machinefile machines -np 8 mpifxcorr test-" + testname + "_1.input"
  elif (testname == "rdv70-gpu" or testname == "v252f-gpu") :
    arg = "mpirun -machinefile machines -np 8 mpifxcorr --usegpu test-" + testname + "_1.input"
  elif ((testname != "rdv70-gpu" or testname != "v252f-gpu") and testname[-4:] == "-gpu"):
    arg = "mpirun -machinefile machines -np 4 mpifxcorr --usegpu test-" + testname + ".input"
  else:
    arg = "mpirun -machinefile machines -np 4 mpifxcorr test-" + testname + ".input"
 
  #quit()
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
  difx2fitslogfile = working_directory + "/difx2fits.log"
  difx2fitserrfile = working_directory + "/difx2fits.error"
  f_difx2fitslog = open(difx2fitslogfile,"w")
  f_difx2fitserr = open(difx2fitserrfile,"w")

  
  if (testname == "rdv70" or testname == "v252f") :
    arg = "difx2fits test-" + testname + "_1"
  else:  
    arg = "difx2fits test-" + testname
 
  try:
    proc = subprocess.run(arg,cwd=working_directory,shell=True,stdout=f_difx2fitslog,stderr=f_difx2fitserr,check=True)
  except subprocess.CalledProcessError as e:
    print("difx2fits failed check log files:")
    print(difx2fitslogfile)
    print(difx2fitserrfile)
    print()
    raise()

  
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

  fd = fits.FITSDiff(hdu1, hdu2, ignore_keywords=['DATE-MAP','CDATE','HISTORY'], atol=abstol , rtol=reltol)  
  diff_fitslog = working_directory + "/fits_diff_vs_cpu.log"  
  output = fd.report(fileobj=diff_fitslog, indent=0, overwrite=True) 
  results[1] = fd.identical
  hdu1.close()
  hdu2.close()
  #warnings.resetwarnings()
  #warnings.filterwarnings('always', category=UserWarning, append=True)
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
    #print(testname)
    if (testname == "rdv70" or testname == "v252f"):
      tar_command = tar_command + " " + working_directory + "/benchmark_results/*" + " " + working_directory + "test-" + testname + ".vex "+ working_directory + "test-" + testname + "_1.threads " + working_directory + "machines" 
      print(tar_command)
    else:    
      tar_command = tar_command + " " + working_directory + "/benchmark_results/*" + " " + working_directory + "test-" + testname + ".vex "+ working_directory + "test-" + testname + ".v2d " + working_directory + "machines"    
 


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

def create_test_data():
 
  print("Creating test data")

  # Create Synthetic VDIF Data
  DURATION=10
  
  SEED1=38573
  SEED2=58573

  if (os.path.isdir("testdata") == False):
    os.mkdir("testdata")

  
  # USB Real 
  TEST1vdiflog = "testdata/TEST1.vdif.log"
  TEST1vdiferrfile = "testdata/TEST1.vdif.error"
  f_TEST1vdiflog = open(TEST1vdiflog,"w")
  f_TEST1vdiferrfile = open(TEST1vdiferrfile,"w")
  args = "generateVDIF -seed="+str(SEED1)+" -w 4 -b 2 -C 1 -l "+str(DURATION)+" -noise -amp2 0.05 -tone2 1.5 -year 2020 -dayno 100 -time 07:00:00 testdata/TEST1.vdif" 
  subprocess.run(args,shell=True,stdout=f_TEST1vdiflog,stderr=f_TEST1vdiferrfile,check=True)


  TEST2_usbvdiflog = "testdata/TEST2-usb.vdif.log"
  TEST2_usbvdiferrfile = "testdata/TEST2-usb.vdif.error"
  f_TEST2_usbvdiflog = open(TEST2_usbvdiflog,"w")
  f_TEST2_usbvdiferrfile = open(TEST2_usbvdiferrfile,"w")
  args = "generateVDIF -seed="+str(SEED2)+" -w 4 -b 2 -C 1  -l " + str(DURATION) + " -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 testdata/TEST2-usb.vdif"
  subprocess.run(args,shell=True,stdout=f_TEST2_usbvdiflog,stderr=f_TEST2_usbvdiferrfile,check=True)
  
  
  ## LSB Real
  TEST2_lsbvdiflog = "testdata/TEST2-lsb.vdif.log"
  TEST2_lsbvdiferrfile = "testdata/TEST2-lsb.vdif.error"
  f_TEST2_lsbvdiflog = open(TEST2_lsbvdiflog,"w")
  f_TEST2_lsbvdiferrfile = open(TEST2_lsbvdiferrfile,"w")
  args = "generateVDIF -seed="+str(SEED2)+" -w 4 -b 2 -C 1  -l " + str(DURATION) + " -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 -lsb testdata/TEST2-lsb.vdif"
  subprocess.run(args,shell=True,stdout=f_TEST2_lsbvdiflog,stderr=f_TEST2_lsbvdiferrfile,check=True)
  
  
  ## Complex (single side band)
  TEST1_complex_usbvdiflog = "testdata/TEST1-complex-usb.vdif.log"
  TEST1_complex_usbvdiferrfile = "testdata/TEST1-complex-usb.vdif.error"
  f_TEST1_complex_usbvdiflog = open(TEST1_complex_usbvdiflog,"w")
  f_TEST1_complex_usbvdiferrfile = open(TEST1_complex_usbvdiferrfile,"w")
  args = "generateVDIF -seed="+str(SEED1)+" -w 4 -b 2 -C 1  -l "+str(DURATION)+" -noise -amp2 0.05 -tone2 1.5 -year 2020 -dayno 100 -time 07:00:00 -complex testdata/TEST1-complex-usb.vdif"
  subprocess.run(args,shell=True,stdout=f_TEST1_complex_usbvdiflog,stderr=f_TEST1_complex_usbvdiferrfile,check=True)

  TEST2_complex_usbvdiflog = "testdata/TEST2-complex-usb.vdif.log"
  TEST2_complex_usbvdiferrfile = "testdata/TEST2-complex-usb.vdif.error"
  f_TEST2_complex_usbvdiflog = open(TEST2_complex_usbvdiflog,"w")
  f_TEST2_complex_usbvdiferrfile = open(TEST2_complex_usbvdiferrfile,"w")
  args = "generateVDIF -seed="+str(SEED2)+" -w 4 -b 2 -C 1  -l "+str(DURATION)+" -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 -complex testdata/TEST2-complex-usb.vdif"
  subprocess.run(args,shell=True,stdout=f_TEST2_complex_usbvdiflog,stderr=f_TEST2_complex_usbvdiferrfile,check=True)
  
  TEST2_complex_lsbvdiflog = "testdata/TEST2-complex-lsb.vdif.log"
  TEST2_complex_lsbvdiferrfile = "testdata/TEST2-complex-lsb.vdif.error"
  f_TEST2_complex_lsbvdiflog = open(TEST2_complex_lsbvdiflog,"w")
  f_TEST2_complex_lsbvdiferrfile = open(TEST2_complex_lsbvdiferrfile,"w")
  args = "generateVDIF -seed="+str(SEED2)+" -w 4 -b 2 -C 1  -l "+str(DURATION)+" -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 -complex -lsb testdata/TEST2-complex-lsb.vdif"
  subprocess.run(args,shell=True,stdout=f_TEST2_complex_lsbvdiflog,stderr=f_TEST2_complex_lsbvdiferrfile,check=True)
  


  ## Complex (double side band)
  TEST2_dsb_usbvdiflog = "testdata/TEST2-dsb-usb.vdif.log"
  TEST2_dsb_usbvdiferrfile = "testdata/TEST2-dsb-usb.vdif.error"
  f_TEST2_dsb_usbvdiflog = open(TEST2_dsb_usbvdiflog,"w")
  f_TEST2_dsb_usbvdiferrfile = open(TEST2_dsb_usbvdiferrfile,"w")
  args = "generateVDIF -seed="+str(SEED2)+" -w 4 -b 2 -C 1  -l "+str(DURATION)+" -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 -complex -doublesideband testdata/TEST2-dsb-usb.vdif"
  subprocess.run(args,shell=True,stdout=f_TEST2_dsb_usbvdiflog,stderr=f_TEST2_dsb_usbvdiferrfile,check=True)



  TEST2_dsb_lsbvdiflog = "testdata/TEST2-dsb-lsb.vdif.log"
  TEST2_dsb_lsbvdiferrfile = "testdata/TEST2-dsb-lsb.vdif.error"
  f_TEST2_dsb_lsbvdiflog = open(TEST2_dsb_lsbvdiflog,"w")
  f_TEST2_dsb_lsbvdiferrfile = open(TEST2_dsb_lsbvdiferrfile,"w")
  args = "generateVDIF -seed="+str(SEED2)+" -w 4 -b 2 -C 1  -l "+str(DURATION)+" -noise -amp2 0.05 -tone2 1.0 -year 2020 -dayno 100 -time 07:00:00 -complex -doublesideband -lsb testdata/TEST2-dsb-lsb.vdif"
  subprocess.run(args,shell=True,stdout=f_TEST2_dsb_lsbvdiflog,stderr=f_TEST2_dsb_lsbvdiferrfile,check=True)
  
  f_TEST1vdiflog.close()
  f_TEST1vdiferrfile.close()
  f_TEST2_usbvdiflog.close()
  f_TEST2_usbvdiferrfile.close()
  f_TEST2_lsbvdiflog.close()
  f_TEST2_lsbvdiferrfile.close()
  f_TEST1_complex_usbvdiflog.close()
  f_TEST1_complex_usbvdiferrfile.close()
  f_TEST2_complex_usbvdiflog.close()
  f_TEST2_complex_usbvdiferrfile.close()
  f_TEST2_complex_lsbvdiflog.close()
  f_TEST2_complex_lsbvdiferrfile.close()
  f_TEST2_dsb_usbvdiflog.close()
  f_TEST2_dsb_usbvdiferrfile.close()
  f_TEST2_dsb_lsbvdiflog.close()
  f_TEST2_dsb_lsbvdiferrfile.close()


def display_test_results(passfail):

  print()
  print()
  print("FITS FILE COMPARISON TEST RESULTS:")
  print()
  for testname in passfail:
    fits_result = "FAIL"
    if (passfail[testname][1]): fits_result = "PASS"
    string = "{0:30}{1}".format(testname, fits_result)
    print(string)

  print()
  print()
  print("BINARY FILE COMPARISON TEST RESULTS:")
  print()
  for testname in passfail:
    binary_result = passfail[testname][0]
    string = "{0:30}{1}".format(testname,binary_result)
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

  # Check basic installation/compatability issues
  pre_checks()

  # Grab benchmark results and setup files
  get_results_and_setup_files()

  # list of test names
  test_name_list = ["complex-complex","lsb","lsb-complex","lsb-dsb","usb","usb-complex","usb-dsb"]
  gpu_compatable_test_name_list = ["complex-complex-gpu","lsb-gpu","lsb-complex-gpu","lsb-dsb-gpu","usb-gpu","usb-complex-gpu","usb-dsb-gpu"]

  # Dictionary with pass/fail status of each test {"test name" : [binary file same (results), FITS file same (true/false)]} 
  passfail = {"complex-complex":["",True],"lsb":["",True],"lsb-complex":["",True],"lsb-dsb":["",True],"usb":["",True],"usb-complex":["",True],"usb-dsb":["",True]}



  # Download real data tests from the ftp site

  if (download == "YES"):
    test_name_list.append("rdv70") 
    test_name_list.append("v252f")
    gpu_compatable_test_name_list.append("rdv70-gpu")
    gpu_compatable_test_name_list.append("v252f-gpu")
    passfail["rdv70"] = ["",True]  
    passfail["v252f"] = ["",True] 


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



    
  # Generate synthetic VDIF data
  if (generateVDIF == "YES" or (os.path.isdir("testdata") == False)):
    create_test_data()
   
  # Run DiFX on all compatable tests 
  for testname in test_name_list:
    rm_output_files(testname)
    run_vex2difx(testname)
    if (usebenchmarkimfile == "YES"):
      # copy benchmark im file to working directory
      current_directory = os.getcwd()
      working_directory = current_directory + "/" + testname + "/"
      results_directory = working_directory + "/benchmark_results/" 
      benchmark_im_file = get_im_file(results_directory) 
      shutil.copy2(benchmark_im_file, working_directory)
    else:
      run_difxcalc(testname)
    runtest(testname)
  
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



