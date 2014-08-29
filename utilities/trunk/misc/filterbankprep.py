#!/usr/bin/python
import os
import sys
import re
import getopt 
import math

def usage():
  print ' '
  print '  filterbank.py -x <vexfile>  -s <Source Name>  -l <dir2filelist.lst>'
  print '                -m <data format e.g. Mark5B-1024-16-2>'
  print '                -t <spectrogram time resolution [microsec]>'
  print '                -n <num spectral bins per IF>'
  print '                -f <fch1>  [-p <1|2>]\n'
  print '  Most arguments are obligatory.'
  print ' '
  sys.exit(-1)
  return

def substring_after(s, delim):
  return s.partition(delim)[2]

def getClass(Class,ALLtext):
  sourcestat=()
  bit=0
  list=[]
  string=Class+'(\S*)'
  for line in ALLtext:
    endsec = re.search('=+',line)
    if not endsec:
      bit=0
    match = re.search(string,line)
    if match: 
      list.append(sourcestat)
      sourcestat=()
      bit=1
    if  bit==1:
      LineInfo= re.search(r'\s+(.*)',line)
      sourcestat=sourcestat+(LineInfo.group(1),)

  list.append(sourcestat)
  return list

def openfile(filename):
  f = open(filename,'rU')
  return f.readlines()  

################# Extracts Info From the Vex with vexpeek ################   
def extract_info(filename):
  os.system('vexpeek '+filename+' -v |less > vexstat.txt')
  f = open('vexstat.txt','rU')
  os.system('rm vexstat.txt')
  ALLtext = f.readlines()
  SourceInfo = getClass('Source ',ALLtext)  
  ScanInfo = getClass('Scan N',ALLtext)
  SourceInfo.pop(0)
  ScanInfo.pop(0)
  return SourceInfo, ScanInfo  

################# Finds all the scan number for a given source ###########
def ScanNum(Source,ScanInfo):
  string='source='+Source
  NumList =[Source]
  m=[]
  for scan in ScanInfo:
    for item in scan:
      match = get('No\S+',item)
      if match:NumList.append(item)
      if get('source=\S+',item):
        if item==string:
          m.append(1)
        else:
          m.append(0)
  NumList.pop(0)
  FinalList=[]
  count=0
  for bi in m:
    if bi == 1:
      FinalList.append(NumList[count])
    count=count+1
  return FinalList

def getPrjtNum(filename):
  match = re.search(r'(\S+)\.',filename)
  return match.group(1)

def getFBpath(PrjNum,tel,ScanNum):
  path='~/fbdata/'
  return path+PrjNum+'_KVN'+tel+'_'+ScanNum+'.fb'

######### Give a pattern and a string returns the match in that string ###
def get(string,text):
  match = re.search(string,text)
  matches=()
  if match: matches=match.group()   
  return matches

################# From FileList it finds the paths to the data ############
def getScanFile(Scan,FileList):
  text = openfile(FileList)
  Path=''
  time=''
  for line in text:
    match = get('\S+'+Scan,line)
    if match: 
      Path=match
      time = line.split()[1]
  return Path,time

################ GETS TELSCOPE NUMBER FROM FILELIST.TEL ##############
def getTele(FileList):
  Letters=get('\.(\S+)',FileList)
  if Letters=='.KU': NumID='20'
  elif Letters=='.KY': NumID='21'
  elif Letters=='.KT': NumID='22'
  else:
       Letters='.UN'
       NumID='00'
  return NumID,Letters

################ GET SOURCE INDEX NUMBER ############
def getSourceNum(Source,SourceInfo):
  count=0
  for ind in SourceInfo:
    #print "Checking "+SourceInfo[count][0]+"for "+Source
    if Source==SourceInfo[count][0]:
      ScrNum=count
    count=count+1
  return ScrNum   

############ GETS RA AND DEC FROM VEX IN RAD ########
def getCoord(Source,SourceInfo):
  Index=getSourceNum(Source,SourceInfo)
  for stuff in SourceInfo[Index]:
    if stuff.startswith('ra='):
       RA=substring_after(stuff,'ra=')
    if stuff.startswith('dec='):
       DEC=substring_after(stuff,'dec=')
  return RA,DEC

# Convert RA (deg) to H.M.S: (From http://supernovae.in2p3.fr/~baumont/phAse/src/HTML/astroTools.py)
def deg2HMS( RAin ):

   if(RAin<0):
      sign = -1
      ra   = -RAin
   else:
      sign = 1
      ra   = RAin

   h = int( ra/15. )
   ra -= h*15.
   m = int( ra*4.)
   ra -= m/4.
   s = ra*240.

   if(sign == -1):
      out = '-%02d%02d%06.3f'%(h,m,s)
   else: out = '%02d%02d%06.3f'%(h,m,s)
   
   return out
   
# Convert Decl. (deg) to D.M.S: (From http://supernovae.in2p3.fr/~baumont/phAse/src/HTML/astroTools.py)
def deg2DMS( Decin ):

   if(Decin<0):
      sign = -1
      dec  = -Decin
   else:
      sign = 1
      dec  = Decin

   d = int( dec )
   dec -= d
   dec *= 100.
   m = int( dec*3./5. )
   dec -= m*5./3.
   s = dec*180./5.

   if(sign == -1):
      out = '-%02d%02d%06.3f'%(d,m,s)
   else: out = '%02d%02d%06.3f'%(d,m,s)

   return out
   
def main(): 
  Npols = '1'
  Polstr=''

  try:
    opts,args = getopt.getopt(sys.argv[1:],"hx:l:s:t:m:o:n:f:d:")
    for item in opts:
      if item[0] == '-h': usage()
      if item[0] == '-x': VEX = item[1]
      if item[0] == '-l': FileList = item[1]
      if item[0] == '-s': Source = item[1]
      if item[0] == '-t': tsamp = item[1]
      if item[0] == '-m': mode = item[1]
      if item[0] == '-o': out = item[1]
      if item[0] == '-n': nchanPif = item[1] 
      if item[0] == '-f': fch1= item[1]
      #if item[0] == '-d': foff =item[1] 
  except getopt.GetoptError as ee:
      print 'Error: unknown command line argument.'
      usage()
 
  print 'Number of polarisations: '+Npols
  if (Npols!='1' and Npols!='2'):
    print 'Number of polarisations ('+Npols+') is odd'
  
  SourceInfo,ScanInfo = extract_info( VEX )
  ScanList= ScanNum(Source,ScanInfo)
  TeleNum,TeleName=getTele(FileList)
  ra,dec=getCoord(Source,SourceInfo)
  RA =str(deg2HMS(float(ra)*180/math.pi))
  DEC=str(deg2DMS(float(dec)*180/math.pi))
  
 
  ModeNums=re.findall('-\d+',mode)
  NumIfs=get('\d+',ModeNums[1])
  nchans=str(int(nchanPif)*int(NumIfs)/int(Npols))
  #nchans=str(int(nchanPif)*int(NumIfs))
  tsampInSec=str(float(float(tsamp)/1000000))
  foff=str(float(ModeNums[0])/int(ModeNums[2])/-2.0/int(nchanPif)/int(NumIfs))
  fch1 = str(float(fch1) +float(foff)/2.0)

  if Npols=='2':
    P1='UULLUULLUULLUULL'
    P2='RLRLRLRLRLRLRLRL'
    P1=P1[0:(int(NumIfs))]
    P2=P2[0:(int(NumIfs))]
    Polstr='-i '+P1+' -p '+P2+' '

  for scan in ScanList: 
    ScanFile,TimeSt = getScanFile(scan,FileList)
    
    if ScanFile!='':
      biNAME= Source+'_'+TimeSt+TeleName+'_'+scan  
      quote1='m5fb -b -nopol '+Polstr+ScanFile+' '+mode+' '+nchanPif+' '+tsamp+' '+biNAME+'.fb'
      quote2a='makeheader -tstart '+TimeSt+' -source_name '+Source+' -nchans '+ nchans+' -tsamp '+tsampInSec
      if Npols != '1':
        quote2a = quote2a+' -nifs '+Npols
      quote2b=' -telescope_id '+TeleNum+' -src_raj '+RA+' -src_dej '+DEC+' -fch1 '+fch1+' -foff '+foff+' > '+biNAME+'.fil'
      quote2=quote2a+quote2b
      quote3='cat '+biNAME+'.fb >> '+biNAME+'.fil' 
      print ' '
      print quote1
      print quote2
      print quote3
      #os.system(quote1)
      #os.system(quote2)
      #os.system(quote3)   


if __name__ == '__main__':
  main()
