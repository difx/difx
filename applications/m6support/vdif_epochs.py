#!/usr/bin/env python
#
# (c) Massachusetts Institute of Technology, 2023
# (c) Geoffrey B. Crew, 2023
#
import datetime
import sys

try:
  for ee in range(int(sys.argv[1]), int(sys.argv[2])):
    year = '2%03d' % (ee//2)
    if ee % 2 == 0: month = '01'
    else:           month = '07'
    isodate = year + '-' + month + '-01T00:00:00'
    ts = datetime.datetime.fromisoformat(isodate
       ).replace(tzinfo=datetime.timezone.utc).timestamp()
    print("    /*%3d@0.0== */ %10d, /* %s */" % (ee,ts,isodate) )
except:
    print('Usage: ',sys.argv[0], 'start-epoch', 'finish-epoch')
#
# eof
#
