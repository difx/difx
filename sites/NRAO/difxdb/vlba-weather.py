#!/usr/bin/env python

import psycopg2 as pg
import sys
from os import environ

# This program is to mine the database over a specified mjd time range and 
# extract all of the weather data.
# 
# The output is one file per antenna with rows of weather information sorted 
# in time order.  Every so often the weather station should populate the 
# database with a set of monitor points with the same (or nearly so) 
# timetag.  These should be collected into one record and written like:
# 
# # Ant MJD temp(C) pressure(mbar) dewpoint(C) windspeed(m/s) winddir(degrees) rain(cm) gust(m/s)
# BR 55162.4000000 11.8 978.4 6.1 0.5 329.0 0.00 1.4
# BR 55162.4059259 11.6 978.5 6.2 0.0 247.0 0.00 1.1
# BR 55162.4118519 11.5 978.5 6.0 0.0 201.0 0.00 0.7
# BR 55162.4177778 10.8 978.5 6.0 0.0 305.0 0.00 0.2
# BR 55162.4237037 10.8 978.6 5.9 0.0 260.0 0.00 0.5
# BR 55162.4296296 10.9 978.6 5.8 0.0 320.0 0.00 0.7
# BR 55162.4355440 10.5 978.7 6.0 0.5 214.0 0.00 0.7
# 
# 
# There are 9 columns
# 1. Antenna name (upper case, the prefix to -ws in the hostname field)
# 2. MJD of the record
# 3. "temp" mon point
# 4. "hdewpt" mon point
# 5. "barometer" mon point
# 6. "windspeed" mon point
# 7. "winddir" mon point
# 8. "rain" mon point
# 9. "windgust" mon point
# 
# Comment lines begin with #.  You can just copy the one shown above and put 
# that as the first line to keep things uniform.
# 
# For mon points that don't have data in the interval a value of -999 should 
# be substituted.  The "rain" mon point probably doesn't exist for the GBT 
# but will for the VLBA once it starts populating the database with weather 
# info.
# 
# That should be all.  I have a similar program that mines the database for 
# Mark5 module information: /home/swc/DiFX-2.2/src/difxdb/mark5c2db .  I 
# hope to use what I learn from you to improve that program.

# For VLA (from Bryan):
# temperature = emr-m352.HMT337.Temperature
# pressure = emr-m352.WXT520.Pressure
# dew point temperature = emr-m352.HMT337.Dewpoint_Temperature
# wind speed = emr-m352.WXT520.Wind_Speed_Average
# wind speed max (gust) = emr-m352.WXT520.Wind_Speed_Maximum
# wind direction = emr-m352.WXT520.Wind_Direction_Average
# rain accumulation = emr-m352.WXT520.Rain_Accumulation
# the rain accumulation isn't done in the way you want - you'll have to
# interpret that.

# Parse the command line
if len(sys.argv) != 3:
    print "Usage: %s <start-time> <end-time>" % sys.argv[0]
    sys.exit(-1)

arguments = {'from': sys.argv[1], 'to': sys.argv[2]}
    
# fetch the weather data for the observation

dbAccess = environ['VLBAMPTS_DB']

db = pg.connect(dbAccess).cursor()
db.execute("""
SELECT
  split_part(hostname_timestamp, ',', 1) as hostname,
  split_part(hostname_timestamp, ',', 2)::double precision as timestamp,
  COALESCE(temp, -999) AS temp,
  COALESCE(hdewpt, -999) AS hdewpt,
  COALESCE(barometer, -999) AS barometer,
  COALESCE(windspeed, -999) AS windspeed,
  COALESCE(winddir, -999) AS winddir,
  COALESCE(rain, 0.0) AS rain,
  COALESCE(windgust, -999) AS windgust
FROM crosstab(
  $$ SELECT
       UPPER(SUBSTR(hostname,1,2)) || ',' || timestamp,
       monpointname,
       monpointvalue
     FROM mcdata
     WHERE hostname = 'gb-ws' AND
           devicename = 'WS' AND
           monpointname IN ('temp', 'hdewpt', 'barometer', 'windspeed',
                            'winddir', 'rain', 'windgust') AND
           timestamp BETWEEN %(from)s AND %(to)s
     ORDER BY 1,2 $$,
  $$ VALUES ('temp'), ('hdewpt'), ('barometer'), ('windspeed'), ('winddir'),
            ('rain'), ('windgust') $$
  ) AS ct(hostname_timestamp text,
          temp double precision,
          hdewpt double precision,
          barometer double precision,
          windspeed double precision,
          winddir double precision,
          rain double precision,
          windgust double precision)""", arguments)

print "# Ant MJD temp(C) pressure(mbar) dewpoint(C) windspeed(m/s) winddir(degrees) rain(cm) gust(m/s)"
for host, time, temp, hdewpt, barometer, windspeed, dir, rain, gust in db.fetchall():
    print "%s %f %f %f %f %f %f %f %f" % (host, time, temp, hdewpt, barometer, windspeed, dir, rain, gust)
