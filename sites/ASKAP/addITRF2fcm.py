#!/usr/bin/python3
import astropy.coordinates
import argparse, os, sys

## Convenience function to parse java style properties file
def load_props(filepath, sep='=', comment_char='#'):
    """
    Read the file passed as parameter as a properties file.
    """
    props = {}
    with open(filepath, "rt") as f:
        for line in f:
            l = line.strip()
            if l and not l.startswith(comment_char):
                key_value = l.split(sep)
                key = key_value[0].strip()
                value = sep.join(key_value[1:]).strip().strip('"')
                keysplit = key.split('.')
                currentdict = props
                while len(keysplit) > 1:
                    if not keysplit[0] in list(currentdict.keys()):
                        currentdict[keysplit[0]] = {}
                    currentdict = currentdict[keysplit[0]]
                    keysplit = keysplit[1:]
                if "[" in value and "]" in value and not "*" in value:
                    v = []
                    for vx in value.strip()[1:-1].split(','):
                        v.append(vx)
                    value = v
                currentdict[keysplit[0]] = value
    return props

## Argument parser
parser = argparse.ArgumentParser()
parser.add_argument("fcm",  help="ASKAP .fcm file describing array")
args = parser.parse_args()
fcmlines = open(args.fcm).readlines()

## Check if an org fcm file already exists, if so bail out
if os.path.exists("%s.org" % args.fcm):
    print("%s.org already exists, aborting" % args.fcm)
    sys.exit()

## Load configuration data
fcm = load_props(args.fcm)
addlines = []
for antenna in list(fcm["common"]["antenna"].keys()):
    if not "location" in list(fcm["common"]["antenna"][antenna].keys()):
        continue
    #if not "itrf" in list(fcm["common"]["antenna"][antenna]["location"].keys()):
    wgs = fcm["common"]["antenna"][antenna]["location"]["wgs84"]
    p = astropy.coordinates.EarthLocation.from_geodetic(lon=wgs[0], lat=wgs[1], height=wgs[2])
    xyz = p.to_geocentric()
    if not "itrf" in list(fcm["common"]["antenna"][antenna]["location"].keys()):
        towrite = "common.antenna.%s.location.itrf = [%.3f, %.3f, %.3f]\n" % (antenna, xyz[0].value, xyz[1].value, xyz[2].value)
        addlines.append(towrite)
    #if "itrf" in list(fcm["common"]["antenna"][antenna]["location"].keys()):
    #    itrf = astropy.coordinates.EarthLocation.from_geocentric(x=fcm["common"]["antenna"][antenna]["location"]["itrf"][0], 
    #                                                             y=fcm["common"]["antenna"][antenna]["location"]["itrf"][1],
    #                                                             z=fcm["common"]["antenna"][antenna]["location"]["itrf"][2], unit="m")
    #    print(xyz[0]-itrf.to_geocentric()[0], xyz[1]-itrf.to_geocentric()[1], xyz[2]-itrf.to_geocentric()[2])
    #print(xyz)

newfcmout = open(args.fcm + ".updated", "w")
for line in fcmlines:
    newfcmout.write(line)
    if "wgs84" in line:
        antenna = line.split('.')[2]
        if not "itrf" in list(fcm["common"]["antenna"][antenna]["location"].keys()):
            for addline in addlines:
                if antenna in addline:
                    print("Adding", addline)
                    newfcmout.write(addline)
newfcmout.close()

os.system("mv %s %s.org" % (args.fcm, args.fcm))
os.system("mv %s.updated %s" % (args.fcm, args.fcm))
