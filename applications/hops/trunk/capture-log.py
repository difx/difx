#
# simple script to cull some useless crap from svn log
#
import re
import subprocess
import sys

# list of things to discard -- will be surrounded by .*
crap = [
    'swc/',
    'ChangeLog.txt',
    'capture-log.py',
    'README'
]

# compile the re's we'll need to use
crap_re = map(lambda x: re.compile(r'.*' + str(x) + '.*'), crap)
blok_re = re.compile(r'^--------------------------------------.*')

# command to generate the svn log based on the first argument
cmd = 'svn -v log ' + sys.argv[1]
p = subprocess.Popen(cmd.split(' '), stdout=subprocess.PIPE)

# the script: the lines in 'dump' will be output if 'ok'
dump = ''
for line in p.stdout:
    if blok_re.match(line):
        if len(dump) > 0 and ok:
            print dump
        dump = line
        ok = True
        continue
    for cre in crap_re:
        if cre.match(line):
            ok = False
            continue
    if ok:
        dump += line
p.wait()

#
# eof
#
