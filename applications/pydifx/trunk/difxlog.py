"""
Import the basic logging functions then set some defaults
can be edited whenever needed

TODO change to another logger other than root
TODO add a logging level VERBOSE between INFO and DEBUG?
"""
import os.path
import logging
import logging.handlers
from logging import debug, info, warning, error, critical, exception

try:
    from observation import logpath, logname, logfile_level, stdout_level
except ImportError:
    logpath = '.'
    logname = 'log'

logging.root.setLevel(logfile_level)

# Log to stdout
s1 = logging.StreamHandler()
f1 = logging.Formatter("%(asctime)s\t%(levelname)s\t%(filename)s:%(lineno)d\t%(message)s")
s1.setFormatter(f1)
logging.root.addHandler(s1)

# Log to logfile
#f2 = logging.Formatter("%(levelname)s %(asctime)s %(funcName)s %(lineno)d %(message)s")
f2 = f1
try:
    file1 = logging.handlers.RotatingFileHandler(os.path.join(logpath, logname), backupCount = 1000)
    file1.setFormatter(f2)
    file1.setLevel(logging.DEBUG)
    file1.doRollover()
    logging.root.addHandler(file1)
except:
    logging.exception("Error starting log file")
    logging.warning("Will log only to stdout")

logging.info('logging started')
