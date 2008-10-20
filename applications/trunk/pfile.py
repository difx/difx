"""
Functions for printing to files

TODO allow to search of multiple occurances of a single parameter is a single go
TODO add an add_parameter function
FIXME parameter CANNOT contain the same parameter twice
"""

from sys import stdout
import os

#functions which write to stdout

def print_parameter(parameter, value, outfile = stdout, just = 20):
    """
    Print parameter and value to an open file object.

    (by default stdout)
    """
    print >> outfile, (parameter + ':').ljust(just) + str(value)

def get_parameter(parameter, infile, occurrence = 0):
    """
    Returns the value of a single parameter in the input file as a string.

        parameter is the name of the parameter.
        infile is the file to read
        occurence means take the nth occurence (zero indexed!)

    parameter also accepts lists/tuples as inputs.

    If n is set then it must have a single value or the same length as 
    parameter

    KNOWN BUGS:
    parameter CANNOT contain the same parameter twice
    """
    if isinstance(parameter, str):
        parameter = [parameter]
    else:
        parameter = list(parameter)
    n = len(parameter)

    if isinstance(occurrence, int):
        nvalue = occurrence
        occurrence = [nvalue for i in range(n)]

    found = [0 for i in range(n)]
    total_found = 0
    values = [None for i in range(n)]

    f = open(infile, 'r')
    for line in f:
        if line[0] == '#':
            continue

        split = line.split(':')
        if split[0] in parameter:
            i = parameter.index(split[0])
            if found[i] == occurrence[i]:
                values[i] = split[1].strip()
                total_found += 1
                if total_found == n:
                    break
            found[i] += 1
    f.close()

    if not total_found == n:
        raise RuntimeError, "Couldn't find all values (" + \
                            str(total_found) + ' / ' + str(n)
    if n == 1:
        return values[0]
    else:
        return values

def set_parameter(parameter, value, infile, occurrence = 0, just = 20):
    """
    Changes the value of a single parameter in the input file.

        parameter is the name of the parameter.
        value is its new value
        infile is the file to change
        occurence means take the nth occurence (zero indexed!)
        just is the width in characters before the start of value.

    value must have the same length as parameter
    occurence must have lenth 1 or the same as parameter

    KNOWN BUGS:
    parameter CANNOT contain the same parameter twice
    """
    if isinstance(parameter, str):
        parameter = [parameter]
    else:
        parameter = list(parameter)
    if isinstance(value, str):
        value = value,

    n = len(parameter)
    if not len(value) == n:
        raise RuntimeError, "parameter and value must be the same length"

    if isinstance(occurrence, int):
        nvalue = occurrence
        occurrence = [nvalue for i in range(n)]

    found = [0 for i in range(n)]
    total_found = 0

    f = open(infile, 'r')
    output_file = infile + '~'
    f1 = open(output_file, 'w')

    for line in f:
        if line[0] == '#':
            f1.write(line)
            continue

        split = line.split(':')
        if split[0] in parameter:
            i = parameter.index(split[0])
            if found[i] == occurrence[i]:
                newline = (parameter[i] + ':').ljust(just) + value[i]
                f1.write(newline + '\n')
                total_found += 1
            else:
                f1.write(line)
            found[i] += 1
        else:
            f1.write(line)

    if not total_found == n:
        raise RuntimeError, "Couldn't find all values"

    f.close()
    f1.close()
    os.remove(infile)
    os.rename(output_file, infile)

def del_parameter(parameter, infile, occurrence = 0):
    """
    Deletes the value of a single parameter in the input file.

        parameter is the name of the parameter.
        infile is the file to read
        occurence means take the nth occurence (zero indexed!)

    parameter also accepts lists/tuples as inputs.

    If n is set then it must have a single value or the same length as 
    parameter

    KNOWN BUGS:
    parameter CANNOT contain the same parameter twice
    """
    if isinstance(parameter, str):
        parameter = [parameter]
    else:
        parameter = list(parameter)
    n = len(parameter)

    if isinstance(occurrence, int):
        nvalue = occurrence
        occurrence = [nvalue for i in range(n)]

    found = [0 for i in range(n)]
    total_found = 0

    f = open(infile, 'r')
    output_file = infile + '~'
    f1 = open(output_file, 'w')

    for line in f:
        if line[0] == '#':
            f1.write(line)
            continue
        split = line.split(':')
        if split[0] in parameter:
            i = parameter.index(split[0])
            if found[i] == occurrence[i]:
                total_found += 1
            else:
                f1.write(line)
            found[i] += 1
        else:
            f1.write(line)
    f.close()
    f1.close()

    if not total_found == n:
        raise RuntimeError, "Couldn't find all values"
    os.remove(infile)
    os.rename(output_file, infile)
