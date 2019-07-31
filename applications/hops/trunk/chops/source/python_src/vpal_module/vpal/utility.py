"""utility function module"""
#core imports
from __future__ import print_function
from __future__ import division
from builtins import zip
from builtins import str
from builtins import object
from past.utils import old_div
from builtins import range
import sys

#non-core imports
import numpy


def limit_periodic_quantity_to_range(value_to_limit, low_value=-180.0, high_value=180.0):
    """clamp periodic variable to range [low_value,high_value)"""
    high = high_value
    low = low_value
    if high_value < low_value:
        high = low_value
        low = high_value

    dm = divmod(value_to_limit + high, high - low)
    return low + dm[1]

def minimum_angular_difference(angle1, angle2, low_value=-180.0, high_value=180.0):
    """compute smallest value of (angle1-angle2) taking branch cuts into account"""
    high = high_value
    low = low_value
    if high_value < low_value:
        high = low_value
        low = high_value

    #first put a and b in the expected branch:
    aprime = limit_periodic_quantity_to_range(angle1, low, high)
    bprime = limit_periodic_quantity_to_range(angle2, low, high)

    ret_val = aprime - bprime
    if ret_val > old_div(abs(high - low),2.0):
        ret_val = abs(high - low) - ret_val

    return ret_val

def time_to_int(year, day, hour, minute, sec):
    """ported from time_to_int.c in hops/sub/util"""
    if year == 0:
        year80 = 0
    else:
        year80 = year % 100 - 80

    if year80 < 0:
        year80 += 100

    nleaps = old_div((year80 + 3),4)
    secs_since_80 = year80*31536000 + (day+nleaps-1)*86400 + hour*3600 + minute*60 + sec
    return secs_since_80

class DiscreteQuantityFilter(object):
    """representation of a filter which passes objects that have
    a named quantity present in a discrete list.
    filter may be inverted to pass the opposite selection"""

    def __init__(self, quantity_name, quantity_value_list, invert_selection=False):
        self.quantity_name = quantity_name
        self.quantity_value_list = quantity_value_list
        self.invert = invert_selection

    def does_object_pass_filter(self, obj):
        "filter query function, returns True/False"
        ret_val = False
        test_val = getattr(obj, self.quantity_name)
        for q in self.quantity_value_list:
            if test_val == q:
                ret_val = True
                break
        if self.invert:
            ret_val = not ret_val
        return ret_val


class ContinuousQuantityFilter(object):
    """representation of a filter which passes objects that have
    a named quantity present in continuous value range
    (although this quantity may in fact take on discrete values).
    filter may be inverted to pass the opposite selection"""

    def __init__(self, quantity_name, lower_limit, upper_limit, invert_selection=False):
        self.quantity_name = quantity_name
        self.invert = invert_selection
        if lower_limit <= upper_limit:
            self.lower_limit = lower_limit
            self.upper_limit = upper_limit
        else:
            self.lower_limit = upper_limit
            self.upper_limit = lower_limit

    def does_object_pass_filter(self, obj):
        "filter query function, returns True/False, limits are inclusive"
        ret_val = False
        test_val = getattr(obj, self.quantity_name)
        if self.lower_limit <= test_val and test_val <= self.upper_limit:
            ret_val = True
        if self.invert:
            ret_val = not ret_val
        return ret_val


def combined_filter(object_list, filter_list):
    """takes a list of objects, and a list of filters (may be a
    combination of discrete or continuous value filters)
    and returns a list of objects files which pass all of the filters"""

    filtered_objects = object_list
    for filt in filter_list:
        filtered_objects = [item for item in filtered_objects if filt.does_object_pass_filter(item)]
    return filtered_objects

def sort_objects_by_quantity(object_list, quantity_name, reverse_boolean=False):
    """#sorts low to high (if reverse is True, then high to low)"""
    object_list.sort(key=lambda x: getattr(x, quantity_name), reverse=reverse_boolean)

def collect_object_values(obj_list, value_name):
    """collect a value from a list of objects"""
    value_list = []
    for obj in obj_list:
        value_list.append(getattr(obj, value_name))
    return value_list

def collect_object_value_pairs(obj_list, value_name1, value_name2, sort_items=False):
    """collect set of value pairs from a list of objects
    (if sort_items=True, we sort them on the first value) """

    value_list1 = []
    value_list2 = []
    ret_value_list1 = []
    ret_value_list2 = []

    for obj in obj_list:
        value_list1.append(getattr(obj, value_name1))
        value_list2.append(getattr(obj, value_name2))

    if sort_items is True:
        zipped_items = list(zip(value_list1, value_list2))
        zipped_items.sort()
        ret_value_list1 = [x for (x, y) in zipped_items]
        ret_value_list2 = [y for (x, y) in zipped_items]
    else:
        ret_value_list1 = value_list1
        ret_value_list2 = value_list2

    return [ret_value_list1, ret_value_list2]

def compute_weighted_mean(value_list, weight_list):
    """return the weighted mean of a list of values"""
    assert len(value_list) == len(weight_list)
    numer = 0.0
    denom = 0.0
    for n in list(range(0, len(value_list))):
        x = value_list[n]
        w = weight_list[n]
        numer += x*w
        denom += abs(w) #weights had better be positive
    return old_div(numer,denom)

def mad(value_list):
    """computes the median absolute deviation of a list of numbers
    see: Anomaly Detection by Robust Statistics, P. Rousseeuw & M. Hubert"""
    med = numpy.median(value_list)
    diff = []
    for val in value_list:
        diff.append( abs(val - med) )
    med_diff = numpy.median(diff)
    scale_factor = 1.4826
    return scale_factor*med_diff

################################################################################
def compute_2d_pareto_front(obj_list, par1, par2, maximize1=True, maximize2=True):
    """returns a list of objects on the pareto front of par1 and par2
    if maximize1 or maximize2 is false, then the objective for that variable will
    be minimization rather than maximization"""

    #empty set of pareto points
    pareto_set = set()

    #sort the list along the 1st axis
    sort_objects_by_quantity(obj_list, par1, maximize1)

    #take the first object and add it to the pareto set
    pareto_set.add(obj_list[0])

    #slow O(N^2) search using simple cull method
    #TODO clean this up...
    if maximize1 is True and maximize2 is True:
        for n in list(range(1, len(obj_list))):
            x = obj_list[n]
            on_interior = False
            p1x = getattr(x, par1)
            p2x = getattr(x, par2)
            for y in pareto_set:
                #now determine if x is on the interior
                p1y = getattr(y, par1)
                p2y = getattr(y, par2)
                if p1x <= p1y and p2x <= p2y:
                    on_interior = True
                    break
            if on_interior is False:
                #now prune any points in the pareto front which
                #are interior to this new point
                for y in pareto_set.copy():
                    p1y = getattr(y, par1)
                    p2y = getattr(y, par2)
                    if p1y <= p1x and p2y <= p2x:
                        pareto_set.remove(y)
                #add the new point to the pareto front
                pareto_set.add(x)
    elif maximize1 is True and maximize2 is False:
        for n in list(range(1, len(obj_list))):
            x = obj_list[n]
            on_interior = False
            p1x = getattr(x, par1)
            p2x = getattr(x, par2)
            for y in pareto_set:
                #now determine if x is on the interior
                p1y = getattr(y, par1)
                p2y = getattr(y, par2)
                if p1x <= p1y and p2x >= p2y:
                    on_interior = True
                    break
            if on_interior is False:
                #now prune any points in the pareto front which
                #are interior to this new point
                for y in pareto_set.copy():
                    p1y = getattr(y, par1)
                    p2y = getattr(y, par2)
                    if p1y <= p1x and p2y >= p2x:
                        pareto_set.remove(y)
                #add the new point to the pareto front
                pareto_set.add(x)
    elif maximize1 is False and maximize2 is True:
        for n in list(range(1, len(obj_list))):
            x = obj_list[n]
            on_interior = False
            p1x = getattr(x, par1)
            p2x = getattr(x, par2)
            for y in pareto_set:
                #now determine if x is on the interior
                p1y = getattr(y, par1)
                p2y = getattr(y, par2)
                if p1x >= p1y and p2x <= p2y:
                    on_interior = True
                    break
            if on_interior is False:
                #now prune any points in the pareto front which
                #are interior to this new point
                for y in pareto_set.copy():
                    p1y = getattr(y, par1)
                    p2y = getattr(y, par2)
                    if p1y >= p1x and p2y <= p2x:
                        pareto_set.remove(y)
                #add the new point to the pareto front
                pareto_set.add(x)
    elif maximize1 is False and maximize2 is False:
        for n in list(range(1, len(obj_list))):
            x = obj_list[n]
            on_interior = False
            p1x = getattr(x, par1)
            p2x = getattr(x, par2)
            for y in pareto_set:
                #now determine if x is on the interior
                p1y = getattr(y, par1)
                p2y = getattr(y, par2)
                if p1x >= p1y and p2x >= p2y:
                    on_interior = True
                    break
            if on_interior is False:
                #now prune any points in the pareto front which
                #are interior to this new point
                for y in pareto_set.copy():
                    p1y = getattr(y, par1)
                    p2y = getattr(y, par2)
                    if p1y >= p1x and p2y >= p2x:
                        pareto_set.remove(y)
                #add the new point to the pareto front
                pareto_set.add(x)

    pareto_list = []
    for y in pareto_set:
        pareto_list.append(y)
    return pareto_list

################################################################################
def print_table(table, n_digits=9):
    """simple formatted print out of a table"""
    max_width = 0
    for row in table:
        for col in row:
            if isinstance(col, float):
                if n_digits > max_width:
                    max_width = n_digits
            else:
                if len(str(col)) > max_width:
                    max_width = len(str(col))
    max_width += 2;
    for row in table:
        row2 = []
        for x in row:
            if isinstance(x, float):
                row2.append(round(x, n_digits))
            else:
                row2.append(x)
        print("".join(str(word).ljust(max_width) for word in row2))

################################################################################
def tabulate(table, headers=None, n_digits=9, padding=3):
    """simple formatted chunk of text representing a table, replacement for tabulate module"""
    augmented_table = []
    n_rows = 0
    n_cols = 0
    if headers != None:
        header_length = len(headers)
        use_headers = True
        for row in table:
            n_rows += 1
            n_cols = max(n_cols, len(row))
            if n_cols != header_length:
                use_headers = False
        if use_headers is False:
            print("Error: header length does not match number of columns.")
        else:
            augmented_table.append(headers)
            n_rows += 1

    for row in table:
        augmented_table.append(row)


    #get the maximum width of each column
    max_column_widths = []
    for j in list(range(0, n_cols)):
        max_col_width = 0
        for i in list(range(0, n_rows)):
            if j < len(augmented_table[i]):
                elem = augmented_table[i][j]
                elem_len = len(str(elem))
                if isinstance(elem, float):
                    if elem_len > n_digits:
                        elem_len = n_digits
                if elem_len > max_col_width:
                    max_col_width = elem_len
        max_column_widths.append(max_col_width)

    lines = ''
    for row in augmented_table:
        row2 = []
        for j in list(range(0, len(row) ) ):
            x = row[j]
            if isinstance(x, float):
                row2.append(round(x, n_digits))
            else:
                row2.append( str(x) )
        row_string = "".join( str(row2[j]).ljust(max_column_widths[j] + padding) for j in list(range(0, len(row2))) )
        lines += row_string + "\n"

    return lines

################################################################################
class Bar(object):
    """Dumb spinning cursor progress.Bar-like class for when the progress module is unavailable"""

    #default values for init
    def __init__(self, status, max):
        self.cursor_chars = ['-', '/', '|', '\\']
        self.status = status
        self.total_n_ticks = max #yes, this is a poor variable name choice, but it has to match the progress.Bar implementation
        self.tick_count = 0
        self.start()

    def start(self):
        sys.stdout.write(self.status)
        self.tick_count = 0

    def next(self):
        sys.stdout.write(self.cursor_chars[self.tick_count%4] )
        sys.stdout.flush()
        sys.stdout.write('\b')
        self.tick_count += 1

    def finish(self):
        sys.stdout.write('\n')
        sys.stdout.flush()
        self.tick_count = 0
