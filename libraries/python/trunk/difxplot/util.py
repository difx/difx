"""Hold smaller util classes/functions for difxPlot.py"""
from glob import glob
import parseDiFX

class InputDetail:
    """Stores the experiment information detailed by a .input file using parseDiFX"""

    numfreqs: int
    freqs: list
    numtelescopes: int
    telescopes: list
    numdatastreams: int
    datastreams: list
    numbaselines: int
    baselines: list
    freq_range: tuple
    input_file_base: str
    source_name: str
    
    def __init__(self, input_file_base):
        self.input_file_base = input_file_base
        input_file = input_file_base + ".input"
        (self.numfreqs, self.freqs) = parseDiFX.get_freqtable_info(input_file)
        (self.numtelescopes, self.telescopes) = parseDiFX.get_telescopetable_info(
            input_file
        )
        (self.numdatastreams, self.datastreams) = parseDiFX.get_datastreamtable_info(
            input_file
        )
        (self.numbaselines, self.baselines) = parseDiFX.get_baselinetable_info(
            input_file
        )
        freq_list = [f.freq for f in self.freqs]
        self.freq_range = (min(freq_list), max(freq_list))
        self.source_name = self.get_source_from_calc()
        
    def get_source_from_calc(self):
        """Find the source name from the .calc file"""
        src_name = ""
        with open(self.input_file_base+'.calc', 'rt') as calc_file:
            for line in calc_file:
                if "SOURCE 0 NAME" in line:
                    src_name = line.split(':')[1].strip()
        return src_name



def check_for_difx_file(file_base) -> str:
    """Tries to find a difx file given a file base (exp_xx)"""
    try:
        difx_file = glob(file_base + ".difx/DIFX*")[0]
        return difx_file
    except IndexError:
        return False
