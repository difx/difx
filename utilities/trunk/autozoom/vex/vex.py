# Copyright (C) 2012, 2013 Mark Kettenis
# Copyright (C) 2012, 2013 Joint Institute for VLBI in Europe
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

"""

This module provides a simple VEX parser.

"""

import ply.lex as lex
import ply.yacc as yacc

from MultiDict import MultiDict

reserved = {
    'def': 'DEF',
    'enddef': 'ENDDEF',
    'scan': 'SCAN',
    'endscan': 'ENDSCAN',
    'ref': 'REF',
}
   
# List of token names.
tokens = [
    'COLON',
    'SEMICOLON',
    'EQ',
    'DOLLAR',
    'IDENT',
] + list(reserved.values())

# Regular expressions rules for simple tokens
t_COLON     = r':'
t_SEMICOLON = r';'
t_EQ        = r'='
t_DOLLAR    = r'\$'

def t_STRING(t):
    r'\"(\\.|[^\\"])*\"'
    t.value = t.value[1:-1]
    t.type = 'IDENT'
    return t

def t_IDENT(t):
    r'[^&$\*:;=\t\n\r ]+'
    t.type = reserved.get(t.value, 'IDENT')
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    return

def t_error(t):
    print "Illegal character '%s'" % t.value[0]
    t.lexer.skip(1)
    return

t_ignore_COMMENT = r'\*.*'
t_ignore = ' \t&'

lexer = lex.lex()

def p_vex(t):
    'vex : vex_header blocks'
    t[0] = t[2]
    return

def p_vex_header(t):
    'vex_header : IDENT EQ IDENT SEMICOLON'
    t[0] = MultiDict()
    t[0][t[1]] = t[3]
    return

def p_blocks(t):
    '''blocks : block
              | blocks block'''
    t[0] = t[1]
    if len(t) > 2:
        t[0].update(t[2])
        pass
    return

def p_block(t):
    'block : block_header block_content'
    t[0] = MultiDict()
    t[0][str(t[1])] = t[2]
    return

def p_block_header(t):
    'block_header : DOLLAR IDENT SEMICOLON'
    t[0] = t[2]
    return

def p_block_content(t):
    '''block_content : block_lines
                     | block_content def_block
                     | block_content scan_block'''
    t[0] = t[1]
    if len(t) > 2:
        t[0].update(t[2])
        pass
    return

def p_def_block(t):
    'def_block : DEF word SEMICOLON block_lines ENDDEF SEMICOLON'
    t[0] = MultiDict()
    t[0][str(t[2])] = t[4]
    return

def p_scan_block(t):
    'scan_block : SCAN word SEMICOLON block_lines ENDSCAN SEMICOLON'
    t[0] = MultiDict()
    t[0][str(t[2])] = t[4]
    return

def p_block_lines(t):
    '''block_lines :
                   | block_lines block_line'''
    if len(t) == 1:
        t[0] = MultiDict()
    else:
        t[0] = t[1]
        t[0].update(t[2])
        pass
    return

def p_block_line(t):
    '''block_line : REF DOLLAR word EQ value SEMICOLON
                  | word EQ value SEMICOLON'''
    t[0] = MultiDict()
    if t[1] == "ref":
        t[0][str(t[3])] = t[5]
    else:
        t[0][str(t[1])] = t[3]
        pass
    return

def p_value(t):
    '''value : word
             | 
             | value COLON word
             | value COLON'''
    if len(t) == 1:
        t[0] = ""
    elif len(t) == 2:
        t[0] = t[1]
    elif len(t) == 3:
        t[0] = t[1]
    else:
        try:
            t[0] = t[1]
            t[0].append(t[3])
        except:
            t[0] = [t[1], t[3]]
            pass
        pass
    return

def p_word(t):
    '''word : IDENT
            | word IDENT'''
    if len(t) == 2:
        t[0] = t[1]
    else:
        t[0] = str(t[1]) + ' ' + str(t[2])
        pass
    return

def p_error(t):
    raise SyntaxError, "at line %d, token %s" % (t.lineno, t.value)

parser = yacc.yacc(debug=0)

def parse(s):
    return parser.parse(s, lexer=lexer)

def Vex(file):
    fp = open(file, 'r')
    vex = fp.read()
    fp.close()
    return parse(vex)

if __name__ == "__main__":
    import sys
    print Vex(sys.argv[1])
