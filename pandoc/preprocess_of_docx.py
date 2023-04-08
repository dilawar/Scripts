#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Replace \gls to their value in TeX file. It read the \loadglsentries files from
Tex File to find the replacement. 

Replace \SI and \si etc as well.

First encounter is replaced by large form, rest are replaced by short form.

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import re

def find_glossaries( tex, dirname ):
    pat = re.compile( r'\\loadglsentries\{\s*(?P<fname>.+?)\}' )
    return [ os.path.join( dirname, x) for x in pat.findall( tex )] 

def read_glossary( f, gls ):
    if not os.path.isfile( f ):
        f = '%s.tex' % f

    # If glossary file is not found, don't do anything.
    if not os.path.exists(f):
        return 

    glpat = re.compile( r'\\newacronym\{\s*(?P<gls>.+?)\}' + \
            r'\s*\{(?P<short>.+?)\}\s*\{(?P<long>.+?)\}', re.DOTALL )

    with open( f, 'r' ) as h:
        glsText = h.read( )
        for m in glpat.findall( glsText ):
            gls[ m[0] ] = m[1:]

def _replace_function( m, tex, prevI, newTex ):
    a, b = m.span( )
    newTex += tex[prevI:a]
    return b, newTex

def replace_glossaries( tex, gls ):
    prevI, newTex, firstTime = 0, '', []
    glsPat = re.compile( r'\\gls\{\s*(?P<id>.+?)\}' )
    newTex, b = '', 0
    for m in glsPat.finditer( tex ):
        a, b = m.span( )
        prevI, newTex = _replace_function( m, tex, prevI, newTex )
        # Now find replacement.
        mm = '%s' % m.group(1)
        try:
            if mm not in firstTime:
                firstTime.append( mm )
                replaceWith = gls[ mm ][1] + ' (**%s**)' % gls[mm][0]
            else:
                replaceWith = gls[ mm ][0] 
            newTex += replaceWith
        except Exception as e:
            newTex += mm
    return newTex + tex[b:]

def unit_to_tex( unit ):
    replaceDict = { 
            'meter' : 'm', 'nanometer' : 'nm', 'kilometer' : 'km'
            , 'sec' : 'sec', 'second' : 's', 'hour' : 'h', 'minute' : 'min'
            , 'mole' : 'M', 'kilogram' : 'kg'
            }

    prefix = dict( per = '^-1^', square = '^2^', cubic = '^3^' )
    suffix = dict( squared = '^2^', cubed = '^3^' )
    unit = [ x.strip() for x in unit.split( '\\' ) if x.strip( ) ]

    newUnit = [ ]
    for i, u in enumerate( unit ):
        if u in replaceDict:
            # Now search for prefixes and suffixes.
            newU = replaceDict[u]
            for ii in range(i+1, len(unit) ):
                if unit[ii] in suffix:
                    newU += suffix[ unit[ii] ]
                    unit[ii] = ''
                else:
                    break
            for ii in range( i-1, -1, -1):
                if unit[ii] in prefix:
                    if unit[ii] == 'per':
                        if re.search( r'\^\d+\^', newU ):
                            newU = re.sub( r'\^(\d+)\^', r'^-\1^', newU )
                        else:
                            newU += '^-1^'
                    else:
                        newU += prefix[ unit[ii] ]
                    unit[ii] = ''
                else:
                    break
            newUnit.append( newU )
    return '{0}'.format( ''.join( newUnit ))


def si_replacement( val, unit ):
    if 'e' in val.lower( ):
        val = val.split( 'e' )
        val = r'%s×10^%s^' % (val[0],val[1])
    unit = unit_to_tex( unit )
    return '%s %s' % (val, unit)

def replace_si( text ):
    # yet to be implemented.
    pat = re.compile( r'\\SI\{(.+?)\}\{(.+?)}', re.DOTALL )
    siWithVals = pat.finditer( text )
    prevI, newText = 0, ''
    for m in siWithVals:
        a, b = m.span( )
        val, units = m.group(1), m.group(2)
        newText += text[prevI:a]
        replaceWith = si_replacement( val, units )
        #  print( '[INFO] %s %s %s' % (val,units,replaceWith.strip()))
        newText += replaceWith 
        prevI = b
    return newText + text[b:]

def read_glossaries( gls_files ):
    glossaries = { }
    for f in gls_files:
        read_glossary( f, glossaries )
    return glossaries

def replace( infile ):
    dirname = os.path.dirname( os.path.realpath(infile.name) )
    tex = infile.read( )
    glsFiles = find_glossaries( tex, dirname )
    gls = read_glossaries( glsFiles )
    text = replace_glossaries( tex, gls )
    text = replace_si( text )
    print( text, file = sys.stdout )

def main( args ):
    replace( args.INPUT )

if __name__ == '__main__':
    import argparse
    # Argument parser.
    description = '''Replace glossaries/acronyms (\gls) in TeX/Pandoc docs.'''
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('INPUT', nargs = '?'
            , type = argparse.FileType('r'), default = sys.stdin
            , help = 'Input TeX/pandoc file or stdin'
        )
    class Args: pass 
    args = Args()
    parser.parse_args(namespace=args)
    main( args )
