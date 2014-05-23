#!/usr/bin/env python
# -*- coding: utf-8 -*-
# -*- mode: python -*-

import os
import setuptools
from mock import Mock


def parse_setup_file( setup_file ):

    fh = open( setup_file )
    content = fh.read()
    fh.close()

    setup_file = os.path.abspath( setup_file )
    setup_dir = os.path.dirname( setup_file )

    org_dir = os.path.abspath( '.' )
    os.chdir( setup_dir )

    setuptools.setup = Mock()
    exec(content)

    os.chdir( org_dir )

    modded_values = {}

    key_funcs = { 'name': ( lambda x: x, None ),
                  'version': ( lambda x: x, None ),
                  'test_suite': ( lambda x: x.replace( '.', '/' ), 'test-suite' ),
                  'provides': ( lambda x: x[0], None ),
                  'package_dir': ( lambda x: x[x.keys()[0]], 'package-dir' ) }

    for key, value in setuptools.setup.call_args[1].iteritems():
        if key in key_funcs:
            name = key_funcs[key][1]
            if name == None:
                name = key
            modded_values[name] = key_funcs[key][0]( value )

    modded_values['setup-file'] = setup_file
    modded_values['setup-dir'] = setup_dir
    modded_values['package-dir'] = os.path.join( modded_values['setup-dir'], modded_values['package-dir'] )
    modded_values['src-dir'] = os.path.join( modded_values['package-dir'], modded_values['provides'] )
    modded_values['test-suite'] = os.path.join( modded_values['package-dir'], modded_values['test-suite'] )

    return modded_values


if __name__ == '__main__':

    from optparse import OptionParser

    usage="returns all info from setupfile in a dict."

    parser = OptionParser( usage="%prog [options] setup-file\n" + usage )


    ( options, args ) = parser.parse_args()

    if len(args ) < 1:
        parser.error( "Please supply setup.py file" )

    result = None
    status = "OK"
    try:
        raw_values = parse_setup_file( args[0] )
        result = ""
        for key, value in raw_values.iteritems():
            result += key + "=" + str( value ) + "\n"

    except Exception, e:
        status = "ERROR"
        result = e

    print "%s:"% status
    print result
