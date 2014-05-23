#!/usr/bin/env python
# -*- coding: utf-8 -*-
# -*- mode: python -*-

import glob
from setuptools import setup

#IMPORTANT: The version of the build is defined in the version file
#           "src/dbc_python/_version.py".  In this way the same
#           version number is used to build the package and is also
#           available inside the package, or when the code is used
#           directly at the commandline
import re
VERSIONFILE="src/dbc_python/_version.py"
verstrline = open( VERSIONFILE, "rt" ).read()
VSRE = r"^__version__ = ['\"]([^'\"]*)['\"]"
mo = re.search( VSRE, verstrline, re.M )
if mo:
    version_string = mo.group(1)
else:
    raise RuntimeError("Unable to find version string in %s." % ( VERSIONFILE ) )


setup( name = "dbc-python",
       version = version_string, ### The Version of this build is defined in the file (VERSIONFILE) "src/dbc_python/_version.py"
       url = "http://dbc.dk",
       maintainer = "DBC",
       maintainer_email = "http://bugs.dbc.dk/",
       description = "Base library for python development at DBC.",
       long_description = "Base Library for python development at DBC\n" +
                          "Provides functions to do basic tasks",
       package_dir={'': 'src'},
       packages = ['dbc_python',
                   'dbc_python.net',
                   'dbc_python.tests',
                   'dbc_python.tests.net',
                   'dbc_python.tests.utils',
                   'dbc_python.utils'],
       data_files = [( 'dbc_python/xml', ['xml/junit.xsd'] ),
                     ( 'dbc_python/tests/data', glob.glob( 'src/dbc_python/tests/data/*' ) ),
                     ],
       test_suite = "dbc_python.tests",
       provides = ['dbc_python'],
       zip_safe=False )
