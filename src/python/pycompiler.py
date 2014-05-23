#!/usr/bin/env python
# -*- coding: utf-8 -*-
# -*- mode: python -*-
import sys


def parse( cfile ):
    fh = open( cfile )
    content = fh.read()
    fh.close()
    print content
    if "ERROR" in content or "FAILED" in content:
        sys.exit(1)
    sys.exit(0)

if __name__ == '__main__':
    parse( sys.argv[1] )
