#!/usr/bin/env python
# -*- coding: utf-8 -*-
# -*- mode: python -*-
from setuptools import setup

version_string="1.0.2"

setup( name = "example-project",
       version = version_string,
       url = "http://example.foo.bar",
       maintainer = "example-maintainer",
       maintainer_email = "http://bugs.example.foo.bar/",
       description = "Example project for elisp testing.",
       long_description = "Example project for elisp testing.",
       package_dir={'': 'src'},
       packages = ['example_project'],
       test_suite = "example.tests",
       provides = ['example_project'],
       zip_safe=False )
