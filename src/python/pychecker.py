#!/usr/bin/env python
# -*- coding: utf-8 -*-
# -*- mode: python -*-
"""
Pychecker
=========

Run python sourcefiles through pep8 and pyflakes, formats
error/warnings so they conform to emacs flymake-mode.

A format options using uatopep8 is also provided.
"""
import subprocess
import re


def main():
    """ Main method for pychecker """
    from optparse import OptionParser

    usage = "Output warnings and errors from pep8 and pyflakes run over file(s)"
    parser = OptionParser(usage="%prog [options] file ...\n" + usage)

    parser.add_option("-o", "--out", type="string", action="store", dest="out", help="Store output in file")
    parser.add_option("-f", "--format", action="store_true", dest="format",
                      help="Formats input files with autopep instead of reporting errors/warnings")

    (options, args) = parser.parse_args()

    if not args:
        parser.error("Need at least one file to check")

    for path in args:
        if options.format:
            output(options.out, format_source(path))
        else:
            output(options.out, check_source(path))


def format_source(sourcefile, ignore=["E221", "E501"]):
    """ Formats source using autopep8
        By default these error types are ignored:

        *. "E221" - multiple spaces before operator
        *. "E501" - Line to long
    """

    out, err, status = run_script("autopep8 %s" % " ".join(map(lambda x: "--ignore %s" % x, ignore)), sourcefile)
    if status != 0:
        raise RuntimeError("Error During formatting: %s" % err)
    return out


def check_source(sourcefile):
    """ Checks sourcefile with pep8 and pyflakes, and creates report """
    entrys = run_pyflakes(sourcefile) + run_pep8(sourcefile)
    entrys.sort(lambda x, y: int(x[-1]) - int(y[-1]))
    return '\n'.join(map(lambda x: "%s %s%s %s at %s line %s." % tuple(x), entrys))


def run_pep8(sourcefile, ignore=["E221", "E501"]):
    """ Runs pep8 on sourcefile and returns report
        By default these error types are ignored:

        *. "E221" - multiple spaces before operator
        *. "E501" - Line to long
    """
    pep_re = re.compile('^([^:]+):(\d+):(\d+):\s*([WECR])(\d+)\s*(.*)')
    out, err, status = run_script('pep8', sourcefile)

    report = []
    for line in map(str.strip, out.split('\n')):
        match = pep_re.match(line)
        if match:
            filename, linenum, row, errtype, errnum, description = match.groups()
            if not errtype + errnum in ignore:
                report.append(['Warning', errtype, errnum, description, filename, linenum])
    return report


def run_pyflakes(sourcefile):
    """ Runs pyflakes on sourcefile and returns report """
    out, err, status = run_script('pyflakes', sourcefile)

    report = map(lambda x: map(lambda y: y.strip(), x.split(':')),
                 filter(lambda x: x != '', out.split('\n')))

    formatted_report = []
    for entry in report:
        formatted_report.append(['Error', 'E', '001', entry[2], entry[0], entry[1]])

    return formatted_report


def run_script(script, argument):
    cmd = "%s %s" % (script, argument)
    proc = subprocess.Popen(cmd, shell=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
    (stdout, stderr) = proc.communicate()
    return (stdout, stderr, proc.returncode)


def output(target, string):
    if target:
        with open(target, 'w') as fh:
            fh.write(string + '\n')
    else:
        print string

if __name__ == '__main__':
    main()
