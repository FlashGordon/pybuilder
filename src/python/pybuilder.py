#!/usr/bin/env python
# -*- coding: utf-8 -*-
# -*- mode: python -*-
import datetime
import os
import re
import select
import subprocess
import sys
from lxml import etree


COMPILE_FILENAME = ".compile"


class BasicPrintHandler( object ):

    def __init__( self ):
        pass

    def build_start( self ):
        print "building project"

    def build( self, setup_info, test_result ):

        out = "BUILD: %s-%s: %s - %s"%( setup_info['name'], setup_info['version'], test_result['mesg'][1], test_result['mesg'][0] )
        if 'coverage-result' in test_result:
            out += " - " + test_result['coverage-result'][2]
        print out

    def test_start( self ):
        print "building project"

    def test( self, setup_info, test_result ):
        print "TEST: %s-%s: %s - %s"%( setup_info['name'], setup_info['version'], test_result['mesg'][1], test_result['mesg'][0] )


class EmacsPrintHandler( object ):

    def __init__( self ):
        pass

    def build_start( self ):
        print "[emacs.build.build-start],building project"

    def build( self, setup_info, test_result ):

        out = [ setup_info['name'],
                setup_info['version'],
                test_result['num-of-tests'],
                str( test_result['status'] ),
                test_result['string-status'][0],
                str( test_result['time'] ) ]
        if 'coverage-result' in test_result:
            out.append( str( test_result['coverage-result'][1] ) )
        else:
            out.append( str( test_result['mesg'][1] ) )

        print "[emacs.build.build-result],%s"% ", ".join( out )

    def test_start( self ):
        pass

    def test( self, setup_info, test_result ):

        out = [ setup_info['name'],
                setup_info['version'],
                test_result['num-of-tests'],
                str( test_result['status'] ),
                test_result['string-status'][0],
                str( test_result['time'] ),
                str( test_result['mesg'][1] ) ]

        print "[emacs.test.build-result],%s"% ", ".join( out )


class Builder( object ):

    def __init__( self, setup_dir, print_handler ):

        self.setup_dir = setup_dir
        self.print_handler = print_handler
        self.setup_info = self._get_project_definition()
        self.builds = 0

        self.methods = { 'stat': self.stat, 'build': self.build, 'test': self.test }

    def stat( self ):
        print self.setup_info

    def build( self ):
        self.print_handler.build_start()
        self.builds += 1

        cmd = self._set_env( "python setup.py nosetests --with-coverage --cover-erase --cover-package=%s"%
                             self.setup_info['provides'] )

        status, out, err = self._run( cmd )

        test_result = self._analyse_test_result( out, err )

        if status == 0:
            self._coverage()
            test_result['coverage-result'] = self._analyse_coverage()

        self._write_result_to_file( test_result['test'], COMPILE_FILENAME )
        self.print_handler.build( self.setup_info, test_result )

    def _analyse_coverage( self ):
        parser = etree.XMLParser( remove_blank_text=True, encoding="UTF-8" )
        xml = etree.parse( os.path.join( self.setup_dir, "coverage.xml" ), parser )
        raw = float( xml.getroot().get( 'line-rate' ) )
        formatted = "%s"%'{:.2%}'.format( raw )
        string = "coverage: %s"%formatted
        return ( raw, formatted, string )

    def test( self, target ):
        self.print_handler.test_start()

        cmd = self._set_env( "python setup.py nosetests --tests %s"%target )

        status, out, err = self._run( cmd )
        test_result = self._analyse_test_result( out, err )

        self._write_result_to_file( out + err, COMPILE_FILENAME )
        self.print_handler.test( self.setup_info, test_result )

    ####################################################################################################
    def _get_project_definition( self ):

        cmds = { 'provides' : "python setup.py --provides",
                 'version' : "python setup.py --version",
                 'name'     : "python setup.py --name" }

        setup_info = dict()
        for name, cmd in cmds.iteritems():

            result = self._run( cmd )
            if result[0] != 0:
                raise RuntimeError( result[2].strip() )
            setup_info[name] = result[1].strip()

        return setup_info

    def _run( self, cmd ):
        """
        """
        cwd = os.getcwd()
        os.chdir( self.setup_dir )
        proc = subprocess.Popen( cmd, shell=True, stderr=subprocess.PIPE, stdout=subprocess.PIPE )
        retcode = proc.wait()
        ( stdout, stderr ) = proc.communicate()

        os.chdir( cwd )

        return ( retcode, stdout, stderr )

    def _set_env( self, cmd ):
        if 'pyenv' in os.listdir( self.setup_dir ):
            cmd = "( `./pyenv` && %s )"%cmd
        return cmd

    def _coverage( self ):
        cmd = "coverage xml --include=*/%s/*"%self.setup_info['provides']
        result = self._run( cmd )
        if result[0] != 0:
            raise RuntimeError( result[2].strip() )

    def _analyse_test_result( self, out, err ):

        result = out + err
        cstart = re.search( "Name\s+Stmts\s+Miss\s+Cover\s+Missing", result, re.M )
        rev_result = result[::-1]
        cend = re.search( "^-+$", rev_result, re.M )
        info = {}

        if cstart:
            info['test'] = result[:cstart.start()] + "\n" + result[-cend.end():]
            info['coverage'] = result[cstart.start():-cend.start()]
        else:
            info['test'] = result

        status_regex = re.compile( "(OK)|(FAILED)\s*\((\w*)=(\d*)" )
        info['mesg'] = filter(lambda x: x != '', result[-cend.start():].split( "\n" ) )

        match = re.search( status_regex, info['mesg'][1] )
        if match.group( 1 ):
            info['status'] = 0
            info['string-status'] = [ match.group( 1 ) ]
        else:
            if match.group( 3 ) == 'failures':
                info['status'] = 1
            else:
                info['status'] = 2
            info['string-status'] = [ match.group( 2 ), match.group( 3 ), int( match.group( 4 ) ) ]

        test_regex = re.compile( "Ran (\d*) tests* in ((?:\d|\.)*)s" )

        match = re.search( test_regex, info['mesg'][0] )
        info['num-of-tests'] = match.group( 1 )
        info['time'] = float( match.group( 2 ) )
        return info

    def _isolate_test_result( self, out, err ):
        result = out + err
        cstart = re.search( "Name\s+Stmts\s+Miss\s+Cover\s+Missing", result, re.M )
        rev_result = result[::-1]
        cend = re.search( "^-+$", rev_result, re.M )

        return result[:cstart.start()] + "\n" + result[-cend.end():]

    def _write_result_to_file( self, result, path ):
        fh = open( os.path.join( self.setup_dir, path ), 'w' )
        fh.write( result )
        fh.close()


class Server( object ):

    def __init__( self, history_size=100 ):

        self.history_size = history_size
        self.history_lst = []

        ### NOTE: PERSISTENT HISTORY
        self.history_file = os.path.abspath( "/home/shm/.pybuilder-history" )
        self._load_history()

        self.methods = { "echo": self.echo, "timer": self.timer, "hist": self.history, "help": self.help }
        self.commands = dict()
        self.commands.update( self.methods )

    def run( self, builder, build_interval ):

        self.build_interval = build_interval
        self.commands.update( builder.methods )

        running = True
        while running:
            now = datetime.datetime.now()
            builder.build()
            delta = datetime.datetime.now() - now
            diff = self.build_interval - delta.seconds
            if diff <= 0:
                diff = 1

            while diff > 0:

                i, o, e = select.select( [sys.stdin], [], [], diff )
                if len( i ) > 0:
                    inp = i[0].readline().strip()
                    command, args = self._parse_input( inp )

                    if command:
                        self._add_history_entry( inp )

                        if command == 'quit':
                            ### NOTE: PERSISTENT HISTORY
                            self._save_history()

                            running = False
                            break
                        elif command in self.commands:
                            self._run_with_try_catch( self.commands[command], args )
                        else:
                            print "Unknown command '%s'"%command

                delta = datetime.datetime.now() - now
                diff = self.build_interval - delta.seconds

    def _parse_input( self, input ):
        if input.strip() == "":
            return ( "", "" )
        args = []
        splt = input.split()
        command = splt[0]
        if len( splt ) > 1:
            args = splt[1:]

        return command, args

    def _run_with_try_catch( self, func, args ):
        try:
            func( *args )
        except Exception, e:
            print e

    def _add_history_entry( self, entry ):
        ignore_prefix = ['hist', 'quit']

        ignore_entry = any( map( lambda x: entry.startswith( x ), ignore_prefix ) )
        if not ignore_entry:
            while len( self.history_lst ) >= self.history_size:
                self.history_lst.pop( 0 )
            self.history_lst.append( entry )

    ### NOTE: PERSISTENT HISTORY
    def _load_history( self ):
        if os.path.exists( self.history_file ):
            fh = open( self.history_file )
            content = fh.read()
            fh.close()
            self.history_lst = eval( content )

    ### NOTE: PERSISTENT HISTORY
    def _save_history( self ):
        fh = open( self.history_file, 'w' )
        fh.write( str( self.history_lst ) )
        fh.close()

    def timer( self, new_time ):
        """
        Sets build interval for server.

        :param new_time:
            New build_interval
        :type new_time:
            int
        """
        self.build_interval = int( new_time )

    def echo( self, *str ):
        """ prints provided string.
        """
        print " ".join( str )

    def help( self, target=None ):
        """ prints available commands, or provide help for single command.
        """
        keys = self.commands.keys()
        if not target:
            print "Commands:"
            keys.sort()
            for key in keys:
                print "   " + key + ": ",
                docstring = self.commands[key].__doc__
                if docstring:
                    print docstring.split( "\n" )[0]
                else:
                    print ""

        elif target in keys:
            help( self.commands[target] )

        else:
            print "No help on entry %s"%target

    def history( self, size=10 ):
        for i, entry in enumerate( self.history_lst[::-1] ):
            print "%s: %s"%( i + 1, entry )
            if i + 1 >= int( size ):
                break


if __name__ == '__main__':

    from optparse import OptionParser
    available_print_handlers = { "basic": BasicPrintHandler,
                                 "emacs": EmacsPrintHandler }

    usage="Runs Pybuilder testbuilder on python project."

    parser = OptionParser( usage="%prog [options] project_dir\n" + usage )

    parser.add_option("-p", "--print-handler", type="string", action="store", dest="print_handler",
                      default="basic",
                      help="Printhandler to use when printing build/test results. default is basic. available print handlers: %s"%str( available_print_handlers.keys() ) )

    parser.add_option("-i", "--interval", type="int", action="store", dest="interval",
                      default="100",
                      help="Automatic build interval in seconds. Default is 100" )

    parser.add_option("-t", "--tests", type="string", action="store", dest="tests",
                      help="run specific test or tests" )

    ( options, args ) = parser.parse_args()

    if not options.print_handler in available_print_handlers.keys():
        parser.error( "Unknown print handler '%s'"%options.print_handler )

    project_dir = '.'
    if len( args ) > 0:
        project_dir = args[0]

    if not os.path.exists( os.path.join( os.path.abspath( project_dir ), 'setup.py' ) ):
        parser.error( "Could not find project file (setup.py) in folder '%s'"%project_dir )

    print_handler = available_print_handlers[options.print_handler]
    builder = Builder( project_dir, print_handler() )

    if options.tests:
        builder.test( options.tests)
    else:
        server = Server()
        server.run( builder, options.interval )
