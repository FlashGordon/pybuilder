import unittest

from example_project.example import MyClass
from example_project.example_two import MyClass as MyClass2
from example_project.example import my_function


class TestExample( unittest.TestCase ):

    # def test_function( self ):
    #     self.assertEqual( "function", my_function() )

    def test_method( self ):

        cls = MyClass()
        self.assertEqual( "method", cls.my_method() )
