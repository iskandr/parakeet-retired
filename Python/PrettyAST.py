"""Python AST pretty-printer.

This module exports a function that can be used to print a human-readable
version of the AST.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import sys
#from compiler.ast import Node
from ast import AST
import ast as Ast

__all__ = ('printAst',)


def printAst(ast, indent='  ', stream=sys.stdout, initlevel=0):
    "Pretty-print an AST to the given output stream."
    rec_node(ast, initlevel, indent, stream.write)
    stream.write('\n')
    stream.flush()

def rec_node(node, level, indent, write):
    "Recurse through a node, pretty-printing it."
    pfx = indent * level
    if isinstance(node, AST):
        write(pfx)
        write(node.__class__.__name__)
        write('(')
        for i,(name,child) in enumerate(Ast.iter_fields(node)):
#            print "NAME:",name,"CHILD:",child
            if i != 0:
                write(',')
            if type(child) == type([]):
                for element in child:
                    write('\n')
                    rec_node(element,level+1,indent,write)
            elif isinstance(child,AST):
                write('\n')
                rec_node(child, level+1, indent, write)
            else:
                write(repr(child))
#        write('\n')
#        write(pfx)
        
        write(')')

    else:
        write(pfx)
        write(repr(node))


