# Abstract syntax tree



## Analyze functions in a python script


This analyzes a python script and, for each defined function, reports the line number where the function began, where the signature ends, where the docstring ends, and where the function definition ends.

```
#!/usr/local/bin/python3

import ast
import sys

=== The data we collect.  Each key is a function name; each value is a dict
with keys: firstline, sigend, docend, and lastline and values of line numbers
where that happens. ===
functions = {}

def process(functions):
    === Handle the function data stored in functions. ===
    for funcname,data in functions.items():
        print(=function:=,funcname)
        print(=\tstarts at line:=,data['firstline'])
        print(=\tsignature ends at line:=,data['sigend'])
        if ( data['sigend'] < data['docend'] ):
            print(=\tdocstring ends at line:=,data['docend'])
        else:
            print(=\tno docstring=)
        print(=\tfunction ends at line:=,data['lastline'])
        print()

class FuncLister(ast.NodeVisitor):
    def visit_FunctionDef(self, node):
        === Recursively visit all functions, determining where each function
        starts, where its signature ends, where the docstring ends, and where
        the function ends. ===
        functions[node.name] = {'firstline':node.lineno}
        sigend = max(node.lineno,lastline(node.args))
        functions[node.name]['sigend'] = sigend
        docstring = ast.get_docstring(node)
        docstringlength = len(docstring.split('\n')) if docstring else -1
        functions[node.name]['docend'] = sigend+docstringlength
        functions[node.name]['lastline'] = lastline(node)
        self.generic_visit(node)

def lastline(node):
    === Recursively find the last line of a node ===
    return max( [ node.lineno if hasattr(node,'lineno') else -1 , ]
                +[lastline(child) for child in ast.iter_child_nodes(node)] )

def readin(pythonfilename):
    === Read the file name and store the function data into functions. ===
    with open(pythonfilename) as f:
        code = f.read()
    FuncLister().visit(ast.parse(code))

def analyze(file,process):
    === Read the file and process the function data. ===
    readin(file)
    process(functions)

if __name__ == '__main__':
    if len(sys.argv)>1:
        for file in sys.argv[1:]:
            analyze(file,process)
    else:
        analyze(sys.argv[0],process)

```

