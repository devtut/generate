# Abstract syntax tree



## Analyze functions in a python script


This analyzes a python script and, for each defined function, reports the line number where the function began, where the signature ends, where the docstring ends, and where the function definition ends.

```
#!/usr/local/bin/python3

import ast
import sys

&quot;&quot;&quot; The data we collect.  Each key is a function name; each value is a dict
with keys: firstline, sigend, docend, and lastline and values of line numbers
where that happens. &quot;&quot;&quot;
functions = {}

def process(functions):
    &quot;&quot;&quot; Handle the function data stored in functions. &quot;&quot;&quot;
    for funcname,data in functions.items():
        print(&quot;function:&quot;,funcname)
        print(&quot;\tstarts at line:&quot;,data['firstline'])
        print(&quot;\tsignature ends at line:&quot;,data['sigend'])
        if ( data['sigend'] < data['docend'] ):
            print(&quot;\tdocstring ends at line:&quot;,data['docend'])
        else:
            print(&quot;\tno docstring&quot;)
        print(&quot;\tfunction ends at line:&quot;,data['lastline'])
        print()

class FuncLister(ast.NodeVisitor):
    def visit_FunctionDef(self, node):
        &quot;&quot;&quot; Recursively visit all functions, determining where each function
        starts, where its signature ends, where the docstring ends, and where
        the function ends. &quot;&quot;&quot;
        functions[node.name] = {'firstline':node.lineno}
        sigend = max(node.lineno,lastline(node.args))
        functions[node.name]['sigend'] = sigend
        docstring = ast.get_docstring(node)
        docstringlength = len(docstring.split('\n')) if docstring else -1
        functions[node.name]['docend'] = sigend+docstringlength
        functions[node.name]['lastline'] = lastline(node)
        self.generic_visit(node)

def lastline(node):
    &quot;&quot;&quot; Recursively find the last line of a node &quot;&quot;&quot;
    return max( [ node.lineno if hasattr(node,'lineno') else -1 , ]
                +[lastline(child) for child in ast.iter_child_nodes(node)] )

def readin(pythonfilename):
    &quot;&quot;&quot; Read the file name and store the function data into functions. &quot;&quot;&quot;
    with open(pythonfilename) as f:
        code = f.read()
    FuncLister().visit(ast.parse(code))

def analyze(file,process):
    &quot;&quot;&quot; Read the file and process the function data. &quot;&quot;&quot;
    readin(file)
    process(functions)

if __name__ == '__main__':
    if len(sys.argv)>1:
        for file in sys.argv[1:]:
            analyze(file,process)
    else:
        analyze(sys.argv[0],process)

```

