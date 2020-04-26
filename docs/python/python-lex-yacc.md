# Python Lex-Yacc


PLY is a pure-Python implementation of the popular compiler construction tools lex and yacc.



## Getting Started with PLY


To install PLY on your machine for python2/3, follow the steps outlined below:

1. Download the source code from [here](http://www.dabeaz.com/ply/ply-3.10.tar.gz).
1. Unzip the downloaded zip file
1. Navigate into the unzipped `ply-3.10` folder
1. Run the following command in your terminal: `python setup.py install`

If you completed all the above, you should now be able to use the PLY module. You can test it out by opening a python interpreter and typing `import ply.lex`.

Note: Do **not** use `pip` to install PLY, it will install a broken distribution on your machine.



## The "Hello, World!" of PLY - A Simple Calculator


Let's demonstrate the power of PLY with a simple example: this program will take an arithmetic expression as a string input, and attempt to solve it.

Open up your favourite editor and copy the following code:

```
from ply import lex
import ply.yacc as yacc

tokens = (
    'PLUS',
    'MINUS',
    'TIMES',
    'DIV',
    'LPAREN',
    'RPAREN',
    'NUMBER',
)

t_ignore = ' \t'

t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIV     = r'/'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'

def t_NUMBER( t ) :
    r'[0-9]+'
    t.value = int( t.value )
    return t

def t_newline( t ):
  r'\n+'
  t.lexer.lineno += len( t.value )

def t_error( t ):
  print(=Invalid Token:=,t.value[0])
  t.lexer.skip( 1 )

lexer = lex.lex()

precedence = (
    ( 'left', 'PLUS', 'MINUS' ),
    ( 'left', 'TIMES', 'DIV' ),
    ( 'nonassoc', 'UMINUS' )
)

def p_add( p ) :
    'expr : expr PLUS expr'
    p[0] = p[1] + p[3]

def p_sub( p ) :
    'expr : expr MINUS expr'
    p[0] = p[1] - p[3]

def p_expr2uminus( p ) :
    'expr : MINUS expr %prec UMINUS'
    p[0] = - p[2]

def p_mult_div( p ) :
    '''expr : expr TIMES expr
            | expr DIV expr'''

    if p[2] == '*' :
        p[0] = p[1] * p[3]
    else :
        if p[3] == 0 :
            print(=Can't divide by 0=)
            raise ZeroDivisionError('integer division by 0')
        p[0] = p[1] / p[3]

def p_expr2NUM( p ) :
    'expr : NUMBER'
    p[0] = p[1]

def p_parens( p ) :
    'expr : LPAREN expr RPAREN'
    p[0] = p[2]

def p_error( p ):
    print(=Syntax error in input!=)

parser = yacc.yacc()

res = parser.parse(=-4*-(3-5)=) # the input
print(res)

```

Save this file as `calc.py` and run it.

Output:

```
-8

```

Which is the right answer for `-4 * - (3 - 5)`.



## Part 1: Tokenizing Input with Lex


There are two steps that the code from example 1 carried out: one was **tokenizing** the input, which means it looked for symbols that constitute the arithmetic expression, and the second step was **parsing**, which involves analysing the extracted tokens and evaluating the result.

This section provides a simple example of how to **tokenize** user input, and then breaks it down line by line.

```
    import ply.lex as lex

    # List of token names. This is always required
    tokens = [
       'NUMBER',
       'PLUS',
       'MINUS',
       'TIMES',
       'DIVIDE',
       'LPAREN',
       'RPAREN',
    ]

    # Regular expression rules for simple tokens
    t_PLUS    = r'\+'
    t_MINUS   = r'-'
    t_TIMES   = r'\*'
    t_DIVIDE  = r'/'
    t_LPAREN  = r'\('
    t_RPAREN  = r'\)'

    # A regular expression rule with some action code
    def t_NUMBER(t):
        r'\d+'
        t.value = int(t.value)    
        return t

    # Define a rule so we can track line numbers
    def t_newline(t):
        r'\n+'
        t.lexer.lineno += len(t.value)

    # A string containing ignored characters (spaces and tabs)
    t_ignore  = ' \t'

    # Error handling rule
    def t_error(t):
        print(=Illegal character '%s'= % t.value[0])
        t.lexer.skip(1)

    # Build the lexer
    lexer = lex.lex()

    # Give the lexer some input
    lexer.input(data)

    # Tokenize
    while True:
        tok = lexer.token()
        if not tok: 
            break      # No more input
        print(tok)

```

Save this file as `calclex.py`. We'll be using this when building our Yacc parser.

### Breakdown

<li>
Import the module using `import ply.lex`
</li>
<li>
All lexers must provide a list called `tokens` that defines all of the possible token names that can be produced by the lexer. This list is always required.
<pre><code> tokens = [
    'NUMBER',
    'PLUS',
    'MINUS',
    'TIMES',
    'DIVIDE',
    'LPAREN',
    'RPAREN',
 ]
</code></pre>
</li>

`tokens` could also be a tuple of strings (rather than a string), where each string denotes a token as before.

<li>
The regex rule for each string may be defined either as a string or as a function. In either case, the variable name should be prefixed by t_ to denote it is a rule for matching tokens.
<ul>
<li>
For simple tokens, the regular expression can be specified as strings: `t_PLUS = r'\+'`
</li>
<li>
If some kind of action needs to be performed, a token rule can be specified as a function.
<pre><code>   def t_NUMBER(t):
       r'\d+'
       t.value = int(t.value)
       return t
</code></pre>
Note, the rule is specified as a doc string within the function. The function accepts one argument which is an instance of `LexToken`, performs some action and then returns back the argument.
If you want to use an external string as the regex rule for the function instead of specifying a doc string, consider the following example:
<pre><code>   @TOKEN(identifier)         # identifier is a string holding the regex
   def t_ID(t):
       ...      # actions
</code></pre>
</li>
<li>
An instance of `LexToken` object (let's call this object `t`) has the following attributes:
<ol>
1. `t.type` which is the token type (as a string) (eg: `'NUMBER'`, `'PLUS'`, etc). By default, `t.type` is set to the name following the `t_` prefix.
1. `t.value` which is the lexeme (the actual text matched)
1. `t.lineno` which is the current line number (this is not automatically updated, as the lexer knows nothing of line numbers). Update lineno using a function called `t_newline`.

<li>
For simple tokens, the regular expression can be specified as strings: `t_PLUS = r'\+'`
</li>
<li>
If some kind of action needs to be performed, a token rule can be specified as a function.
<pre><code>   def t_NUMBER(t):
       r'\d+'
       t.value = int(t.value)
       return t
</code></pre>
Note, the rule is specified as a doc string within the function. The function accepts one argument which is an instance of `LexToken`, performs some action and then returns back the argument.
If you want to use an external string as the regex rule for the function instead of specifying a doc string, consider the following example:
<pre><code>   @TOKEN(identifier)         # identifier is a string holding the regex
   def t_ID(t):
       ...      # actions
</code></pre>
</li>
<li>
An instance of `LexToken` object (let's call this object `t`) has the following attributes:
<ol>
- `t.type` which is the token type (as a string) (eg: `'NUMBER'`, `'PLUS'`, etc). By default, `t.type` is set to the name following the `t_` prefix.
- `t.value` which is the lexeme (the actual text matched)
- `t.lineno` which is the current line number (this is not automatically updated, as the lexer knows nothing of line numbers). Update lineno using a function called `t_newline`.
</ol>
<h3></h3>
<pre><code>  def t_newline(t):
      r'\n+'
      t.lexer.lineno += len(t.value)
</code></pre>
<h3></h3>
<ol start="4">
- `t.lexpos` which is the position of the token relative to the beginning of the input text.
</ol>
</li>

### 

<li>
If nothing is returned from a regex rule function, the token is discarded. If you want to discard a token, you can alternatively add t_ignore_ prefix to a regex rule variable instead of defining a function for the same rule.
<pre><code>   def t_COMMENT(t):
       r'\#.*'
       pass
       # No return value. Token discarded
</code></pre>
...Is the same as:
<pre><code>   t_ignore_COMMENT = r'\#.*'
</code></pre>
<h3></h3>
<sup>This is of course invalid if you're carrying out some action when you see a comment. In which case, use a function to define the regex rule.</sup>
If you haven't defined a token for some characters but still want to ignore it, use `t_ignore = =<characters to ignore>=` (these prefixes are necessary):
<pre><code>   t_ignore_COMMENT = r'\#.*'
   t_ignore  = ' \t'    # ignores spaces and tabs
</code></pre>
<h3></h3>
</li>
<li>
When building the master regex, lex will add the regexes specified in the file as follows:
<ol>
- Tokens defined by functions are added in the same order as they appear in the file.
- Tokens defined by strings are added in decreasing order of the string length of the string defining the regex for that token.
</ol>
If you are matching `==` and `=` in the same file, take advantage of these rules.
</li>

### 

### 

<li>
<p>Literals are tokens that are returned as they are. Both `t.type` and `t.value` will be set to the character itself.
Define a list of literals as such:</p>
<pre><code>literals = [ '+', '-', '*', '/' ]
</code></pre>
or,
<pre><code>literals = =+-*/=
</code></pre>
<h3></h3>
It is possible to write token functions that perform additional actions when literals are matched. However, you'll need to set the token type appropriately. For example:
<pre><code>literals = [ '{', '}' ]

def t_lbrace(t):
    r'\{'
    t.type = '{'  # Set token type to the expected literal (ABSOLUTE MUST if this is a literal)
    return t
</code></pre>
</li>

### 

<li>
Handle errors with t_error function.
<pre><code># Error handling rule
def t_error(t):
    print(=Illegal character '%s'= % t.value[0])
    t.lexer.skip(1) # skip the illegal token (don't process it)
</code></pre>
In general, `t.lexer.skip(n)` skips n characters in the input string.
</li>

Final preparations:

Build the lexer using `lexer = lex.lex()`.

### 

You can also put everything inside a class and call use instance of the class to define the lexer. Eg:

### 

```
 import ply.lex as lex  
 class MyLexer(object):            
       ...     # everything relating to token rules and error handling comes here as usual 

       # Build the lexer
       def build(self, **kwargs):
           self.lexer = lex.lex(module=self, **kwargs)

       def test(self, data):
           self.lexer.input(data)
           for token in self.lexer.token():
               print(token)

       # Build the lexer and try it out

 m = MyLexer()
 m.build()           # Build the lexer
 m.test(=3 + 4=)     #

```

Provide input using `lexer.input(data)` where data is a string

To get the tokens, use `lexer.token()` which returns tokens matched. You can iterate over lexer in a loop as in:

### 

```
for i in lexer: 
    print(i)

```



## Part 2: Parsing Tokenized Input with Yacc


This section explains how the tokenized input from Part 1 is processed - it is done using Context Free Grammars (CFGs). The grammar must be specified, and the tokens are processed according to the grammar. Under the hood, the parser uses an LALR parser.

```
# Yacc example

import ply.yacc as yacc

# Get the token map from the lexer. This is required.
from calclex import tokens

def p_expression_plus(p):
    'expression : expression PLUS term'
    p[0] = p[1] + p[3]

def p_expression_minus(p):
    'expression : expression MINUS term'
    p[0] = p[1] - p[3]

def p_expression_term(p):
    'expression : term'
    p[0] = p[1]

def p_term_times(p):
    'term : term TIMES factor'
    p[0] = p[1] * p[3]

def p_term_div(p):
    'term : term DIVIDE factor'
    p[0] = p[1] / p[3]

def p_term_factor(p):
    'term : factor'
    p[0] = p[1]

def p_factor_num(p):
    'factor : NUMBER'
    p[0] = p[1]

def p_factor_expr(p):
    'factor : LPAREN expression RPAREN'
    p[0] = p[2]

# Error rule for syntax errors
def p_error(p):
    print(=Syntax error in input!=)

# Build the parser
parser = yacc.yacc()

while True:
   try:
       s = raw_input('calc > ')
   except EOFError:
       break
   if not s: continue
   result = parser.parse(s)
   print(result)

```

### Breakdown

<li>
Each grammar rule is defined by a function where the docstring to that function contains the appropriate context-free grammar specification. The statements that make up the function body implement the semantic actions of the rule. Each function accepts a single argument p that is a sequence containing the values of each grammar symbol in the corresponding rule. The values of `p[i]` are mapped to grammar symbols as shown here:
<pre><code>  def p_expression_plus(p):
      'expression : expression PLUS term'
      #   ^            ^        ^    ^
      #  p[0]         p[1]     p[2] p[3]

      p[0] = p[1] + p[3]
</code></pre>
</li>

<li>
For tokens, the =value= of the corresponding `p[i]` is the same as the `p.value` attribute assigned in the lexer module. So, `PLUS` will have the value `+`.
</li>
<li>
<p>For non-terminals, the value is determined by whatever is placed in `p[0]`. If nothing is placed, the value is None.
Also, `p[-1]` is not the same as `p[3]`, since `p` is not a simple list (`p[-1]` can specify embedded actions (not discussed here)).</p>
</li>

Note that the function can have any name, as long as it is preceeded by `p_`.

<li>
The `p_error(p)` rule is defined to catch syntax errors (same as `yyerror` in yacc/bison).
</li>
<li>
Multiple grammar rules can be combined into a single function, which is a good idea if productions have a similar structure.
<pre><code>  def p_binary_operators(p):
      '''expression : expression PLUS term
                    | expression MINUS term
         term       : term TIMES factor
                    | term DIVIDE factor'''
      if p[2] == '+':
          p[0] = p[1] + p[3]
      elif p[2] == '-':
          p[0] = p[1] - p[3]
      elif p[2] == '*':
          p[0] = p[1] * p[3]
      elif p[2] == '/':
          p[0] = p[1] / p[3] 
</code></pre>
</li>
<li>
Character literals can be used instead of tokens.
<pre><code>  def p_binary_operators(p):
      '''expression : expression '+' term
                    | expression '-' term
         term       : term '*' factor
                    | term '/' factor'''
      if p[2] == '+':
          p[0] = p[1] + p[3]
      elif p[2] == '-':
          p[0] = p[1] - p[3]
      elif p[2] == '*':
          p[0] = p[1] * p[3]
      elif p[2] == '/':
          p[0] = p[1] / p[3]
</code></pre>
Of course, the literals must be specified in the lexer module.
</li>
<li>
Empty productions have the form `'''symbol : '''`
</li>
<li>
To explicitly set the start symbol, use `start = 'foo'`, where `foo` is some non-terminal.
</li>
<li>
Setting precedence and associativity can be done using the precedence variable.
<h3></h3>
<pre><code>  precedence = (
      ('nonassoc', 'LESSTHAN', 'GREATERTHAN'),  # Nonassociative operators
      ('left', 'PLUS', 'MINUS'),
      ('left', 'TIMES', 'DIVIDE'),
      ('right', 'UMINUS'),            # Unary minus operator
  )
</code></pre>
Tokens are ordered from lowest to highest precedence. `nonassoc` means that those tokens do not associate. This means that something like `a < b < c` is illegal whereas `a < b` is still legal.
</li>
<li>
`parser.out` is a debugging file that is created when the yacc program is executed for the first time. Whenever a shift/reduce conflict occurs, the parser always shifts.
</li>



#### Remarks


Additional links:

1. [Official docs](http://www.dabeaz.com/ply/)
1. [Github](https://github.com/dabeaz/ply)

