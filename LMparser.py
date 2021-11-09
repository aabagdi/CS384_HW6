#
# CSCI 384 : parser.py
# 
# recursive-descent parser for lambda calculus expressions
#

import sys

# The recursive descent parser for the (disambiguated) grammar
#<def>  ::= <name> := <term>; | <def>;<def>
#<term> ::= fn <name> => <term>|(<term>)|<app>
#<app>  ::= <term><term>|<name>
#<name> ::= zero | fib | main
def parseDef(tokens):
    #
    #<def>  ::= <name> := <term>; | <def>;<def>
    #
    seq = ["Defs"]
    while tokens.next() != "eof":
        x = parseName(tokens)
        tokens.eat(":=")
        t = parseTerm(tokens)
        tokens.eat(';')
        seq.append(["LM",x,t])
    return seq 
def parseTerm(tokens):
    #
    #<term> ::= fn <name> => <term>
    #
    if tokens.next() == 'fn':
        tokens.eat('fn')
        x = parseName(tokens)
        tokens.eat('=>')
        t = parseTerm(tokens)
        return ["LM",x,t]
    #
    #<term> ::= (<term>)
    #
    if tokens.next() == '(':
        tokens.eat('(')
        t = parseTerm(tokens)
        tokens.eat(')')
        return t 
    #
    #<term> ::= <app>
    #
    else: return parseApp(tokens)

def parseApp(tokens):
    #<app>  ::= <term> <term>|<name>
    if tokens.nextIsName():
        t1 = parseName(tokens)
    else:
        t1=parseTerm(tokens)
    
    if not(tokens.nextIsName()):
        return t1
    else:
        t2 = parseName(tokens)
    return["AP",t1,t2]

def parseName(tokens):
    if tokens.nextIsName():
        x = tokens.eatName()
        return["VA",x]
    else: 
        e = "Expected a name"
        raise LexError(e)
        return ["e"]

def reportPlace(expn):
    return expn[-1]

#
# The supporting code and driver follows below here
#

def parse(entry):
    tks = TokenStream(entry)
    ast = parseDef(tks)
    if not tks.atEOF():
        raise SyntaxError("Some unconsumed tokens remain.")
    print()
    prettyprint(ast)
    print()

def loadAll(files):
    try:
        # Load definitions from the specified source files.
        for fname in files:
            print("[opening "+fname+"]")
            f = open(fname,"r")
            src = f.read()
            parse(src)
    except SyntaxError as e:
        print("Syntax error.")
        print(e.args[0])
        print("Bailing command-line loading.")
    except LexError as e:
        print("Bad token reached.")
        print(e.args[0])
        print("Bailing command-line loading.")
def LsttoStr(ast):
    if ast[0] == "VA":
        return "VA(\""+ast[1]+"\")"
    if ast[0] == "LM":
        return "LM("+LsttoStr(ast[1])+","+LsttoStr(ast[2])+")"
    if ast[0] == "AP":
         return "AP("+LsttoStr(ast[1])+","+LsttoStr(ast[2])+")"
def prettyprint(ast):
    count = 1
    for term in ast:
        if term == "Defs":
            print('let')
        else:
            if term[0]=="Def":
                print('val x'+str(count)+' = '+LsttoStr(term[1]))
                print('val t'+str(count)+' = '+LsttoStr(term[2]))

#
# Exceptions
#
# These define the exception raised by the parser.
#
class EvalError(Exception):
    pass

class LexError(Exception):
    pass

# 
# Keywords, primitives, unary operations, and binary operations.
#
# The code below defines several strings or string lists used by
# the lexical analyzer (housed as class TokenStream, below).
#

RESERVED = ['fn','=>',':=']

# Characters that separate expressions.
DELIMITERS = '();'

# Characters that might make up unary and binary operations.
OPERATORS = '=>:=' 


#
# LEXICAL ANALYSIS / TOKENIZER
#
# The code below converts ML source code text into a sequence 
# of tokens (a list of strings).  It does so by defining the
#
#    class TokenStream
#
# which describes the methods of an object that supports this
# lexical conversion.  The key method is "analyze" which provides
# the conversion.  It is the lexical analyzer for ML source code.
#
# The lexical analyzer works by CHOMP methods that processes the
# individual characters of the source code's string, packaging
# them into a list of token strings.
#
# The class also provides a series of methods that can be used
# to consume (or EAT) the tokens of the token stream.  These are
# used by the parser.
#


class TokenStream:

    def __init__(self,src,filename="STDIN"):
        """
        Builds a new TokenStream object from a source code string.
        """
        self.sourcename = filename
        self.source = src # The char sequence that gets 'chomped' by the lexical analyzer.
        self.tokens = []  # The list of tokens constructed by the lexical analyzer.
        self.extents = []     
        self.starts = []

        # Sets up and then runs the lexical analyzer.
        self.initIssue()
        self.analyze()
        self.tokens.append("eof")

    #
    # PARSING helper functions
    #

        
    def lexassert(self,c):
        if not c:
            self.raiseLex("Unrecognized character.")

    def raiseLex(self,msg):
        s = self.sourcename + " line "+str(self.line)+" column "+str(self.column)
        s += ": " + msg
        raise LexError(s)

    def atEOF(self):
        """
        Checks whether the TokenStream has been exhausted.
        """
        return self.tokens[0] == 'eof'
        
    def next(self):
        """
        Returns the unchomped token at the front of the stream of tokens.
        """
        return self.tokens[0]

    def advance(self):
        """ 
        Advances the token stream to the next token, giving back the
        one at the front.
        """
        tk = self.next()
        del self.tokens[0]
        del self.starts[0]
        return tk

    def report(self):
        """ 
        Helper function used to report the location of errors in the 
        source code.
        """
        print(self.starts)
        print(self.tokens)
        lnum = self.starts[0][0]
        cnum = self.starts[0][1]
        return self.sourcename + " line "+str(lnum)+" column "+str(cnum)

    def eat(self,tk):
        """
        Eats a specified token, making sure that it is the next token
        in the stream.
        """
        if tk == self.next():
            return self.advance()
        elif self.atEOF():
            err1 = "EOF reached. "
            err2 = "Expected: '"+tk+"'. "
            raise SyntaxError(err1 + err2)
        else:
            where = self.report()
            err1 = "Unexpected token at "+where+". "
            err2 = "Saw: '"+self.next()+"'. "
            err3 = "Expected: '"+tk+"'. "
            raise SyntaxError(err1 + err2 + err3)

    def eatInt(self):
        """
        Eats an integer literal token, making sure that such a token is next
        in the stream.
        """
        if self.nextIsInt():
            tk = self.advance()
            if tk[0] == '-':
                return -int(tk[1:])
            else:
                return int(tk)
        elif self.atEOF():
            err1 = "EOF reached. "
            err2 = "Expected an integer literal. "
            raise SyntaxError(err1 + err2)
        else:
            where = self.report()
            err1 = "Unexpected token at "+where+". "
            err2 = "Saw: '"+self.next()+"'. "
            err3 = "Expected an integer literal. "
            raise SyntaxError(err1 + err2 + err3)

    def eatName(self):
        """
        Eats a name token, making sure that such a token is next in the stream.        """
        if self.nextIsName():
            return self.advance()
        elif self.atEOF():
            err1 = "EOF reached. "
            err2 = "Expected a name. "
            raise SyntaxError(err1 + err2)
        else:
            where = self.report()
            err1 = "Unexpected token at "+where+". "
            err2 = "Saw: '"+self.next()+"'. "
            err3 = "Expected a name. "
            raise SyntaxError(err1 + err2 + err3)

    def eatString(self):
        """
        Eats a string literal token, making sure that such a token is next in the stream.
        """
        if self.nextIsString():
            return self.advance()[1:-1]
        elif self.atEOF():
            err1 = "EOF reached. "
            err2 = "Expected a string literal. "
            raise SyntaxError(err1 + err2)
        else:
            where = self.report()
            err1 = "Unexpected token at "+where+". "
            err2 = "Saw: '"+self.next()+"'. "
            err3 = "Expected a string literal. "
            raise SyntaxError(err1 + err2 + err3)

    def nextIsInt(self):
        """
        Checks if next token is an integer literal token.
        """
        tk = self.next()
        return tk.isdigit()

    def nextIsName(self):
        """
        Checks if next token is a name.
        """
        tk = self.next()
        isname = tk[0].isalpha() or tk[0] =='_'
        for c in tk[1:]:
            isname = isname and (c.isalnum() or c == '_')
        return isname and (tk not in RESERVED)

    def nextIsString(self):
        """
        Checks if next token is a string literal.
        """
        tk = self.next()
        return tk[0] == '"' and tk[-1] == '"'

    #
    # TOKENIZER helper functions
    #
    # These are used by the 'analysis' method defined below them.
    #
    # The parsing functions EAT the token stream, whereas
    # the lexcial analysis functions CHOMP the source text
    # and ISSUE the individual tokens that form the stream.
    #

    def initIssue(self):
        self.line = 1
        self.column = 1
        self.markIssue()

    def markIssue(self):
        self.mark = (self.line,self.column)

    def issue(self,token):
        self.tokens.append(token)
        self.starts.append(self.mark)
        self.markIssue()

    def nxt(self,lookahead=1):
        if len(self.source) == 0:
            return ''
        else:
            return self.source[lookahead-1]

    def chompSelector(self):
        self.lexassert(self.nxt() == '#' and self.nxt(2).isdigit())
        token = self.chompChar()
        token = '#'
        while self.nxt().isdigit():
            token += self.chompChar()
        self.issue(token)

    def chompWord(self):
        self.lexassert(self.nxt().isalpha() or self.nxt() == '_')
        token = self.chompChar()
        while self.nxt().isalnum() or self.nxt() == '_':
            token += self.chompChar()
        self.issue(token)
        
    def chompInt(self):
        ck = self.nxt().isdigit()
        self.lexassert(ck)
        token = ""
        token += self.chompChar()     # first digit
        while self.nxt().isdigit():
            token += self.chompChar() # remaining digits=
        self.issue(token)
        
    def chompString(self):
        self.lexassert(self.nxt() == '"')
        self.chompChar() # eat quote
        token = ""
        while self.nxt() != '' and self.nxt() != '"':
            if self.nxt() == '\\':
                self.chompChar()
                if self.nxt() == '\n':
                    self.chompWhitespace(True)
                elif self.nxt() == '\\':
                    token += self.chompChar()
                elif self.nxt() == 'n':
                    self.chompChar()
                    token += '\n'
                elif self.nxt() == 't':
                    self.chompChar()
                    token += '\t'
                elif self.nxt() == '"': 
                    self.chompChar()
                    token += '"'
                else:
                    self.raiseLex("Bad string escape character")
            elif self.nxt() == '\n':
                self.raiseLex("End of line encountered within string")
            elif self.nxt() == '\t':
                self.raiseLex("Tab encountered within string")
            else:
                token += self.chompChar()

        if self.nxt() == '':
            self.raiseLex("EOF encountered within string")
        else:
            self.chompChar() # eat endquote
            self.issue('"'+token+'"')

    def chompComment(self):
        self.lexassert(len(self.source)>1 and self.source[0:1] == '(*')
        self.chompChar() # eat (*
        self.chompChar() #
        while len(self.source) >= 2 and self.source[0:1] != '*)':        
            self.chomp()
        if len(self.source) < 2:
            self.raiseLex("EOF encountered within comment")
        else:
            self.chompChar() # eat *)
            self.chompChar() #     

    def chomp(self):
        if self.nxt() in "\n\t\r ":
            self.chompWhitespace()
        else:
            self.chompChar()

    def chompChar(self):
        self.lexassert(len(self.source) > 0)
        c = self.source[0]
        self.source = self.source[1:]
        self.column += 1
        return c

    def chompWhitespace(self,withinToken=False):
        self.lexassert(len(self.source) > 0)
        c = self.source[0]
        self.source = self.source[1:]
        if c == ' ':
            self.column += 1
        elif c == '\t':
            self.column += 4
        elif c == '\n':
            self.line += 1
            self.column = 1
        if not withinToken:
            self.markIssue()
        
    def chompOperator(self):
        token = ''
        while self.nxt() in OPERATORS:
            token += self.chompChar()
        self.issue(token)

    #
    # TOKENIZER
    #
    # This method defines the main loop of the
    # lexical analysis algorithm, one that converts
    # the source text into a list of token strings.

    def analyze(self):
        while self.source != '':
            # CHOMP a string literal
            if self.source[0] == '"':
                self.chompString()
            # CHOMP a comment
            elif self.source[0:1] == '(*':
                self.chompComment()
            # CHOMP whitespace
            elif self.source[0] in ' \t\n\r':
                self.chompWhitespace()
            # CHOMP an integer literal
            elif self.source[0].isdigit():
                self.chompInt()
            # CHOMP a single "delimiter" character
            elif self.source[0] in DELIMITERS:
                self.issue(self.chompChar())
            # CHOMP an operator               
            elif self.source[0] in OPERATORS:
                self.chompOperator()
            # CHOMP a reserved word or a name.
            else:
                self.chompWord()

#
#  usage: 
#    python3 parser.py <file 1> ... <file n>
#
#    - parses and prints the ASTs of several ML source files
#
#
#    python3 parser.py
#
#    - requests an expression, parses it, prints its AST
#
if len(sys.argv) > 1:
    loadAll(sys.argv[1:])
else:
    print("Enter an expression to parse: ",end='')
    parse(input())
