import ply.lex as lex
import ply.yacc as yacc


# BEGIN LEXICAL ANALYZER DEFINITION
reserved = {
    'if': 'IF',
    'then': 'THEN',
    'else': 'ELSE',
    'let': 'LET',
    'val': 'VAL',
    'func': 'FUNC',
    'end': 'END',
    'in': 'IN',
    'nil': 'NIL',
    'true': 'TRUE',
    'false': 'FALSE',
    'exec': 'EXEC',
}

tokens = [
    'ID',
    'ID_FUNC',
    'NUMBER',
    'STRING',
    'LPAREN',
    'RPAREN',
    'LBRACE',
    'RBRACE',
    'COMMA',
    'ASSIGN',
    'EQUAL',
    'LESSTHAN',
    'GREATERTHAN',
    'PLUS',
    'MINUS',
    'TIMES',
    'DIVIDE',
    'DOT',
    'AND',
    'OR',
    'UMINUS',
] + list(reserved.values())

t_ignore = ' \t\r\n'

def t_COMMENT(t):
    r'//.*'
    pass

def t_ID(t):
    r'[a-z][a-zA-Z0-9_\']*'
    if t.value in reserved:
        t.type = reserved[t.value]
    return t

def t_ID_FUNC(t):
    r'[A-Z][a-zA-Z0-9_\']*'
    if t.value in reserved:
        t.type = reserved[t.value]
    return t

def t_NUMBER(t):
    r'[0-9]+'
    t.value = int(t.value)
    return t
def t_STRING(t):
    r'\"([^\\\n]|(\\.))*?\"'
    return t
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'\['
t_RBRACE = r'\]'
t_COMMA = r','
t_ASSIGN = r":="
t_EQUAL = r'='
t_LESSTHAN = r'<'
t_GREATERTHAN = r'>'
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_DOT = r'\.'
t_AND = r'&'
t_OR = r'\|'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print('Invalid Token: ', t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()
# END LEXICAL ANALYZER DEFINITION

###########################

# BEGIN PARSING DEFINITION
def p_global_facts(p):
    "global_facts : facts exec_line"

def p_facts_func_def(p):
    "facts : func_def facts"

def p_facts_assign(p):
    "facts : assign facts"
    
def p_facts_empty(p):
    "facts : "

def p_func_def(p):
    "func_def : FUNC ID_FUNC LBRACE params RBRACE ASSIGN stm END"

def p_params_ID_FUNC_params(p):
    "params : ID_FUNC COMMA params"

def p_params_ID_params(p):
    "params : ID COMMA params"
    
def p_params_ID_FUNC(p):
    "params : ID_FUNC"
    
def p_params_ID(p):
    "params : ID"

def p_assign(p):
    "assign : VAL ID ASSIGN stm END"

def p_stm_call(p):
    "stm : ID_FUNC LBRACE args RBRACE"

def p_args_ID_FUNC_args(p):
    "args : ID_FUNC COMMA args"

def p_args_stm_args(p):
    "args : stm COMMA args"
    
def p_args_ID_FUNC(p):
    "args : ID_FUNC"

def p_args_arg(p):
    "args : stm"

def p_stm_PLUS(p):
    "stm : stm PLUS stm"
    
def p_stm_MINUS(p):
    "stm : stm MINUS stm"
    
def p_stm_TIMES(p):
    "stm : stm TIMES stm"

def p_stm_DIVIDE(p):
    "stm : stm DIVIDE stm"
    
def p_stm_DOT(p):
    "stm : stm DOT stm"
    
def p_stm_LESSTHAN(p):
    "stm : stm LESSTHAN stm"
    
def p_stm_GREATERTHAN(p):
    "stm : stm GREATERTHAN stm"

def p_stm_EQUAL(p):
    "stm : stm EQUAL stm"

def p_stm_AND(p):
    "stm : stm AND stm"

def p_stm_OR(p):
    "stm : stm OR stm"

def p_stm_STRING(p):
    "stm : STRING"

def p_stm_NUMBER(p):
    "stm : NUMBER"

def p_stm_TRUE(p):
    "stm : TRUE"

def p_stm_FALSE(p):
    "stm : FALSE"

def p_stm_NIL(p):
    "stm : NIL"

def p_stm_ID(p):
    "stm : ID"

def p_stm_PAREN(p):
    "stm : LPAREN stm RPAREN"

def p_stm_IF_THEN(p):
    "stm : IF stm THEN stm ELSE stm END"

def p_stm_LET_IN(p):
    "stm : LET facts IN stm END"

def p_exec_line(p):
    "exec_line : EXEC stm"

def p_unary_minus(p):
    "stm : MINUS stm %prec UMINUS"

def p_error(p):
    print(p)
    if p:
        print(f"Syntax error in input: {p.lineno}")
    else:
        print("Syntax error in input: none.")

precedence = (
    ('left', 'OR'),
    ('left', 'AND'),
    ('nonassoc', 'EQUAL', 'LESSTHAN', 'GREATERTHAN'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS')
)
# END PARSING DEFINITION


# CALL PARSING 

def main():
  print("Initiating Parsing")

  # Build the lexer and parser
  lexer = lex.lex()
  parser = yacc.yacc()

  # Read the file
  textFile = open('Program_Test.txt', 'r')
  data = textFile.read()

  # Parse the file
  parser.parse(data, lexer=lexer)
  
  print("Finalizing Parsing")

if __name__ == '__main__':
  main()

