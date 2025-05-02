import ply.lex as lex

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

# Getter to refactor imports
def get_lexer():
    return lex.lex()