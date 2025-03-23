import ply.lex as lex
import ply.yacc as yacc
import json

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
    p[0] = {"facts": p[1], "stm": p[2]}

def p_facts_func_def(p):
    "facts : func_def facts"
    if p[2] is None:
        p[0] = {p[1]["name"]: p[1]}
    else:
        result = {p[1]["name"]: p[1]}
        result.update(p[2])
        p[0] = result

def p_facts_assign(p):
    "facts : assign facts"
    if p[2] is None:
        p[0] = {p[1]["name"]: p[1]}
    else:
        result = {p[1]["name"]: p[1]}
        result.update(p[2])
        p[0] = result
    
def p_facts_empty(p):
    "facts : "
    p[0] = {}

def p_func_def(p):
    "func_def : FUNC ID_FUNC LBRACE params RBRACE ASSIGN stm END"
    p[0] = {"type": "func", "name": p[2], "params": p[4], "stm": p[7]}

def p_params_ID_FUNC_params(p):
    "params : ID_FUNC COMMA params"
    p[0] = [{"type": "id_func", "id_func": p[1]}] + p[3]

def p_params_ID_params(p):
    "params : ID COMMA params"
    p[0] = [{"type": "id", "id": p[1]}] + p[3]
    
def p_params_ID_FUNC(p):
    "params : ID_FUNC"
    p[0] = [{"type": "id_func", "id_func": p[1]}]
    
def p_params_ID(p):
    "params : ID"
    p[0] = [{"type": "id", "id": p[1]}]

def p_assign(p):
    "assign : VAL ID ASSIGN stm END"
    p[0] = {"type": "val", "name": p[2], "stm": p[4]}

def p_stm_call(p):
    "stm : ID_FUNC LBRACE args RBRACE"
    p[0] = {"type": "stm_func_call", "id_func": p[1], "args": p[3]}

def p_args_ID_FUNC_args(p):
    "args : ID_FUNC COMMA args"
    p[0] = [{"type": "id_func", "id_func": p[1]}] + p[3]

def p_args_stm_args(p):
    "args : stm COMMA args"
    p[0] = [p[1]] + p[3]
    
def p_args_ID_FUNC(p):
    "args : ID_FUNC"
    p[0] = [{"type": "id_func", "id_func": p[1]}]

def p_args_arg(p):
    "args : stm"
    p[0] = [p[1]]

def p_stm_PLUS(p):
    "stm : stm PLUS stm"
    # p[0] = p[1] + p[3]
    p[0] = {"type": "stm_op", "op": '+', "value1": p[1], "value2": p[3]}
    
def p_stm_MINUS(p):
    "stm : stm MINUS stm"
    # p[0] = p[1] - p[3]
    p[0] = {"type": "stm_op", "op": '-', "value1": p[1], "value2": p[3]}
    
def p_stm_TIMES(p):
    "stm : stm TIMES stm"
    # p[0] = p[1] * p[3]
    p[0] = {"type": "stm_op", "op": '*', "value1": p[1], "value2": p[3]}

def p_stm_DIVIDE(p):
    "stm : stm DIVIDE stm"
    # p[0] = p[1] / p[3]
    p[0] = {"type": "stm_op", "op": '/', "value1": p[1], "value2": p[3]}
    
def p_stm_DOT(p):  
    "stm : stm DOT stm"
    p[0] = {"type": "stm_op", "op": '.', "value1": p[1], "value2": p[3]}
    
def p_stm_LESSTHAN(p):
    "stm : stm LESSTHAN stm"
    # p[0] = p[1] < p[3]
    p[0] = {"type": "stm_op", "op": '<', "value1": p[1], "value2": p[3]}
    
def p_stm_GREATERTHAN(p):
    "stm : stm GREATERTHAN stm"
    # p[0] = p[1] > p[3]
    p[0] = {"type": "stm_op", "op": '>', "value1": p[1], "value2": p[3]}

def p_stm_EQUAL(p):
    "stm : stm EQUAL stm"
    # p[0] = p[1] == p[3]
    p[0] = {"type": "stm_op", "op": '=', "value1": p[1], "value2": p[3]}

def p_stm_AND(p):
    "stm : stm AND stm"
    # p[0] = p[1] and p[3]
    p[0] = {"type": "stm_op", "op": '&', "value1": p[1], "value2": p[3]}

def p_stm_OR(p):
    "stm : stm OR stm"
    # p[0] = p[1] or p[3]
    p[0] = {"type": "stm_op", "op": '|', "value1": p[1], "value2": p[3]}

def p_stm_STRING(p):
    "stm : STRING"
    # p[0] = p[1]
    p[0] = {"type": "stm_value", "type_value": "string", "value": p[1]}

def p_stm_NUMBER(p):
    "stm : NUMBER"
    # p[0] = p[1]
    p[0] = {"type": "stm_value", "type_value": "number", "value": p[1]}

def p_stm_TRUE(p):
    "stm : TRUE"
    # p[0] = True
    p[0] = {"type": "stm_value", "type_value": "boolean", "value": True}

def p_stm_FALSE(p):
    "stm : FALSE"
    # p[0] = False
    p[0] = {"type": "stm_value", "type_value": "boolean", "value": False}

def p_stm_NIL(p):
    "stm : NIL"
    # p[0] = None
    p[0] = {"type": "stm_value", "type_value": "nil", "value": None}

def p_stm_ID(p):
    "stm : ID"
    # p[0] = p[1]
    p[0] = {"type": "stm_id", "id": p[1]}

def p_stm_PAREN(p):
    "stm : LPAREN stm RPAREN"
    p[0] = p[2]

def p_stm_IF_THEN(p):
    "stm : IF stm THEN stm ELSE stm END"
    p[0] = {"type": "stm_if", "condition": p[2], "then": p[4], "else": p[6]}

def p_stm_LET_IN(p):
    "stm : LET facts IN stm END"
    p[0] = {"type": "stm_let", "facts": p[2], "stm": p[4]}

def p_exec_line(p):
    "exec_line : EXEC stm"
    p[0] = p[2]

def p_unary_minus(p):
    "stm : MINUS stm %prec UMINUS"
    # p[0] = -p[2]
    p[0] = {"type": "stm_op", "op": 'uminus', "value": p[2]}

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
    ast = parser.parse(data, lexer=lexer)
    
    # Printing AST
    print("\nAbstract Syntax Tree:\n")
    print(json.dumps(ast, indent=2))

    print("\nFinalizing Parsing")

if __name__ == '__main__':
  main()