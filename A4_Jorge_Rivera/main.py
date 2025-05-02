import json
from scanner import get_lexer
from parser import get_parser
from interpreter import interpreter

def main():
    print("Initiating Parsing")

    # Build the lexer and parser
    lexer = get_lexer()
    parser = get_parser()

    # Read the file
    textFile = open('Program_Test.txt', 'r')
    data = textFile.read()

    # Parse the file
    ast = parser.parse(data, lexer=lexer)
    
    # Printing AST
    print("\nAbstract Syntax Tree:\n")
    print(json.dumps(ast, indent=4))

    # Interpreter
    try:
        result = interpreter(ast)
        print("\nEvaluation result: ", result)
    
    except Exception as e:
        print(f"\nExecution error: {type(e).__name__}: {str(e)}")

    print("\nFinalizing Parsing")

if __name__ == '__main__':
  main()