import json
from scanner import get_lexer
from parser import get_parser
from interpreter import interpreter
import sys

def main():
    print("Initiating Parsing")

    # Build the lexer and parser
    lexer = get_lexer()
    parser = get_parser()

    try:
        # Read the file
        file_path = "Program_Test.txt"
        if len(sys.argv) > 1:
            file_path = sys.argv[1]
        
        with open(file_path, 'r') as textFile:
            data = textFile.read()

        # Parse the file
        ast = parser.parse(data, lexer=lexer)
        
        # Printing AST
        print("\nAbstract Syntax Tree:\n")
        print(json.dumps(ast, indent=4)) 
        # In testing screenshots, removed indent argument for formatting the images

        # Interpreter
        try:
            result = interpreter(ast)
            print("\nEvaluation result: ", result)
        
        except Exception as e:
            print(f"\nExecution error: {type(e).__name__}: {str(e)}")

    except FileNotFoundError:
        print(f"File '{file_path}' not found.")
    except Exception as e:
        print (f"Error: {type(e).__name__}: {str(e)}")

    print("\nFinalizing Parsing")

if __name__ == '__main__':
  main()