# INTERPRETER DEFINITION

# Recursively evaluate the AST
def evaluate(node, env):
    if not node:
        return None
    
    # Get the type of the current node (dictionary)
    node_type = node.get("type")

    # Primitive types
    if node_type == "stm_value":
        return node["value"]
    
    # Variable
    elif node_type == "stm_id":
        var_name = node["id"]
        if var_name in env:
            fact = env[var_name]

            # If it is a variable definition
            if fact["type"] == "val":
                return evaluate(fact["stm"], env)
            return fact
        else:
            raise NameError("f:Undefined variable: {var_name}")
    
    # Operations
    elif node_type == "stm_op":
        op = node["op"]

        # Unary minus
        if op == "uminus":
            value = evaluate(node["value"], env)
            # Type checking
            if not isinstance(value, (int, float)):
                raise TypeError(f"Unary minus requires a number of integer or float type. Type is: {type(value).__name__}.")
            
            return -1 * value
        
        # Values for binary operations
        left_val = evaluate(node["value1"], env)
        right_val = evaluate(node["value2"], env)

        # Arithmetic operations
        if op == "+":
            if not (isinstance(left_val, (int, float)) and isinstance(right_val, (int, float))):
                raise TypeError(f"'+' requires both values to be numbers of type float or integer, types are: {type(left_val).__name__} and {type(right_val).__name__}.")
            return left_val + right_val
            
        elif op == "-":
            if not (isinstance(left_val, (int, float)) and isinstance(right_val, (int, float))):
                raise TypeError(f"'-' requires both values to be numbers of type float or integer, types are: {type(left_val).__name__} and {type(right_val).__name__}.")
            return left_val - right_val
            
        elif op == "*":
            if not (isinstance(left_val, (int, float)) and isinstance(right_val, (int, float))):
                raise TypeError(f"'*' requires both values to be numbers of type float or integer, types are: {type(left_val).__name__} and {type(right_val).__name__}.")
            return left_val * right_val
            
        elif op == "/":
            if not (isinstance(left_val, (int, float)) and isinstance(right_val, (int, float))):
                raise TypeError(f"'/' requires both values to be numbers of type float or integer, types are: {type(left_val).__name__} and {type(right_val).__name__}.")
            if right_val == 0:
                raise ZeroDivisionError("Division by zero")
            return left_val / right_val
        
        elif op == ".":
            # If they are integers, make a decimal
            if isinstance(left_val, int) and isinstance(right_val, int):
                val = str(left_val) + "." + str(right_val)
                return float(val)
            # If not, concatenate strings
            return str(left_val) + str(right_val)
        
        # Comparisons operations
        elif op == "<":
            if type(left_val) != type(right_val):
                raise TypeError(f"'<' requires both values to be of the same type, types are: {type(left_val).__name__} and {type(right_val).__name__}.")
            return left_val < right_val
        
        elif op == ">":
            if type(left_val) != type(right_val):
                raise TypeError(f"'>' requires both values to be of the same type, types are: {type(left_val).__name__} and {type(right_val).__name__}.")
            return left_val > right_val
        
        elif op == "=":
            if type(left_val) != type(right_val):
                raise TypeError(f"'=' requires both values to be of the same type, types are: {type(left_val).__name__} and {type(right_val).__name__}.")
            return left_val == right_val
        
        # Logic operations
        elif op == "&":
            if not (isinstance(left_val, bool) and isinstance(right_val, bool)):
                raise TypeError(f"'&' requires both values to be of boolean type, types are: {type(left_val).__name__} and {type(right_val).__name__}.")
            return left_val and right_val
        
        elif op == "|":
            if not (isinstance(left_val, bool) and isinstance(right_val, bool)):
                raise TypeError(f"'|' requires both values to be of boolean type, types are: {type(left_val).__name__} and {type(right_val).__name__}.")
            return left_val or right_val
        
        else:
            raise NameError(f"{op} is not recognized.")
        
    # Function call
    elif node_type == "stm_func_call":
        func_name = node["id_func"]
        if func_name not in env:
            raise NameError(f"Undefined function: {func_name}.")
        
        func_def = env[func_name]
        if func_def["type"] != "func":
            raise TypeError(f"{func_name} is not a function, it is of type: {func_def['type']}.")
        
        # Argument values
        arg_values = [evaluate(arg, env) for arg in node["args"]]

        # New environment for scope of function
        call_env = env.copy()
        params = func_def["params"]

        # Check argument count
        if len(params) != len(arg_values):
            raise TypeError(f"Function {func_name} expects {len(params)} arguments, received {len(arg_values)}")
        
        # Pair parameters and arguments
        for i, param in enumerate(params):
            if "id" in param:
                param_name = param["id"]
                call_env[param_name] = {"type": "val", "name": param_name, "stm": {"type": "stm_value", "value": arg_values[i]}}
            
            elif "id_func" in param:
                param_name = param["id_func"]
                call_env[param_name] = {"type": "val", "name": param_name, "stm": {"type": "stm_value", "value": arg_values[i]}}
    
        # Evaluate function body in new environment
        return evaluate(func_def["stm"], call_env)
    
    # If-then-else blocks
    elif node_type == "stm_if":
        condition = evaluate(node["condition"], env)

        # Type check
        if not isinstance(condition, bool):
            raise TypeError(f"'if' condition must be of type bool, type is: {type(condition).__name__}.")
        
        # Branch selection
        if condition:
            return evaluate(node["then"], env)
        else:
            return evaluate(node["else"], env)
        
    # Let-in expression
    elif node_type == "stm_let":
        # New environment for local scope
        local_env = env.copy()

        # Definitions in let
        facts = node["facts"]
        for name, fact in facts.items():
            local_env[name] = fact

        # Recursive evaluation
        return evaluate(node["stm"], local_env)
    
    # Unknown node type
    else:
        raise ValueError(f"Unknown node type: {node_type}.")

def interpreter(ast):
    # if null imput
    if not ast:
        return None

    # Definitions
    env = {}
    if "facts" in ast:
        for name, fact in ast["facts"].items():
            env[name] = fact

    # Execute the ast
    if "stm" in ast:
        return evaluate(ast["stm"], env)
    
    return None