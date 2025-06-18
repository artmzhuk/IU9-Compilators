import abc
import enum
import parser_edsl.parser_edsl as pe
import sys
import re
import typing
from dataclasses import dataclass
from pprint import pprint

funcs = {}
funcs['LEN'] = True

def extract_data_type(expression):
    result_type = None
    if type(expression) is VariableExpression:
        result_type = expression.varname.type
    elif type(expression) is FunctionVariableExpression:
        result_type = expression.name.type
    elif type(expression) is BinaryOperationExpression or type(expression) is ConstantExpression:
        result_type = expression.type
    return result_type


class DataType(enum.Enum):
    Integer = 'INTEGER'
    Long = 'LONG'
    Float = 'FLOAT'
    Double = 'DOUBLE'
    String = 'STRING'
    Boolean = 'BOOLEAN'

# VarDef -> VARNAME Type
@dataclass
class VariableDefinition:
    name : str
    type : DataType = None

# Statement -> AssignStatement
# 	   | IfStatement
# 	   | IfElseStatement
# 	   | WhileStatement
# 	   | ForStatement
# 	   | DimStatement
class CodeStatement(abc.ABC):
    pass

# Function -> FUNCTION VarDef ( FunctionParams ) Statements END FUNCTION
# 	  | SUB VARNAME ( FunctionParams ) Statements END SUB
@dataclass
class FunctionDefinition(CodeStatement):
    FunctionName : str
    name_coord: pe.Position
    args : list[str]
    FunctionBlock : CodeStatement

    @pe.ExAction
    def create(attrs, coords, res_coord):
        name, args, block = attrs
        kwc, varc, lc, fpc,rc, stc, endc, endkwc = coords

        if name.name in funcs:
            raise DuplicateFunctionError(kwc, name.name)
        funcs[name.name] = []
        for arg in args:
            if type(arg) is FunctionVariableExpression:
                funcs[name.name].append(('array', extract_data_type(arg)))
            else:
                funcs[name.name].append(('var', extract_data_type(arg)))


        return FunctionDefinition(name, varc,args,block)


# Program ->  Functions
@dataclass
class ProgramStructure:
    function_defs : list[FunctionDefinition]
    statements : list[CodeStatement]

    def validate(self):
        pass
        # for func in self.function_defs:
        #     if (hasattr(func.FunctionName, 'name') and func.FunctionName.name in funcs):
        #         raise DuplicateFunctionError(func.name_coord, func.FunctionName.name)
        #     elif not hasattr(func.FunctionName, 'name') and func.FunctionName in funcs:
        #         raise DuplicateFunctionError(func.FunctionName, func.FunctionName)



# Expr -> ArithmExpr
#       | ArithmExpr > ArithmExpr
#       | ArithmExpr < ArithmExpr
#       | ArithmExpr >= ArithmExpr
#       | ArithmExpr <= ArithmExpr
#       | ArithmExpr == ArithmExpr
#       | ArithmExpr <> ArithmExpr
class Expression(abc.ABC):
    pass

# AssignStatement -> Factor = Expr
@dataclass
class AssignmentStatement(CodeStatement):
    variable : str
    var_coord: pe.Position
    expr : Expression

    @pe.ExAction
    def create(attrs, coords, res_coord):
        var, expr = attrs
        cvar, cass, cexpr = coords
        if type(expr) is ConstantExpression:
            if expr.type != DataType.String and extract_data_type(var) != DataType.String:
                expr.type = extract_data_type(var)

        tmp = AssignmentStatement(var, cass.start, expr)
        tmp.validate_types()
        return tmp

    def validate_types(self):
        if extract_data_type(self.variable) == DataType.Double and extract_data_type(self.expr) != DataType.String:
            return
        if extract_data_type(self.variable) == DataType.Float and extract_data_type(self.expr) != DataType.String and extract_data_type(self.expr) != DataType.Double:
            return
        if extract_data_type(self.variable) == DataType.Long and extract_data_type(self.expr) != DataType.String and extract_data_type(self.expr) != DataType.Double \
                and extract_data_type(self.expr) != DataType.Float:
            return
        if extract_data_type(self.variable) == DataType.Integer and extract_data_type(self.expr) != DataType.String and extract_data_type(self.expr) != DataType.Double \
                and extract_data_type(self.expr) != DataType.Float and extract_data_type(self.expr) != DataType.Long:
            return

        if extract_data_type(self.variable) == DataType.String and extract_data_type(self.expr) == DataType.String:
            return


        raise TypeMismatchError(self.var_coord, extract_data_type(self.variable), ':=', extract_data_type(self.expr))

# IfStatement -> IF Expr THEN Statements END IF
@dataclass
class ConditionalStatement(CodeStatement):
    condition : Expression
    then_branch : CodeStatement

# IfElseStatement -> IF Expr THEN Statements ELSE Statements END IF
@dataclass
class ConditionalElseStatement(ConditionalStatement):
    else_branch : CodeStatement

# WhileStatement -> DO WHILE Expr Statements LOOP
# 		| DO UNTIL Expr Statements LOOP
# 		| DO Statements LOOP WHILE Expr
# 		| DO Statements LOOP UNTIL Expr
# 		| DO Statements LOOP
@dataclass
class LoopStatement(CodeStatement):
    condition : Expression | None
    body : CodeStatement
    precond: bool # с предусловием


# ForStatement -> FOR VarDef = Expr TO Expr Statements NEXT VarDef
@dataclass
class IterationStatement(CodeStatement):
    variable : str
    var_coord: pe.Fragment
    start : Expression
    start_coord : pe.Fragment
    end : Expression
    end_coord : pe.Fragment
    body : CodeStatement
    var_next : str
    var_next_coord : pe.Fragment

    @pe.ExAction
    def create(attrs, coords, res_coord):
        var, start, end, body, varnext = attrs
        cfor, cvar, cass, cstart, cto, cend, cstats, cnect, cvar2 = coords
        tmp = IterationStatement(var, cvar,start, cstart, end, cend, body, varnext, cvar2)
        tmp.validate_iteration([])
        return tmp

    def validate_iteration(self, vars):
        IntegerTypeError.validate_integer(self.variable, self.var_coord)
        IntegerTypeError.validate_integer(self.start, self.start_coord)
        IntegerTypeError.validate_integer(self.end, self.end_coord)
        IntegerTypeError.validate_integer(self.var_next, self.var_next_coord)

# DimStatement - > DIM Factor
@dataclass
class DeclarationStatement(CodeStatement):
    variable : Expression
    var_coord : pe.Fragment

    @pe.ExAction
    def create(attrs, coords, res_coord):
        var = attrs
        cdim, cfactor = coords
        tmp = DeclarationStatement(var, cfactor)
        tmp.validate_declaration([])
        return tmp

    def validate_declaration(self, vars):
        if hasattr(self.variable[0].type[0], 'varname'):
            IntegerTypeError.validate_integer(self.variable[0].type[0].varname, self.var_coord)
        else:
            IntegerTypeError.validate_integer(self.variable[0].type[0], self.var_coord)


@dataclass
class EmptyStatement(CodeStatement):
    pass


@dataclass
class VariableExpression(Expression):
    varname : str


class FunctionVariableExpression(VariableDefinition):

    args : list[Expression]

    @pe.ExAction
    def create(attrs, coords, res_coord):
        var, args = attrs
        firstCoord = coords[0]
        tmp = FunctionVariableExpression(var, args)
        tmp.validate_function_call(firstCoord)
        return tmp

    def validate_function_call(self, coord):
        name = self.name.name
        if name in funcs:
           for idx, arg in enumerate(self.type):
               type1, type2 = extract_data_type(arg), funcs[name][idx][1]
               if type1 != type2:
                    if type1 == DataType.String and type2 != DataType.String:
                        raise TypeMismatchError(coord, type1, ', а необходим аргумент',type2)
                    if type2 == DataType.Double and type1 == DataType.String:
                        raise TypeMismatchError(coord, type1, ', а необходим аргумент', type2)
                    if type2 == DataType.Float and (type1 == DataType.String or type1 == DataType.Double):
                        raise TypeMismatchError(coord, type1, ', а необходим аргумент', type2)
                    if type2 == DataType.Long and (type1 == DataType.String or type1 == DataType.Double or type1 == DataType.Float):
                        raise TypeMismatchError(coord, type1, ', а необходим аргумент', type2)
                    if type2 == DataType.Integer and (type1 == DataType.String or type1 == DataType.Double or type1 == DataType.Float\
                            or type1 == DataType.Long):
                       raise TypeMismatchError(coord, type1, ', а необходим аргумент', type2)
           return
        if len(self.type) > 0 and extract_data_type(self.type[0]) != DataType.Integer and extract_data_type(self.type[0]) != DataType.Long:
            raise ArrayIndexError(coord)



@dataclass
class ConstantExpression(Expression):
    value : typing.Any
    type : DataType


@dataclass
class BinaryOperationExpression(Expression):
    left : Expression
    op : str
    right : Expression
    type : DataType

    @pe.ExAction
    def create(attrs, coords, res_coord):
        l, op, r = attrs
        lType = None
        if type(l) is VariableExpression:
            lType = l.varname.type
        elif type(l) is FunctionVariableExpression:
            lType = l.name.type
        elif type(l) is BinaryOperationExpression or type(l) is ConstantExpression:
            lType = l.type
        rType = None
        if type(r) is VariableExpression:
            rType = r.varname.type
        elif type(r) is FunctionVariableExpression:
            rType = r.name.type
        elif type(r) is BinaryOperationExpression or type(r) is ConstantExpression:
            rType = r.type

        if lType == DataType.String and rType != DataType.String:
            raise TypeMismatchError(coords[0], DataType.String, op, rType)
        if lType != DataType.String and rType == DataType.String:
            raise TypeMismatchError(coords[0], lType, op, rType)


        resType = None
        if type(l) is ConstantExpression:
            l.type = rType
            resType = rType
        elif type(r) is ConstantExpression:
            r.type = rType
            resType = lType
        elif lType == DataType.Double or rType == DataType.Double:
            resType = DataType.Double
        elif lType == DataType.Float or rType == DataType.Float:
            resType = DataType.Float
        elif lType == DataType.Long or rType == DataType.Long:
            resType = DataType.Long
        elif lType == DataType.Integer or rType == DataType.Integer:
            resType = DataType.Integer
        else:
            resType = DataType.String
            if op != '+':
                raise InvalidOperationError(coords[0], op)

        return BinaryOperationExpression(l, op, r, resType)

@dataclass
class UnaryOperationExpression(Expression):
    op : str
    expr : Expression


INTEGER = pe.Terminal('INTEGER', '[0-9]+', int, priority=7)
LONG = pe.Terminal('LONG', '[0-9]+', int, priority=7)
FLOAT = pe.Terminal('FLOAT',   '[0-9]+(\\.[0-9]*)?(e[-+]?[0-9]+)?', float)
DOUBLE = pe.Terminal('DOUBLE', '[0-9]+(\\.[0-9]*)?(e[-+]?[0-9]+)?', float)
STRING = pe.Terminal('STRING', '\\"[^\'\n]*\\"',  str)
VARNAME = pe.Terminal('VARNAME', '[A-Za-z][A-Za-z0-9]*', str.upper)
#% — целое, & — длинное целое, ! — вещественное одинарной точности, # — вещественное двойной точности, $ — строка

def create_keyword_terminal(image):
    return pe.Terminal(image, image, lambda name: None,
                       re_flags=re.IGNORECASE, priority=10)

KW_FUNCTION, KW_SUB, KW_NEXT, KW_END, KW_INTEGER, KW_LONG, KW_FLOAT, KW_DOUBLE, KW_STRING = \
    map(create_keyword_terminal, 'function sub next end % & ! # \\$'.split())

KW_IF, KW_THEN, KW_ELSE, KW_WHILE, KW_UNTIL, KW_DO, KW_LOOP, KW_FOR, KW_TO, KW_DIM = \
    map(create_keyword_terminal, 'if then else while until do loop for to dim'.split())

KW_OR, KW_AND, KW_NOT, KW_TRUE, KW_FALSE = \
    map(create_keyword_terminal, 'or and not true false'.split())


NProgram, NFunctionDefs, NFunctionDef, NFunctionParams, NVarDef, NType, NStatements = \
    map(pe.NonTerminal, 'Program FunctionDefs FunctionDef FunctionParams VarDef Type Statements'.split())

NStatement, NExpr, NCmpOp, NArithmExpr, NAddOp = \
    map(pe.NonTerminal, 'Statement Expr CmpOp ArithmOp AddOp'.split())

NTerm, NMulOp, NFactor, NConst = \
    map(pe.NonTerminal, 'Term MulOp Factor Const'.split())


NProgram |=  NFunctionDefs, NStatements, ProgramStructure

NFunctionDefs |= lambda: []
NFunctionDefs |= NFunctionDefs, NFunctionDef, lambda fds, fd: fds + [fd]

NFunctionDef |= KW_FUNCTION, NVarDef, '(', NFunctionParams, ')', NStatements, KW_END, KW_FUNCTION, FunctionDefinition.create
NFunctionDef |= KW_SUB, VARNAME, '(', NFunctionParams, ')', NStatements, KW_END, KW_SUB, FunctionDefinition.create

NFunctionParams |= lambda: []
NFunctionParams |= NFunctionParams, ',', NExpr, lambda fps, fp: fps + [fp]
NFunctionParams |= NExpr, lambda fp: [fp]



NVarDef |= VARNAME, NType, VariableDefinition
NVarDef |= VARNAME, VariableDefinition

NType |= KW_INTEGER, lambda: DataType.Integer
NType |= KW_LONG, lambda: DataType.Long
NType |= KW_FLOAT, lambda: DataType.Float
NType |= KW_DOUBLE, lambda: DataType.Double
NType |= KW_STRING, lambda: DataType.String

NStatements |= lambda: []
NStatements |= NStatements, NStatement, lambda sts, st: sts + [st]

NStatement |= NFactor, '=', NExpr, AssignmentStatement.create

NStatement |= (
    KW_IF, NExpr, KW_THEN, NStatements, KW_END, KW_IF, ConditionalStatement
)
NStatement |= (
    KW_IF, NExpr, KW_THEN, NStatements, KW_ELSE, NStatements, KW_END, KW_IF, ConditionalElseStatement
)

NStatement |= (
    KW_DO, KW_WHILE, NExpr, NStatements, KW_LOOP, lambda expr, sts: LoopStatement(expr, sts, True)
)
NStatement |= (
    KW_DO, KW_UNTIL, NExpr, NStatements, KW_LOOP, lambda expr, sts: LoopStatement(expr, sts, True)
)
NStatement |= (
    KW_DO, NStatements, KW_LOOP, KW_WHILE, NExpr, lambda sts, expr: LoopStatement(expr, sts, False)
)
NStatement |= (
    KW_DO, NStatements, KW_LOOP, KW_UNTIL, NExpr, lambda sts, expr: LoopStatement(expr, sts, False)
)
NStatement |= (
    KW_DO, NStatements, KW_LOOP, lambda sts: LoopStatement(None, sts, False)
)
NStatement |= (
    KW_FOR, NVarDef, '=', NExpr, KW_TO, NExpr, NStatements, KW_NEXT, NVarDef, IterationStatement.create
)
NStatement |= (
    KW_DIM, NFactor, DeclarationStatement.create
)


NExpr |= NArithmExpr
NExpr |= NArithmExpr, NCmpOp, NArithmExpr, BinaryOperationExpression.create

def create_operator_lambda(op):
    return lambda: op

for op in ('>', '<', '>=', '<=', '==', '<>'):
    NCmpOp |= op, create_operator_lambda(op)

NArithmExpr |= NTerm
NArithmExpr |= '+', NTerm, lambda t: UnaryOperationExpression('+', t)
NArithmExpr |= '-', NTerm, lambda t: UnaryOperationExpression('-', t)
NArithmExpr |= NArithmExpr, NAddOp, NTerm, BinaryOperationExpression.create

NAddOp |= '+', lambda: '+'
NAddOp |= '-', lambda: '-'
NAddOp |= KW_OR, lambda: 'or'

NTerm |= NFactor
NTerm |= NTerm, NMulOp, NFactor, BinaryOperationExpression.create

NMulOp |= '*', lambda: '*'
NMulOp |= '/', lambda: '/'
NMulOp |= KW_AND, lambda: 'and'

NFactor |= KW_NOT, NFactor, lambda p: UnaryOperationExpression('not', p)
NFactor |= NVarDef, '(', NFunctionParams, ')', FunctionVariableExpression.create
NFactor |= NVarDef, '[', NExpr, ']', FunctionVariableExpression.create
NFactor |= NVarDef, lambda t: VariableExpression(t)
NFactor |= NConst
NFactor |= '(', NExpr, ')'


NConst |= INTEGER, lambda v: ConstantExpression(v, DataType.Integer)
NConst |= LONG, lambda v: ConstantExpression(v, DataType.Long)
NConst |= FLOAT, lambda v: ConstantExpression(v, DataType.Float)
NConst |= DOUBLE, lambda v: ConstantExpression(v, DataType.Double)
NConst |= STRING, lambda v: ConstantExpression(v, DataType.String)
NConst |= KW_TRUE, lambda: ConstantExpression(True, DataType.Boolean)
NConst |= KW_FALSE, lambda: ConstantExpression(False, DataType.Boolean)



p = pe.Parser(NProgram)

p.add_skipped_domain('\\s')
p.add_skipped_domain('\'.*\n')


class SemanticAnalysisError(pe.Error):
    pass

class ArrayIndexError(SemanticAnalysisError):
    def __init__(self, pos):
        self.pos = pos

    @property
    def message(self):
        return f'Индексация не целочисленная: {self.pos}'

class TypeMismatchError(SemanticAnalysisError):
    def __init__(self, pos, left, op, right):
        self.pos = pos
        self.left = left
        self.op = op
        self.right = right

    @property
    def message(self):
        return f'Несовместимые типы: {self.left} {self.op} {self.right}'

class InvalidOperationError(SemanticAnalysisError):
    def __init__(self, pos, op):
        self.pos = pos
        self.op = op
    @property
    def message(self):
        return f'Невозомжная операция: {self.op} {self.pos}'

class DuplicateFunctionError(SemanticAnalysisError):
    def __init__(self, pos, funcname):
        self.pos = pos
        self.funcname = funcname

    @property
    def message(self):
        return f'Повторная функция {self.funcname}'

class IntegerTypeError(SemanticAnalysisError):
    def __init__(self, pos, type_):
        self.pos = pos
        self.type = type_

    @property
    def message(self):
        return f'Ожидался целый тип, получен {self.type}'

    @staticmethod
    def validate_integer(got, pos):
        if type(got) is VariableDefinition and not (got.type == DataType.Integer or got.type == DataType.Long):
            raise IntegerTypeError(pos, got.type)
        elif type(got) is VariableExpression and not (got.varname.type == DataType.Integer or got.varname.type == DataType.Long):
            raise IntegerTypeError(pos, got.varname.type)
        elif type(got) is FunctionVariableExpression and not (got.name.type == DataType.Integer or got.name.type == DataType.Long):
            raise IntegerTypeError(pos, got.name.type)
        else:
            pass






for filename in sys.argv[1:]:
    try:
        with open(filename) as f:
            tree = p.parse(f.read())
            tree.validate()
            pprint(tree)
    except pe.Error as e:
        print(f'Ошибка {e.pos}: {e.message}')
    except Exception as e:
        print(e)