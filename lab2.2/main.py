import abc
import enum
import parser_edsl.parser_edsl as pe
import sys
import re
import typing
from dataclasses import dataclass
from pprint import pprint


class Type(enum.Enum):
    Integer = 'INTEGER'
    Long = 'LONG'
    Float = 'FLOAT'
    Double = 'DOUBLE'
    String = 'STRING'
    Boolean = 'BOOL'

# VarDef -> VARNAME Type
@dataclass
class VarDef:
    name : str 
    type : typing.Optional[Type]

# Statement -> AssignStatement
# 	   | IfStatement
# 	   | IfElseStatement
# 	   | WhileStatement
# 	   | ForStatement
# 	   | DimStatement
# 	   | SwitchStatement
class Statement(abc.ABC):
    pass

# Function -> FUNCTION VarDef ( FunctionParams ) Statements END FUNCTION
# 	  | SUB VARNAME ( FunctionParams ) Statements END SUB
@dataclass
class FunctionDef(Statement):
    FunctionName : str
    args : list[str]
    FunctionBlock : Statement

# Program ->  Functions
@dataclass
class Program:
    function_defs : list[FunctionDef]
    statements : list[Statement]

# Expr -> ArithmExpr
#       | ArithmExpr > ArithmExpr
#       | ArithmExpr < ArithmExpr
#       | ArithmExpr >= ArithmExpr
#       | ArithmExpr <= ArithmExpr
#       | ArithmExpr == ArithmExpr
#       | ArithmExpr <> ArithmExpr
class Expr(abc.ABC):
    pass

# AssignStatement -> Factor = Expr
@dataclass
class AssignStatement(Statement):
    variable : str
    expr : Expr

# IfStatement -> IF Expr THEN Statements END IF
@dataclass
class IfStatement(Statement):
    condition : Expr
    then_branch : Statement

# IfElseStatement -> IF Expr THEN Statements ELSE Statements END IF
@dataclass
class IfElseStatement(IfStatement):
    else_branch : Statement

# WhileStatement -> DO WHILE Expr Statements LOOP
# 		| DO UNTIL Expr Statements LOOP
# 		| DO Statements LOOP WHILE Expr
# 		| DO Statements LOOP UNTIL Expr
# 		| DO Statements LOOP
@dataclass
class WhileStatement(Statement):
    condition : Expr | None
    body : Statement
    precond: bool # с предусловием


# ForStatement -> FOR VarDef = Expr TO Expr Statements NEXT VarDef
@dataclass
class ForStatement(Statement):
    variable : str
    start : Expr
    end : Expr
    body : Statement
    var_next : str

# DimStatement - > DIM Factor
@dataclass
class DimStatement(Statement):
    variable : Expr


# SwitchStatement -> SWITCH Expr CaseStatements END SWITCH
@dataclass
class SwitchStatement(Statement):
    expr : Expr
    cases : list['CaseStatement']


# CaseStatement -> CASE CaseValues Statements
#                | CASE ELSE Statements
@dataclass
class CaseStatement(Statement):
    values : list[Expr] | None
    statements : list[Statement]


@dataclass
class EmptyStatement(Statement):
    pass


@dataclass
class VariableExpr(Expr):
    varname : str

class FuncVariableExpr(VarDef):
    args : list[Expr]


@dataclass
class ConstExpr(Expr):
    value : typing.Any
    type : Type


@dataclass
class BinOpExpr(Expr):
    left : Expr
    op : str
    right : Expr


@dataclass
class UnOpExpr(Expr):
    op : str
    expr : Expr


INTEGER = pe.Terminal('INTEGER', '[0-9]+', int, priority=7)
LONG = pe.Terminal('LONG', '[0-9]+', int, priority=7)
FLOAT = pe.Terminal('FLOAT',   '[0-9]+(\\.[0-9]*)?(e[-+]?[0-9]+)?', float)
DOUBLE = pe.Terminal('DOUBLE', '[0-9]+(\\.[0-9]*)?(e[-+]?[0-9]+)?', float)
STRING = pe.Terminal('STRING', '\\"[^\'\n]*\\"',  str)
VARNAME = pe.Terminal('VARNAME', '[A-Za-z][A-Za-z0-9]*', str.upper)
#% — целое, & — длинное целое, ! — вещественное одинарной точности, # — вещественное двойной точности, $ — строка

def make_keyword(image):
    return pe.Terminal(image, image, lambda name: None,
                       re_flags=re.IGNORECASE, priority=10)

KW_FUNCTION, KW_SUB, KW_NEXT, KW_END, KW_INTEGER, KW_LONG, KW_FLOAT, KW_DOUBLE, KW_STRING = \
    map(make_keyword, 'function sub next end % & ! # \\$'.split())

KW_IF, KW_THEN, KW_ELSE, KW_WHILE, KW_UNTIL, KW_DO, KW_LOOP, KW_FOR, KW_TO, KW_DIM = \
    map(make_keyword, 'if then else while until do loop for to dim'.split())

KW_SWITCH, KW_CASE = \
    map(make_keyword, 'switch case'.split())

KW_OR, KW_AND, KW_NOT, KW_TRUE, KW_FALSE = \
    map(make_keyword, 'or and not true false'.split())


NProgram, NFunctionDefs, NFunctionDef, NFunctionParams, NVarDef, NType, NStatements = \
    map(pe.NonTerminal, 'Program FunctionDefs FunctionDef FunctionParams VarDef Type Statements'.split())

NStatement, NExpr, NCmpOp, NArithmExpr, NAddOp = \
    map(pe.NonTerminal, 'Statement Expr CmpOp ArithmOp AddOp'.split())

NTerm, NMulOp, NFactor, NConst = \
    map(pe.NonTerminal, 'Term MulOp Factor Const'.split())

NSwitchStatement, NCaseStatements, NCaseStatement, NCaseValues = \
    map(pe.NonTerminal, 'SwitchStatement CaseStatements CaseStatement CaseValues'.split())


NProgram |=  NFunctionDefs, NStatements, Program

NFunctionDefs |= lambda: []
NFunctionDefs |= NFunctionDefs, NFunctionDef, lambda fds, fd: fds + [fd]

NFunctionDef |= KW_FUNCTION, NVarDef, '(', NFunctionParams, ')', NStatements, KW_END, KW_FUNCTION, FunctionDef
NFunctionDef |= KW_SUB, VARNAME, '(', NFunctionParams, ')', NStatements, KW_END, KW_SUB, FunctionDef

NFunctionParams |= lambda: []
NFunctionParams |= NFunctionParams, ',', NExpr, lambda fps, fp: fps + [fp]
NFunctionParams |= NExpr, lambda fp: [fp]



NVarDef |= VARNAME, NType, VarDef
NVarDef |= VARNAME, VarDef

NType |= KW_INTEGER, lambda: Type.Integer
NType |= KW_LONG, lambda: Type.Long
NType |= KW_FLOAT, lambda: Type.Float
NType |= KW_DOUBLE, lambda: Type.Double
NType |= KW_STRING, lambda: Type.String

NStatements |= lambda: []
NStatements |= NStatements, NStatement, lambda sts, st: sts + [st]

NStatement |= NFactor, '=', NExpr, AssignStatement

NStatement |= (
    KW_IF, NExpr, KW_THEN, NStatements, KW_END, KW_IF, IfStatement
)
NStatement |= (
    KW_IF, NExpr, KW_THEN, NStatements, KW_ELSE, NStatements, KW_END, KW_IF, IfElseStatement
)

NStatement |= (
    KW_DO, KW_WHILE, NExpr, NStatements, KW_LOOP, lambda expr, sts: WhileStatement(expr, sts, True)
)
NStatement |= (
    KW_DO, KW_UNTIL, NExpr, NStatements, KW_LOOP, lambda expr, sts: WhileStatement(expr, sts, True)
)
NStatement |= (
    KW_DO, NStatements, KW_LOOP, KW_WHILE, NExpr, lambda sts, expr: WhileStatement(expr, sts, False)
)
NStatement |= (
    KW_DO, NStatements, KW_LOOP, KW_UNTIL, NExpr, lambda sts, expr: WhileStatement(expr, sts, False) 
)
NStatement |= (
    KW_DO, NStatements, KW_LOOP, lambda sts: WhileStatement(None, sts, False) 
)
NStatement |= (
    KW_FOR, NVarDef, '=', NExpr, KW_TO, NExpr, NStatements, KW_NEXT, NVarDef, ForStatement
)
NStatement |= (
    KW_DIM, NFactor, DimStatement
)

NStatement |= NSwitchStatement

NSwitchStatement |= (
    KW_SWITCH, NExpr, NCaseStatements, KW_END, KW_SWITCH,
    lambda expr, cases: SwitchStatement(expr, cases)
)

NCaseStatements |= lambda: []
NCaseStatements |= NCaseStatements, NCaseStatement, lambda cases, case: cases + [case]

NCaseStatement |= (
    KW_CASE, NCaseValues, ':', NStatements,
    lambda values, statements: CaseStatement(values, statements)
)
NCaseStatement |= (
    KW_CASE, KW_ELSE, ':', NStatements,
    lambda statements: CaseStatement(None, statements)
)

NCaseValues |= NExpr, lambda expr: [expr]
NCaseValues |= NCaseValues, ',', NExpr, lambda values, expr: values + [expr]


NExpr |= NArithmExpr
NExpr |= NArithmExpr, NCmpOp, NArithmExpr, BinOpExpr

def make_op_lambda(op):
    return lambda: op

for op in ('>', '<', '>=', '<=', '==', '<>'):
    NCmpOp |= op, make_op_lambda(op)

NArithmExpr |= NTerm
NArithmExpr |= '+', NTerm, lambda t: UnOpExpr('+', t)
NArithmExpr |= '-', NTerm, lambda t: UnOpExpr('-', t)
NArithmExpr |= NArithmExpr, NAddOp, NTerm, BinOpExpr

NAddOp |= '+', lambda: '+'
NAddOp |= '-', lambda: '-'
NAddOp |= KW_OR, lambda: 'or'

NTerm |= NFactor
NTerm |= NTerm, NMulOp, NFactor, BinOpExpr

NMulOp |= '*', lambda: '*'
NMulOp |= '/', lambda: '/'
NMulOp |= KW_AND, lambda: 'and'

NFactor |= KW_NOT, NFactor, lambda p: UnOpExpr('not', p)
NFactor |= NVarDef, '(', NFunctionParams, ')', FuncVariableExpr
NFactor |= NVarDef, '[', NExpr, ']', FuncVariableExpr
NFactor |= NVarDef, lambda t: VariableExpr(t)
NFactor |= NConst
NFactor |= '(', NExpr, ')'


NConst |= INTEGER, lambda v: ConstExpr(v, Type.Integer)
NConst |= LONG, lambda v: ConstExpr(v, Type.Long)
NConst |= FLOAT, lambda v: ConstExpr(v, Type.Float)
NConst |= DOUBLE, lambda v: ConstExpr(v, Type.Double)
NConst |= STRING, lambda v: ConstExpr(v, Type.String)
NConst |= KW_TRUE, lambda: ConstExpr(True, Type.Boolean)
NConst |= KW_FALSE, lambda: ConstExpr(False, Type.Boolean)



p = pe.Parser(NProgram)

p.add_skipped_domain('\\s')
p.add_skipped_domain('\'.*\n')


for filename in sys.argv[1:]:
    try:
        with open(filename) as f:
            tree = p.parse(f.read())
            pprint(tree)
    except pe.Error as e:
        print(f'Ошибка {e.pos}: {e.message}')
    except Exception as e:
        print(e)