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
  
  
# VariableDecl -> VARNAME Type  
@dataclass  
class VariableDecl:  
    name : str  
    type : typing.Optional[Type]
  
  
# Instruction -> Assignment 
#           | Conditional 
#           | ConditionalWithElse 
#           | Loop 
#           | CounterLoop 
#           | Declaration 
#           | SwitchStatement 
class Instruction(abc.ABC):  
    pass  
  
  
# Subprogram -> FUNCTION VariableDecl ( Parameters ) CodeBlock END FUNCTION  
#       | SUB VARNAME ( Parameters ) CodeBlock END SUB  
@dataclass  
class SubprogramDef(Instruction):  
    SubprogramName : str  
    args : list[str]  
    SubprogramBlock : Instruction  
  
  
# Program -> Subprograms  
@dataclass  
class Program:  
    subprogram_defs : list[SubprogramDef]  
    instructions : list[Instruction]  
  
  
# Expression -> Arithmetic | Arithmetic > Arithmetic | ... (сравнения)  
class Expression(abc.ABC):  
    pass  
  
  
# Assignment -> Primary = Expression  
@dataclass  
class Assignment(Instruction):  
    variable : str  
    expr : Expression  
  
  
# Conditional -> IF Expression THEN CodeBlock END IF  
@dataclass  
class Conditional(Instruction):  
    condition : Expression  
    then_branch : Instruction  
  
  
# ConditionalWithElse -> IF Expression THEN CodeBlock ELSE CodeBlock END IF  
@dataclass  
class ConditionalWithElse(Conditional):  
    else_branch : Instruction  
  
  
# Loop -> DO WHILE/UNTIL Expression CodeBlock LOOP | DO CodeBlock LOOP WHILE/UNTIL | DO CodeBlock LOOP  
@dataclass  
class Loop(Instruction):  
    condition : Expression | None  
    body : Instruction  
    precond: bool  
  
  
# CounterLoop -> FOR VariableDecl = Expression TO Expression CodeBlock NEXT VariableDecl  
@dataclass  
class CounterLoop(Instruction):  
    variable : str  
    start : Expression  
    end : Expression  
    body : Instruction  
    var_next : str  
  
  
# Declaration -> DIM Primary  
@dataclass  
class Declaration(Instruction):  
    variable : Expression  
  
# SwitchStatement -> SWITCH Expr CaseStatements END SWITCH
@dataclass
class SwitchStatement(Instruction):
    expr : Expression
    cases : list['CaseStatement']


# CaseStatement -> CASE CaseValues Statements
#                | CASE ELSE Statements
@dataclass
class CaseStatement(Instruction):
    values : list[Expression] | None
    statements : list[Instruction]  

@dataclass  
class EmptyInstruction(Instruction):  
    pass  
  
  
@dataclass  
class VariableExpr(Expression):  
    varname : str  
  
  
class FuncVariableExpr(VariableDecl):  
    args : list[Expression]  
  
  
@dataclass  
class ConstExpr(Expression):  
    value : typing.Any  
    type : Type  
  
  
@dataclass  
class BinOpExpr(Expression):  
    left : Expression  
    op : str  
    right : Expression  
  
  
@dataclass  
class UnOpExpr(Expression):  
    op : str  
    expr : Expression  
  
  
INTEGER = pe.Terminal('INTEGER', '[0-9]+', int, priority=7)  
LONG = pe.Terminal('LONG', '[0-9]+', int, priority=7)  
FLOAT = pe.Terminal('FLOAT', '[0-9]+(\\.[0-9]*)?(e[-+]?[0-9]+)?', float)  
DOUBLE = pe.Terminal('DOUBLE', '[0-9]+(\\.[0-9]*)?(e[-+]?[0-9]+)?', float)  
STRING = pe.Terminal('STRING', '\\"[^\'\n]*\\"', str)  
VARNAME = pe.Terminal('VARNAME', '[A-Za-z][A-Za-z0-9]*', str.upper)  
  
  
def make_keyword(image):  
    return pe.Terminal(image, image, lambda name: None, re_flags=re.IGNORECASE, priority=10)  
  
  
KW_FUNCTION, KW_SUB, KW_NEXT, KW_END, KW_INTEGER, KW_LONG, KW_FLOAT, KW_DOUBLE, KW_STRING = \
    map(make_keyword, 'function sub next end % & ! # \\$'.split())  
  
KW_IF, KW_THEN, KW_ELSE, KW_WHILE, KW_UNTIL, KW_DO, KW_LOOP, KW_FOR, KW_TO, KW_DIM = \
    map(make_keyword, 'if then else while until do loop for to dim'.split())  
  
KW_OR, KW_AND, KW_NOT, KW_TRUE, KW_FALSE = \
    map(make_keyword, 'or and not true false'.split())  

KW_SWITCH, KW_CASE = \
    map(make_keyword, 'switch case'.split())
  
NProgram, NSubprograms, NSubprogramDef, NParameters, NVariableDecl, NType, NCodeBlock = \
    map(pe.NonTerminal, 'Program Subprograms SubprogramDef Parameters VariableDecl Type CodeBlock'.split())  
  
NInstruction, NExpression, NCmpOp, NArithmetic, NAddOp = \
    map(pe.NonTerminal, 'Instruction Expression CmpOp Arithmetic AddOp'.split())  
  
NMultiplicative, NMulOp, NPrimary, NLiteral = \
    map(pe.NonTerminal, 'Multiplicative MulOp Primary Literal'.split())  
  
NSwitchStatement, NCaseStatements, NCaseStatement, NCaseValues = \
    map(pe.NonTerminal, 'SwitchStatement CaseStatements CaseStatement CaseValues'.split())

NProgram |= NSubprograms, NCodeBlock, Program  
  
NSubprograms |= lambda: []  
NSubprograms |= NSubprograms, NSubprogramDef, lambda sps, sp: sps + [sp]  
  
NSubprogramDef |= KW_FUNCTION, NVariableDecl, '(', NParameters, ')', NCodeBlock, KW_END,KW_FUNCTION, SubprogramDef  
NSubprogramDef |= KW_SUB, VARNAME, '(', NParameters, ')', NCodeBlock, KW_END, KW_SUB, SubprogramDef  
  
NParameters |= lambda: []  
NParameters |= NParameters, ',', NExpression, lambda ps, p: ps + [p]  
NParameters |= NExpression, lambda p: [p]  
  
NVariableDecl |= VARNAME, NType, VariableDecl  
NVariableDecl |= VARNAME, VariableDecl  
  
NType |= KW_INTEGER, lambda: Type.Integer  
NType |= KW_LONG, lambda: Type.Long  
NType |= KW_FLOAT, lambda: Type.Float  
NType |= KW_DOUBLE, lambda: Type.Double  
NType |= KW_STRING, lambda: Type.String  
  
NCodeBlock |= lambda: []  
NCodeBlock |= NCodeBlock, NInstruction, lambda cbs, cb: cbs + [cb]  
  
NInstruction |= NPrimary, '=', NExpression, Assignment  
  
NInstruction |= KW_IF, NExpression, KW_THEN, NCodeBlock, KW_END, KW_IF, Conditional  
NInstruction |= KW_IF, NExpression, KW_THEN, NCodeBlock, KW_ELSE, NCodeBlock, KW_END, KW_IF, ConditionalWithElse  
  
NInstruction |= KW_DO, KW_WHILE, NExpression, NCodeBlock, KW_LOOP, lambda expr, cbs: Loop(expr, cbs, True)  
NInstruction |= KW_DO, KW_UNTIL, NExpression, NCodeBlock, KW_LOOP, lambda expr, cbs: Loop(expr, cbs, True)  
NInstruction |= KW_DO, NCodeBlock, KW_LOOP, KW_WHILE, NExpression, lambda cbs, expr: Loop(expr, cbs, False)  
NInstruction |= KW_DO, NCodeBlock, KW_LOOP, KW_UNTIL, NExpression, lambda cbs, expr: Loop(expr, cbs, False)  
NInstruction |= KW_DO, NCodeBlock, KW_LOOP, lambda cbs: Loop(None, cbs, False)  
  
NInstruction |= KW_FOR, NVariableDecl, '=', NExpression, KW_TO, NExpression, NCodeBlock, KW_NEXT, NVariableDecl, CounterLoop  
NInstruction |= KW_DIM, NPrimary, Declaration  

NInstruction |= NSwitchStatement

NSwitchStatement |= (
    KW_SWITCH, NExpression, NCaseStatements, KW_END, KW_SWITCH,
    lambda expr, cases: SwitchStatement(expr, cases)
)

NCaseStatements |= lambda: []
NCaseStatements |= NCaseStatements, NCaseStatement, lambda cases, case: cases + [case]

NCaseStatement |= (
    KW_CASE, NCaseValues, ':', NInstruction,
    lambda values, statements: CaseStatement(values, statements)
)
NCaseStatement |= (
    KW_CASE, KW_ELSE, ':', NInstruction,
    lambda statements: CaseStatement(None, statements)
)

NCaseValues |= NExpression, lambda expr: [expr]
NCaseValues |= NCaseValues, ',', NExpression, lambda values, expr: values + [expr]

NExpression |= NArithmetic  
NExpression |= NArithmetic, NCmpOp, NArithmetic, BinOpExpr  
  
  
def make_op_lambda(op):  
    return lambda: op  
  
  
for op in ('>', '<', '>=', '<=', '==', '<>'):  
    NCmpOp |= op, make_op_lambda(op)  
  
  
NArithmetic |= NMultiplicative  
NArithmetic |= '+', NMultiplicative, lambda t: UnOpExpr('+', t)  
NArithmetic |= '-', NMultiplicative, lambda t: UnOpExpr('-', t)  
NArithmetic |= NArithmetic, NAddOp, NMultiplicative, BinOpExpr  
  
NAddOp |= '+', lambda: '+'  
NAddOp |= '-', lambda: '-'  
NAddOp |= KW_OR, lambda: 'or'  
  
NMultiplicative |= NPrimary  
NMultiplicative |= NMultiplicative, NMulOp, NPrimary, BinOpExpr  
  
NMulOp |= '*', lambda: '*'  
NMulOp |= '/', lambda: '/'  
NMulOp |= KW_AND, lambda: 'and'  
  
NPrimary |= KW_NOT, NPrimary, lambda p: UnOpExpr('not', p)  
NPrimary |= NVariableDecl, '(', NParameters, ')', FuncVariableExpr  
NPrimary |= NVariableDecl, '[', NExpression, ']', FuncVariableExpr  
NPrimary |= NVariableDecl, lambda t: VariableExpr(t)  
NPrimary |= NLiteral  
NPrimary |= '(', NExpression, ')'  
  
NLiteral |= INTEGER, lambda v: ConstExpr(v, Type.Integer)  
NLiteral |= LONG, lambda v: ConstExpr(v, Type.Long)  
NLiteral |= FLOAT, lambda v: ConstExpr(v, Type.Float)  
NLiteral |= DOUBLE, lambda v: ConstExpr(v, Type.Double)  
NLiteral |= STRING, lambda v: ConstExpr(v, Type.String)  
NLiteral |= KW_TRUE, lambda: ConstExpr(True, Type.Boolean)  
NLiteral |= KW_FALSE, lambda: ConstExpr(False, Type.Boolean)  
  
  
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