import re
from dataclasses import dataclass, field
from typing import List, Optional, Union, Any

class TokenType:
    INT = "INT"
    DOUBLE = "DOUBLE"
    FOR = "FOR"
    WHILE = "WHILE"
    RETURN = "RETURN"
    IDENT = "IDENT"
    I_CONSTANT = "I_CONSTANT"
    F_CONSTANT = "F_CONSTANT"
    LBRACE = "LBRACE"
    RBRACE = "RBRACE"
    LPAREN = "LPAREN"
    RPAREN = "RPAREN"
    LBRACK = "LBRACK"
    RBRACK = "RBRACK"
    SEMI = "SEMI"
    COMMA = "COMMA"
    ASSIGN = "ASSIGN"
    STAR = "STAR"
    PLUS = "PLUS"
    MINUS = "MINUS"
    INC_OP = "INC_OP"
    LT_OP = "LT_OP"
    EQ_OP = "EQ_OP"
    ADD_ASSIGN = "ADD_ASSIGN"
    MUL_ASSIGN = "MUL_ASSIGN"
    QUESTION = "QUESTION"
    COLON = "COLON"
    EOF = "EOF"

@dataclass
class Token:
    kind: str
    text: str
    line: int = 0
    col: int = 0

    def __repr__(self):
       return f'Tkn({self.kind}, {self.text}, {self.line}, {self.col})\n'

class Lexer:
    def __init__(self, text: str):
        self.text = text
        self.pos = 0
        self.line = 1
        self.col = 1
        self.errors = []
        
    def tokenize(self) -> List[Token]:
        tokens = []
        patterns = [
            (r'\bint\b', TokenType.INT),
            (r'\bdouble\b', TokenType.DOUBLE),
            (r'\bfor\b', TokenType.FOR),
            (r'\bwhile\b', TokenType.WHILE),
            (r'\breturn\b', TokenType.RETURN),
            (r'\+\+', TokenType.INC_OP),
            (r'==', TokenType.EQ_OP),
            (r'\+=', TokenType.ADD_ASSIGN),
            (r'\*=', TokenType.MUL_ASSIGN),
            (r'\d+\.\d*', TokenType.F_CONSTANT),
            (r'\d+', TokenType.I_CONSTANT),
            (r'[A-Za-z_]\w*', TokenType.IDENT),
            (r'\{', TokenType.LBRACE),
            (r'\}', TokenType.RBRACE),
            (r'\(', TokenType.LPAREN),
            (r'\)', TokenType.RPAREN),
            (r'\[', TokenType.LBRACK),
            (r'\]', TokenType.RBRACK),
            (r';', TokenType.SEMI),
            (r',', TokenType.COMMA),
            (r'=', TokenType.ASSIGN),
            (r'\*', TokenType.STAR),
            (r'\+', TokenType.PLUS),
            (r'-', TokenType.MINUS),
            (r'<', TokenType.LT_OP),
            (r'\?', TokenType.QUESTION),
            (r':', TokenType.COLON),
        ]
        
        skip_pattern = re.compile(r'[ \t\r\n]+|//[^\n]*|/\*.*?\*/', re.DOTALL) # dotall для многострочных
        
        while self.pos < len(self.text):
            match = skip_pattern.match(self.text, self.pos)
            if match:
                self._advance(len(match.group()))
                continue
                
            matched = False
            for pattern, token_type in patterns:
                regex = re.compile(pattern)
                match = regex.match(self.text, self.pos)
                if match:
                    value = match.group()
                    tokens.append(Token(token_type, value, self.line, self.col))
                    self._advance(len(value))
                    matched = True
                    break
                    
            if not matched:
                error_msg = f"Unexpected character '{self.text[self.pos]}' at line {self.line}, col {self.col}"
                self.errors.append(error_msg)
                self._advance(1)
                
        tokens.append(Token(TokenType.EOF, "", self.line, self.col))
        
        return tokens
    
    def _advance(self, count: int):
        for _ in range(count):
            if self.pos < len(self.text):
                if self.text[self.pos] == '\n':
                    self.line += 1
                    self.col = 1
                else:
                    self.col += 1
                self.pos += 1

class ASTNode: pass

@dataclass
class Expr(ASTNode): pass

@dataclass
class PrimaryExprIdentifier(Expr): 
    name: str

@dataclass
class PrimaryExprConstant(Expr): 
    value: Any
    kind: str

@dataclass
class PrimaryExprParenthesized(Expr): 
    expression: Expr

@dataclass
class PostfixExprIndex(Expr): 
    expression: Expr
    index_expression: Expr

@dataclass
class PostfixExprCall(Expr): 
    expression: Expr
    arguments: Optional[List[Expr]]

@dataclass
class PostfixExprIncDec(Expr): 
    expression: Expr
    operator: str

@dataclass
class UnaryExpr(Expr): 
    operator: str
    operand: Expr

@dataclass
class BinaryOp(Expr): 
    op: str
    left: Expr
    right: Expr

@dataclass
class ConditionalExpr(Expr): 
    condition: Expr
    true_expr: Expr
    false_expr: Expr

@dataclass
class AssignmentExpr(Expr): 
    left: Expr
    operator: str
    right: Expr

@dataclass
class TypeSpecifier(ASTNode): 
    kind: str

@dataclass
class Pointer(ASTNode): pass

@dataclass
class DirectDeclaratorIdentifier(ASTNode): 
    identifier: str

@dataclass
class DirectDeclaratorArray(ASTNode): 
    base: 'DirectDeclarator'
    size_expr: Optional[Expr]

@dataclass
class DirectDeclaratorFunction(ASTNode): 
    base: 'DirectDeclarator'
    parameters: 'ParameterTypeList'

DirectDeclarator = Union[DirectDeclaratorIdentifier, DirectDeclaratorArray, DirectDeclaratorFunction]

@dataclass
class Declarator(ASTNode):
    pointer: Optional[Pointer] = None
    direct_declarator: DirectDeclarator = None

@dataclass
class ParameterDeclaration(ASTNode):
    specifier: TypeSpecifier
    declarator: Declarator

@dataclass
class ParameterTypeList(ASTNode): 
    parameters: List[ParameterDeclaration]

@dataclass
class Initializer(ASTNode): 
    expression: Expr

@dataclass
class InitDeclarator(ASTNode):
    declarator: Declarator
    initializer: Optional[Initializer] = None

@dataclass
class Declaration(ASTNode):
    specifier: TypeSpecifier
    init_declarators: List[InitDeclarator] = field(default_factory=list)

@dataclass
class Statement(ASTNode): pass

@dataclass
class CompoundStatement(Statement): 
    block_items: List[Union[Declaration, Statement]] = field(default_factory=list)

@dataclass
class ExpressionStatement(Statement): 
    expression: Expr

@dataclass
class ForStatement(Statement):
    body: Statement
    init: Optional[Union[Declaration, Expr]] = None
    condition: Optional[Expr] = None
    iteration: Optional[Expr] = None

@dataclass
class WhileStatement(Statement): 
    condition: Expr
    body: Statement

@dataclass
class ReturnStatement(Statement): 
    expression: Optional[Expr] = None

@dataclass
class FunctionDefinition(ASTNode):
    specifier: TypeSpecifier
    declarator: Declarator
    body: CompoundStatement

@dataclass
class TranslationUnit(ASTNode):
    external_declarations: List[FunctionDefinition] = field(default_factory=list)

class ParserError(Exception):
    def __init__(self, message: str, token: Token = None):
        super().__init__(message)
        self.token = token


class Parser:
    def __init__(self, tokens: List[Token]):
        self.tokens = tokens
        self.pos = 0
        self.current = tokens[0] if tokens else None
        self.errors = []
        self.panic_mode = False
        
    def parse(self) -> TranslationUnit:
        functions = []
        while self.current.kind != TokenType.EOF:
            try:
                func = self._parse_function()
                if func:
                    functions.append(func)
            except ParserError as e:
                self._report_error(e)
                self._recover_to_function_level()
            
        return TranslationUnit(external_declarations=functions)
    
    def _advance(self):
        if self.pos < len(self.tokens) - 1:
            self.pos += 1
            self.current = self.tokens[self.pos]
    
    def _expect(self, kind: str) -> Token:
        if self.current.kind != kind:
            error_msg = f"Expected {kind}, got {self.current.kind} at {self.current.line}:{self.current.col}"
            raise ParserError(error_msg, self.current)
        token = self.current
        self._advance()
        return token
    
    def _report_error(self, error: ParserError):
        self.errors.append(str(error))
        self.panic_mode = True
    
    #Пропускает токены до конца текущей функции (закрывающая })
    #Или до начала следующей функци
    def _recover_to_function_level(self):
        brace_count = 0
        found_opening_brace = False
        
        while self.current.kind != TokenType.EOF:
            if self.current.kind == TokenType.LBRACE:
                brace_count += 1
                found_opening_brace = True
            elif self.current.kind == TokenType.RBRACE:
                brace_count -= 1
                if found_opening_brace and brace_count <= 0:
                    self._advance()
                    break
            elif (brace_count == 0 and found_opening_brace and
                  self.current.kind in [TokenType.INT, TokenType.DOUBLE]):
                break
            elif (not found_opening_brace and brace_count == 0 and
                  self.current.kind in [TokenType.INT, TokenType.DOUBLE]):
                break
            self._advance()
        self.panic_mode = False
    
    #Пропускает токены до разделителей операторов (;, {, })
    #Если находит ;, пропускает его
    def _recover_to_statement_level(self):
        while (self.current.kind != TokenType.EOF and
               self.current.kind not in [TokenType.SEMI, TokenType.LBRACE, TokenType.RBRACE]):
            self._advance()
        if self.current.kind == TokenType.SEMI:
            self._advance()
        self.panic_mode = False
    
    #Отслеживает баланс всех типов скобок (), {}, []
    #Останавливается на разделителях (;, ,) только если все скобки сбалансированы
    def _recover_to_expression_end(self):
        paren_count = 0
        brace_count = 0
        bracket_count = 0
        
        while self.current.kind != TokenType.EOF:
            if self.current.kind == TokenType.LPAREN:
                paren_count += 1
            elif self.current.kind == TokenType.RPAREN:
                paren_count -= 1
                if paren_count < 0:
                    break
            elif self.current.kind == TokenType.LBRACE:
                brace_count += 1
            elif self.current.kind == TokenType.RBRACE:
                brace_count -= 1
                if brace_count < 0:
                    break
            elif self.current.kind == TokenType.LBRACK:
                bracket_count += 1
            elif self.current.kind == TokenType.RBRACK:
                bracket_count -= 1
                if bracket_count < 0:
                    break
            elif (self.current.kind in [TokenType.SEMI, TokenType.COMMA] and
                  paren_count == 0 and brace_count == 0 and bracket_count == 0):
                break
            
            self._advance()
        
        self.panic_mode = False
    
    def _safe_expect(self, kind: str) -> Optional[Token]:
        if self.current.kind != kind:
            error_msg = f"Expected {kind}, got {self.current.kind} at {self.current.line}:{self.current.col}"
            self._report_error(ParserError(error_msg, self.current))
            return None
        token = self.current
        self._advance()
        return token
    
    def _match(self, kind: str) -> bool:
        if self.current.kind == kind:
            self._advance()
            return True
        return False
    
    # function_definition -> type_specifier declarator compound_statement
    def _parse_function(self) -> Optional[FunctionDefinition]:
        try:
            if self.current.kind not in [TokenType.INT, TokenType.DOUBLE]:
                raise ParserError(f"Expected type specifier, got {self.current.kind} at {self.current.line}:{self.current.col}", self.current)
            
            spec = TypeSpecifier(kind=self.current.text)
            self._advance()
            
            decl = self._parse_declarator()
            if not decl:
                decl = Declarator(direct_declarator=DirectDeclaratorIdentifier("__error__"))
            
            body = self._parse_compound_statement()
            if not body:
                body = CompoundStatement()
            
            return FunctionDefinition(specifier=spec, declarator=decl, body=body)
        except ParserError as e:
            self._report_error(e)
            self._recover_to_function_level()
            spec = TypeSpecifier(kind="int")
            decl = Declarator(direct_declarator=DirectDeclaratorIdentifier("__error__"))
            body = CompoundStatement()
            return FunctionDefinition(specifier=spec, declarator=decl, body=body)
    
    # declarator -> pointer? direct_declarator
    def _parse_declarator(self) -> Optional[Declarator]:
        try:
            ptr = None
            if self._match(TokenType.STAR):
                ptr = Pointer()
            
            direct_decl = self._parse_direct_declarator()
            if not direct_decl:
                direct_decl = DirectDeclaratorIdentifier("__error__")
            
            return Declarator(pointer=ptr, direct_declarator=direct_decl)
        except ParserError as e:
            self._report_error(e)
            return Declarator(direct_declarator=DirectDeclaratorIdentifier("__error__"))
    
    # direct_declarator -> IDENT | direct_declarator '[' constant_expression? ']' | direct_declarator '(' parameter_type_list ')'
    def _parse_direct_declarator(self) -> Optional[DirectDeclarator]:
        try:
            if self.current.kind != TokenType.IDENT:
                raise ParserError(f"Expected identifier, got {self.current.kind}", self.current)
            
            base = DirectDeclaratorIdentifier(self.current.text)
            self._advance()
            
            while True:
                if self._match(TokenType.LBRACK):
                    size_expr = None
                    if self.current.kind == TokenType.I_CONSTANT:
                        size_expr = PrimaryExprConstant(value=self.current.text, kind=self.current.kind)
                        self._advance()
                    if not self._safe_expect(TokenType.RBRACK):
                        while (self.current.kind != TokenType.EOF and
                               self.current.kind != TokenType.RBRACK and
                               self.current.kind not in [TokenType.LPAREN, TokenType.LBRACE]):
                            self._advance()
                        if self.current.kind == TokenType.RBRACK:
                            self._advance()
                    base = DirectDeclaratorArray(base, size_expr)
                elif self._match(TokenType.LPAREN):
                    params = self._parse_parameter_list()
                    if not self._safe_expect(TokenType.RPAREN):
                        paren_count = 1
                        while self.current.kind != TokenType.EOF and paren_count > 0:
                            if self.current.kind == TokenType.LPAREN:
                                paren_count += 1
                            elif self.current.kind == TokenType.RPAREN:
                                paren_count -= 1
                            elif self.current.kind == TokenType.LBRACE:
                                break
                            self._advance()
                    base = DirectDeclaratorFunction(base, params)
                else:
                    break
            return base
        except ParserError as e:
            self._report_error(e)
            return DirectDeclaratorIdentifier("__error__")
    
    # parameter_type_list -> parameter_declaration (',' parameter_declaration)*
    def _parse_parameter_list(self) -> ParameterTypeList:
        params = []
        if self.current.kind != TokenType.RPAREN and self.current.kind != TokenType.LBRACE:
            try:
                params.append(self._parse_parameter())
                while self._match(TokenType.COMMA) and self.current.kind != TokenType.LBRACE:
                    params.append(self._parse_parameter())
            except:
                pass
        return ParameterTypeList(parameters=params)
    
    # parameter_declaration -> type_specifier declarator
    def _parse_parameter(self) -> ParameterDeclaration:
        try:
            spec = TypeSpecifier(kind=self.current.text)
            self._advance()
            decl = self._parse_declarator()
            if not decl:
                decl = Declarator(direct_declarator=DirectDeclaratorIdentifier("__error__"))
            return ParameterDeclaration(specifier=spec, declarator=decl)
        except:
            spec = TypeSpecifier(kind="int")
            decl = Declarator(direct_declarator=DirectDeclaratorIdentifier("__error__"))
            return ParameterDeclaration(specifier=spec, declarator=decl)
    
    # compound_statement -> '{' (declaration | statement)* '}'
    def _parse_compound_statement(self) -> Optional[CompoundStatement]:
        try:
            if not self._safe_expect(TokenType.LBRACE):
                items = []
                while self.current.kind != TokenType.RBRACE and self.current.kind != TokenType.EOF:
                    try:
                        if self.current.kind in [TokenType.INT, TokenType.DOUBLE]:
                            decl = self._parse_declaration()
                            if decl:
                                items.append(decl)
                        else:
                            stmt = self._parse_statement()
                            if stmt:
                                items.append(stmt)
                    except ParserError as e:
                        self._report_error(e)
                        self._recover_to_statement_level()
                return CompoundStatement(block_items=items)
            
            items = []
            while self.current.kind != TokenType.RBRACE and self.current.kind != TokenType.EOF:
                try:
                    if self.current.kind in [TokenType.INT, TokenType.DOUBLE]:
                        decl = self._parse_declaration()
                        if decl:
                            items.append(decl)
                    else:
                        stmt = self._parse_statement()
                        if stmt:
                            items.append(stmt)
                except ParserError as e:
                    self._report_error(e)
                    self._recover_to_statement_level()
            
            if not self._safe_expect(TokenType.RBRACE):
                brace_count = 1
                while self.current.kind != TokenType.EOF and brace_count > 0:
                    if self.current.kind == TokenType.LBRACE:
                        brace_count += 1
                    elif self.current.kind == TokenType.RBRACE:
                        brace_count -= 1
                    self._advance()
            
            return CompoundStatement(block_items=items)
        except ParserError as e:
            self._report_error(e)
            return CompoundStatement()
    
    # declaration -> type_specifier init_declarator (',' init_declarator)* ';'
    def _parse_declaration(self) -> Optional[Declaration]:
        try:
            if self.current.kind not in [TokenType.INT, TokenType.DOUBLE]:
                raise ParserError(f"Expected type specifier, got {self.current.kind}", self.current)
            
            spec = TypeSpecifier(kind=self.current.text)
            self._advance()
            
            init_decls = []
            init_decl = self._parse_init_declarator()
            if init_decl:
                init_decls.append(init_decl)
            
            while self._match(TokenType.COMMA):
                init_decl = self._parse_init_declarator()
                if init_decl:
                    init_decls.append(init_decl)
            
            if not self._safe_expect(TokenType.SEMI):
                while (self.current.kind != TokenType.EOF and
                       self.current.kind != TokenType.SEMI and
                       self.current.kind not in [TokenType.RBRACE, TokenType.LBRACE]):
                    self._advance()
                if self.current.kind == TokenType.SEMI:
                    self._advance()
            
            return Declaration(specifier=spec, init_declarators=init_decls)
        except ParserError as e:
            self._report_error(e)
            spec = TypeSpecifier(kind="int")
            decl = Declarator(direct_declarator=DirectDeclaratorIdentifier("__error__"))
            return Declaration(specifier=spec, init_declarators=[InitDeclarator(declarator=decl)])
    
    # init_declarator -> declarator ('=' initializer)?
    def _parse_init_declarator(self) -> Optional[InitDeclarator]:
        try:
            decl = self._parse_declarator()
            if not decl:
                decl = Declarator(direct_declarator=DirectDeclaratorIdentifier("__error__"))
            
            init = None
            if self._match(TokenType.ASSIGN):
                expr = self._parse_expression()
                if expr:
                    init = Initializer(expression=expr)
            
            return InitDeclarator(declarator=decl, initializer=init)
        except ParserError as e:
            self._report_error(e)
            decl = Declarator(direct_declarator=DirectDeclaratorIdentifier("__error__"))
            return InitDeclarator(declarator=decl)
    
    # statement -> compound_statement | for_statement | while_statement | return_statement | expression_statement
    def _parse_statement(self) -> Optional[Statement]:
        try:
            if self.current.kind == TokenType.LBRACE:
                return self._parse_compound_statement()
            elif self.current.kind == TokenType.FOR:
                return self._parse_for_statement()
            elif self.current.kind == TokenType.WHILE:
                return self._parse_while_statement()
            elif self.current.kind == TokenType.RETURN:
                return self._parse_return_statement()
            else:
                return self._parse_expression_statement()
        except ParserError as e:
            self._report_error(e)
            expr = PrimaryExprConstant(value="0", kind="I_CONSTANT")
            return ExpressionStatement(expression=expr)
    
    # for_statement -> 'for' '(' (declaration | expression)? ';' expression? ';' expression? ')' statement
    def _parse_for_statement(self) -> Optional[ForStatement]:
        try:
            self._expect(TokenType.FOR)
            if not self._safe_expect(TokenType.LPAREN):
                body = CompoundStatement()
                return ForStatement(body=body)
            
            init = None
            if self.current.kind != TokenType.SEMI:
                if self.current.kind in [TokenType.INT, TokenType.DOUBLE]:
                    spec = TypeSpecifier(kind=self.current.text)
                    self._advance()
                    decl = self._parse_declarator()
                    initializer = None
                    if self._match(TokenType.ASSIGN):
                        expr = self._parse_expression()
                        if expr:
                            initializer = Initializer(expression=expr)
                    init = Declaration(specifier=spec, init_declarators=[InitDeclarator(declarator=decl, initializer=initializer)])
                else:
                    init = self._parse_expression()
            
            if not self._safe_expect(TokenType.SEMI):
                while (self.current.kind != TokenType.EOF and
                       self.current.kind != TokenType.SEMI and
                       self.current.kind != TokenType.RPAREN):
                    self._advance()
                if self.current.kind == TokenType.SEMI:
                    self._advance()
            
            condition = None
            if self.current.kind != TokenType.SEMI:
                condition = self._parse_expression()
            
            if not self._safe_expect(TokenType.SEMI):
                while (self.current.kind != TokenType.EOF and
                       self.current.kind != TokenType.SEMI and
                       self.current.kind != TokenType.RPAREN):
                    self._advance()
                if self.current.kind == TokenType.SEMI:
                    self._advance()
            
            iteration = None
            if self.current.kind != TokenType.RPAREN:
                iteration = self._parse_expression()
            
            if not self._safe_expect(TokenType.RPAREN):
                paren_count = 1
                while self.current.kind != TokenType.EOF and paren_count > 0:
                    if self.current.kind == TokenType.LPAREN:
                        paren_count += 1
                    elif self.current.kind == TokenType.RPAREN:
                        paren_count -= 1
                    self._advance()
            
            body = self._parse_statement()
            if not body:
                body = CompoundStatement()
            return ForStatement(body=body, init=init, condition=condition, iteration=iteration)
        except ParserError as e:
            self._report_error(e)
            body = CompoundStatement()
            return ForStatement(body=body)
    
    # while_statement -> 'while' '(' expression ')' statement
    def _parse_while_statement(self) -> Optional[WhileStatement]:
        try:
            self._expect(TokenType.WHILE)
            if not self._safe_expect(TokenType.LPAREN):
                condition = PrimaryExprConstant(value="1", kind="I_CONSTANT")
                body = CompoundStatement()
                return WhileStatement(condition=condition, body=body)
            
            condition = self._parse_expression()
            if not condition:
                condition = PrimaryExprConstant(value="1", kind="I_CONSTANT")
            
            if not self._safe_expect(TokenType.RPAREN):
                paren_count = 1
                while self.current.kind != TokenType.EOF and paren_count > 0:
                    if self.current.kind == TokenType.LPAREN:
                        paren_count += 1
                    elif self.current.kind == TokenType.RPAREN:
                        paren_count -= 1
                    self._advance()
            
            body = self._parse_statement()
            if not body:
                body = CompoundStatement()
            
            return WhileStatement(condition=condition, body=body)
        except ParserError as e:
            self._report_error(e)
            condition = PrimaryExprConstant(value="1", kind="I_CONSTANT")
            body = CompoundStatement()
            return WhileStatement(condition=condition, body=body)
    
    # return_statement -> 'return' expression? ';'
    def _parse_return_statement(self) -> Optional[ReturnStatement]:
        try:
            self._expect(TokenType.RETURN)
            expr = None
            if self.current.kind != TokenType.SEMI:
                expr = self._parse_expression()
            
            if not self._safe_expect(TokenType.SEMI):
                while (self.current.kind != TokenType.EOF and
                       self.current.kind != TokenType.SEMI and
                       self.current.kind not in [TokenType.RBRACE, TokenType.LBRACE]):
                    self._advance()
                if self.current.kind == TokenType.SEMI:
                    self._advance()
            
            return ReturnStatement(expression=expr)
        except ParserError as e:
            self._report_error(e)
            return ReturnStatement()
    
    # expression_statement -> expression ';'
    def _parse_expression_statement(self) -> Optional[ExpressionStatement]:
        try:
            expr = self._parse_expression()
            if not expr:
                expr = PrimaryExprConstant(value="0", kind="I_CONSTANT")
            
            if not self._safe_expect(TokenType.SEMI):
                while (self.current.kind != TokenType.EOF and
                       self.current.kind != TokenType.SEMI and
                       self.current.kind not in [TokenType.RBRACE, TokenType.LBRACE]):
                    self._advance()
                if self.current.kind == TokenType.SEMI:
                    self._advance()
            
            return ExpressionStatement(expression=expr)
        except ParserError as e:
            self._report_error(e)
            expr = PrimaryExprConstant(value="0", kind="I_CONSTANT")
            return ExpressionStatement(expression=expr)
    
    # expression -> assignment_expression
    def _parse_expression(self) -> Optional[Expr]:
        try:
            return self._parse_assignment_expression()
        except ParserError as e:
            self._report_error(e)
            self._recover_to_expression_end()
            return PrimaryExprConstant(value="0", kind="I_CONSTANT")
    
    # assignment_expression -> conditional_expression | conditional_expression assignment_operator assignment_expression
    def _parse_assignment_expression(self) -> Expr:
        left = self._parse_conditional_expression()
        if self.current.kind in [TokenType.ASSIGN, TokenType.ADD_ASSIGN, TokenType.MUL_ASSIGN]:
            op = self.current.text
            self._advance()
            right = self._parse_assignment_expression()
            return AssignmentExpr(left=left, operator=op, right=right)
        return left
    
    # conditional_expression -> equality_expression | equality_expression '?' expression ':' conditional_expression
    def _parse_conditional_expression(self) -> Expr:
        expr = self._parse_equality_expression()
        if self._match(TokenType.QUESTION):
            true_expr = self._parse_expression()
            self._expect(TokenType.COLON)
            false_expr = self._parse_conditional_expression()
            return ConditionalExpr(condition=expr, true_expr=true_expr, false_expr=false_expr)
        return expr
    
    # equality_expression -> relational_expression | equality_expression '==' relational_expression
    def _parse_equality_expression(self) -> Expr:
        left = self._parse_relational_expression()
        if self._match(TokenType.EQ_OP):
            right = self._parse_relational_expression()
            return BinaryOp(op="==", left=left, right=right)
        return left
    
    # relational_expression -> additive_expression | relational_expression '<' additive_expression
    def _parse_relational_expression(self) -> Expr:
        left = self._parse_additive_expression()
        if self._match(TokenType.LT_OP):
            right = self._parse_additive_expression()
            return BinaryOp(op="<", left=left, right=right)
        return left
    
    # additive_expression -> multiplicative_expression | additive_expression ('+' | '-') multiplicative_expression
    def _parse_additive_expression(self) -> Expr:
        left = self._parse_multiplicative_expression()
        while self.current.kind in [TokenType.PLUS, TokenType.MINUS]:
            op = self.current.text
            self._advance()
            right = self._parse_multiplicative_expression()
            left = BinaryOp(op=op, left=left, right=right)
        return left
    
    # multiplicative_expression -> unary_expression | multiplicative_expression '*' unary_expression
    def _parse_multiplicative_expression(self) -> Expr:
        left = self._parse_unary_expression()
        while self.current.kind == TokenType.STAR:
            op = self.current.text
            self._advance()
            right = self._parse_unary_expression()
            left = BinaryOp(op=op, left=left, right=right)
        return left
    
    # unary_expression -> postfix_expression | ('++' | '*' | '+' | '-') unary_expression
    def _parse_unary_expression(self) -> Expr:
        if self.current.kind == TokenType.INC_OP:
            op = self.current.text
            self._advance()
            return UnaryExpr(operator=op, operand=self._parse_unary_expression())
        elif self.current.kind == TokenType.STAR:
            op = self.current.text
            self._advance()
            return UnaryExpr(operator=op, operand=self._parse_unary_expression())
        elif self.current.kind in [TokenType.PLUS, TokenType.MINUS]:
            op = self.current.text
            self._advance()
            return UnaryExpr(operator=op, operand=self._parse_unary_expression())
        else:
            return self._parse_postfix_expression()
    
    # postfix_expression -> primary_expression | postfix_expression '[' expression ']' | postfix_expression '(' argument_expression_list? ')' | postfix_expression '++'
    def _parse_postfix_expression(self) -> Expr:
        expr = self._parse_primary_expression()
        while True:
            if self._match(TokenType.LBRACK):
                index = self._parse_expression()
                self._expect(TokenType.RBRACK)
                expr = PostfixExprIndex(expression=expr, index_expression=index)
            elif self._match(TokenType.LPAREN):
                args = None
                if self.current.kind != TokenType.RPAREN:
                    args = [self._parse_assignment_expression()]
                    while self._match(TokenType.COMMA):
                        args.append(self._parse_assignment_expression())
                self._expect(TokenType.RPAREN)
                expr = PostfixExprCall(expression=expr, arguments=args)
            elif self.current.kind == TokenType.INC_OP:
                op = self.current.text
                self._advance()
                expr = PostfixExprIncDec(expression=expr, operator=op)
            else:
                break
        return expr
    
    # primary_expression -> IDENT | I_CONSTANT | F_CONSTANT | '(' expression ')'
    def _parse_primary_expression(self) -> Expr:
        if self.current.kind == TokenType.IDENT:
            name = self.current.text
            self._advance()
            return PrimaryExprIdentifier(name=name)
        elif self.current.kind == TokenType.I_CONSTANT:
            value = self.current.text
            kind = self.current.kind
            self._advance()
            return PrimaryExprConstant(value=value, kind=kind)
        elif self.current.kind == TokenType.F_CONSTANT:
            value = self.current.text
            kind = self.current.kind
            self._advance()
            return PrimaryExprConstant(value=value, kind=kind)
        elif self._match(TokenType.LPAREN):
            expr = self._parse_expression()
            self._expect(TokenType.RPAREN)
            return PrimaryExprParenthesized(expression=expr)
        else:
            raise ParserError(f"Unexpected token: {self.current.kind} at {self.current.line}:{self.current.col}", self.current,)

def main():
    import sys
    
    if len(sys.argv) != 2:
        print("not enough args")
        sys.exit(1)
    
    filename = sys.argv[1]
    
    try:
        with open(filename, "r", encoding="utf-8") as f:
            source_code = f.read()
            
        lexer = Lexer(source_code)
        tokens = lexer.tokenize()
        parser = Parser(tokens)
        ast = parser.parse()
        
        import pprint
        pp = pprint.PrettyPrinter(indent=1, width=90, compact=True)
        
        pp.pprint(ast)
        pp.pprint(parser.errors)
        
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()