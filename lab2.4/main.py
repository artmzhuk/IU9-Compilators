from __future__ import annotations
import re
import sys
from dataclasses import dataclass, field
from typing import List, Optional, Union, Any


# --- Tokenizer ---

class TokenKind:
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
    line: int
    col: int

    def __repr__(self) -> str:
        return f"{self.kind:<15} {self.line}:{self.col:<3} '{self.text}'"


TOKEN_SPEC = [
    (r"\bint\b", TokenKind.INT),
    (r"\bdouble\b", TokenKind.DOUBLE),
    (r"\bfor\b", TokenKind.FOR),
    (r"\bwhile\b", TokenKind.WHILE),
    (r"\breturn\b", TokenKind.RETURN),

    (r"\+\+", TokenKind.INC_OP),
    (r"==", TokenKind.EQ_OP),
    (r"<", TokenKind.LT_OP),
    (r"\+=", TokenKind.ADD_ASSIGN),
    (r"\*=", TokenKind.MUL_ASSIGN),

    (r"\d+\.\d*f?|\.\d+f?|\d+f", TokenKind.F_CONSTANT),
    (r"\d+", TokenKind.I_CONSTANT),

    (r"[A-Za-z_]\w*", TokenKind.IDENT),

    (r"\{", TokenKind.LBRACE), (r"\}", TokenKind.RBRACE), (r"\(", TokenKind.LPAREN),
    (r"\)", TokenKind.RPAREN), (r"\[", TokenKind.LBRACK), (r"\]", TokenKind.RBRACK),
    (r";", TokenKind.SEMI), (r",", TokenKind.COMMA), (r":", TokenKind.COLON),
    (r"=", TokenKind.ASSIGN), (r"\*", TokenKind.STAR), (r"\+", TokenKind.PLUS),
    (r"-", TokenKind.MINUS), (r"\?", TokenKind.QUESTION),
]

SKIP_REGEX = re.compile(r"[ \t\r\n]+|//[^\n]*|/\*.*?\*/", re.S)
MASTER_REGEX = re.compile("|".join(f"(?P<{kind}>{pat})" for pat, kind in TOKEN_SPEC))


class Lexer:
    def __init__(self, text: str):
        self.text, self.pos = text, 0
        self.line, self.col = 1, 1

    def tokenize(self) -> List[Token]:
        tokens: List[Token] = []
        while self.pos < len(self.text):
            m = SKIP_REGEX.match(self.text, self.pos)
            if m: self._adv(m.end() - self.pos); continue
            m = MASTER_REGEX.match(self.text, self.pos)
            if not m:
                raise RuntimeError(
                    f"Lexical error: '{self.text[self.pos]}' "
                    f"(line {self.line}, col {self.col})"
                )
            kind, lex = m.lastgroup, m.group(m.lastgroup)
            assert kind is not None
            tokens.append(Token(kind, lex, self.line, self.col))
            self._adv(len(lex))
        tokens.append(Token(TokenKind.EOF, "<EOF>", self.line, self.col))
        return tokens

    def _adv(self, n: int) -> None:
        for _ in range(n):
            if self.pos >= len(self.text): return
            ch = self.text[self.pos]
            self.pos += 1
            if ch == '\n':
                self.line += 1; self.col = 1
            else:
                self.col += 1


class ASTNode: pass


@dataclass
class Expr(ASTNode): pass


@dataclass
class PrimaryExprIdentifier(Expr): name: str


@dataclass
class PrimaryExprConstant(Expr): value: Any; kind: str


@dataclass
class PrimaryExprParenthesized(Expr): expression: Expr


@dataclass
class PostfixExpr(Expr): expression: Expr  # Base


@dataclass
class PostfixExprIndex(PostfixExpr): index_expression: Expr


@dataclass
class PostfixExprCall(PostfixExpr): arguments: Optional[List[Expr]]


@dataclass
class PostfixExprIncDec(PostfixExpr): operator: str  # "++" (postfix)


@dataclass
class ArgumentExpressionList(ASTNode): expressions: List[Expr]


@dataclass
class UnaryExpr(Expr): operator: str; operand: Expr


@dataclass
class BinaryOp(Expr): op: str; left: Expr; right: Expr


@dataclass
class ConditionalExpr(Expr): condition: Expr; true_expr: Expr; false_expr: Expr


@dataclass
class AssignmentExpr(Expr): left: Expr; operator: str; right: Expr


@dataclass
class TypeSpecifier(ASTNode): kind: str


@dataclass
class Pointer(ASTNode): pass


@dataclass
class DirectDeclarator(ASTNode): pass


@dataclass
class DirectDeclaratorIdentifier(DirectDeclarator): identifier: str


@dataclass
class DirectDeclaratorArray(DirectDeclarator): base: DirectDeclarator; size_expr: Optional[Expr]


@dataclass
class DirectDeclaratorFunction(DirectDeclarator): base: DirectDeclarator; parameters: "ParameterTypeList"


@dataclass
class Declarator(ASTNode):
    pointer: Optional[Pointer] = None
    direct_declarator: DirectDeclarator = None


@dataclass
class ParameterDeclaration(ASTNode):
    specifier: TypeSpecifier
    declarator: Declarator


@dataclass
class ParameterTypeList(ASTNode): parameters: List[ParameterDeclaration]


@dataclass
class Initializer(ASTNode): expression: Expr


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
class CompoundStatement(Statement): block_items: List[Union[Declaration, Statement]] = field(default_factory=list)


@dataclass
class ExpressionStatement(Statement): expression: Expr


@dataclass
class ForStatement(Statement):
    body: Statement
    init: Optional[Union[Declaration, Expr]] = None
    condition: Optional[Expr] = None
    iteration: Optional[Expr] = None


@dataclass
class WhileStatement(Statement): condition: Expr; body: Statement


@dataclass
class ReturnStatement(Statement): expression: Optional[Expr] = None


@dataclass
class FunctionDefinition(ASTNode):
    specifier: TypeSpecifier
    declarator: Declarator
    body: CompoundStatement


@dataclass
class TranslationUnit(ASTNode):
    external_declarations: List[FunctionDefinition] = field(default_factory=list)  # Only functions


# --- Parser (Simplified) ---
class ParserError(Exception): pass


class Parser:
    def __init__(self, tokens: List[Token]):
        self.toks, self.pos = tokens, 0
        self.cur = self.toks[0]

    def _adv(self):
        self.pos += 1
        if self.pos < len(self.toks):
            self.cur = self.toks[self.pos]
        else:
            self.cur = self.toks[-1]

    def _match(self, kind: str) -> bool:
        if self.cur.kind == kind:
            self._adv();
            return True
        return False

    def _expect(self, kind: str) -> Token:
        if self.cur.kind != kind:
            raise ParserError(
                f"Expected {kind}, got {self.cur.kind} "
                f"('{self.cur.text}') at line {self.cur.line}:{self.cur.col}"
            )
        tok = self.cur;
        self._adv();
        return tok

    def parse(self) -> TranslationUnit:
        return self._parse_translation_unit()

    def _parse_translation_unit(self) -> TranslationUnit:
        decls = []
        while self.cur.kind != TokenKind.EOF:
            decls.append(self._parse_function_definition())
        return TranslationUnit(external_declarations=decls)

    def _parse_type_specifier(self) -> TypeSpecifier:
        if self.cur.kind in (TokenKind.INT, TokenKind.DOUBLE):
            kind = self.cur.text
            self._adv()
            return TypeSpecifier(kind=kind)
        raise ParserError(f"Expected type specifier (int, double) at {self.cur}")

    def _parse_function_definition(self) -> FunctionDefinition:
        spec = self._parse_type_specifier()
        decl = self._parse_declarator()
        if not isinstance(decl.direct_declarator, DirectDeclaratorFunction):
            raise ParserError(f"Expected function declarator in function definition at {self.cur}")
        body = self._parse_compound_statement()
        return FunctionDefinition(specifier=spec, declarator=decl, body=body)

    def _parse_declarator(self) -> Declarator:
        ptr = None
        if self._match(TokenKind.STAR):
            ptr = Pointer()

        direct_decl = self._parse_direct_declarator()
        return Declarator(pointer=ptr, direct_declarator=direct_decl)

    def _parse_direct_declarator(self) -> DirectDeclarator:
        base_dd: DirectDeclarator
        if self.cur.kind == TokenKind.IDENT:
            base_dd = DirectDeclaratorIdentifier(self.cur.text);
            self._adv()
        else:
            raise ParserError(f"Expected identifier for direct_declarator at {self.cur}")

        while True:
            if self._match(TokenKind.LBRACK):
                size_expr: Optional[Expr] = None
                if self.cur.kind == TokenKind.I_CONSTANT:
                    size_expr = PrimaryExprConstant(value=self.cur.text, kind=self.cur.kind)
                    self._adv()
                else:
                    raise ParserError(f"Expected integer constant for array size at {self.cur}")
                self._expect(TokenKind.RBRACK)
                base_dd = DirectDeclaratorArray(base_dd, size_expr)
            elif self._match(TokenKind.LPAREN):  # Function declarator part
                params: ParameterTypeList = self._parse_parameter_type_list()
                self._expect(TokenKind.RPAREN)
                base_dd = DirectDeclaratorFunction(base_dd, params)
            else:
                break
        return base_dd

    def _parse_parameter_type_list(self) -> ParameterTypeList:
        params = []
        if self.cur.kind != TokenKind.RPAREN:  # If there are parameters
            params.append(self._parse_parameter_declaration())
            while self._match(TokenKind.COMMA):
                params.append(self._parse_parameter_declaration())
        return ParameterTypeList(parameters=params)

    def _parse_parameter_declaration(self) -> ParameterDeclaration:
        spec = self._parse_type_specifier()
        decl = self._parse_declarator()
        return ParameterDeclaration(specifier=spec, declarator=decl)

    def _parse_initializer(self) -> Initializer:
        # Only ` = expression` form
        return Initializer(expression=self._parse_assignment_expression())

    # --- Statement Parsing (Simplified) ---
    def _parse_statement(self) -> Statement:
        if self.cur.kind == TokenKind.LBRACE:
            return self._parse_compound_statement()
        elif self.cur.kind == TokenKind.FOR:
            return self._parse_for_statement()
        elif self.cur.kind == TokenKind.WHILE:
            return self._parse_while_statement()
        elif self.cur.kind == TokenKind.RETURN:
            return self._parse_return_statement()
        else:  # Expression statement
            expr = self._parse_expression()
            self._expect(TokenKind.SEMI)
            return ExpressionStatement(expression=expr)

    def _parse_compound_statement(self) -> CompoundStatement:
        self._expect(TokenKind.LBRACE)
        items: List[Union[Declaration, Statement]] = []
        while self.cur.kind != TokenKind.RBRACE and self.cur.kind != TokenKind.EOF:
            if self.cur.kind in (TokenKind.INT, TokenKind.DOUBLE):
                items.append(self._parse_declaration_statement())
            else:
                items.append(self._parse_statement())
        self._expect(TokenKind.RBRACE)
        return CompoundStatement(block_items=items)

    def _parse_declaration_statement(self) -> Declaration:
        spec = self._parse_type_specifier()
        init_decls: List[InitDeclarator] = []

        first_decl = self._parse_declarator()
        first_init_decl = InitDeclarator(declarator=first_decl)
        if self._match(TokenKind.ASSIGN):
            first_init_decl.initializer = self._parse_initializer()
        init_decls.append(first_init_decl)

        while self._match(TokenKind.COMMA):
            decl = self._parse_declarator()
            init_decl = InitDeclarator(declarator=decl)
            if self._match(TokenKind.ASSIGN):
                init_decl.initializer = self._parse_initializer()
            init_decls.append(init_decl)

        self._expect(TokenKind.SEMI)
        return Declaration(specifier=spec, init_declarators=init_decls)

    def _parse_for_statement(self) -> ForStatement:
        self._expect(TokenKind.FOR)
        self._expect(TokenKind.LPAREN)

        init_val: Optional[Union[Declaration, Expr]] = None  # Renamed from 'init' to avoid clash
        if self.cur.kind != TokenKind.SEMI:
            if self.cur.kind in (TokenKind.INT, TokenKind.DOUBLE):
                spec = self._parse_type_specifier()
                decl = self._parse_declarator()
                initializer_val: Optional[Initializer] = None  # Renamed
                if self._match(TokenKind.ASSIGN):
                    initializer_val = self._parse_initializer()
                init_val = Declaration(specifier=spec,
                                       init_declarators=[InitDeclarator(declarator=decl, initializer=initializer_val)])
            else:
                init_val = self._parse_expression()
        self._expect(TokenKind.SEMI)

        condition_val: Optional[Expr] = None  # Renamed
        if self.cur.kind != TokenKind.SEMI:
            condition_val = self._parse_expression()
        self._expect(TokenKind.SEMI)

        iteration_val: Optional[Expr] = None  # Renamed
        if self.cur.kind != TokenKind.RPAREN:
            iteration_val = self._parse_expression()
        self._expect(TokenKind.RPAREN)

        body_val = self._parse_statement()  # Renamed
        return ForStatement(body=body_val, init=init_val, condition=condition_val, iteration=iteration_val)

    def _parse_while_statement(self) -> WhileStatement:
        self._expect(TokenKind.WHILE)
        self._expect(TokenKind.LPAREN)
        cond = self._parse_expression()
        self._expect(TokenKind.RPAREN)
        body = self._parse_statement()
        return WhileStatement(condition=cond, body=body)

    def _parse_return_statement(self) -> ReturnStatement:
        self._expect(TokenKind.RETURN)
        expr: Optional[Expr] = None
        if self.cur.kind != TokenKind.SEMI:
            expr = self._parse_expression()
        self._expect(TokenKind.SEMI)
        return ReturnStatement(expression=expr)

    # --- Expression Parsing (Simplified Hierarchy) ---
    def _parse_expression(self) -> Expr:
        return self._parse_assignment_expression()

    def _parse_assignment_expression(self) -> Expr:
        lhs = self._parse_conditional_expression()
        assign_ops = [TokenKind.ASSIGN, TokenKind.ADD_ASSIGN, TokenKind.MUL_ASSIGN]
        if self.cur.kind in assign_ops:
            op_token = self.cur;
            self._adv()
            if not isinstance(lhs, (PrimaryExprIdentifier, UnaryExpr, PostfixExprIndex)):
                pass
            rhs = self._parse_assignment_expression()
            return AssignmentExpr(left=lhs, operator=op_token.text, right=rhs)
        return lhs

    def _parse_conditional_expression(self) -> Expr:
        node = self._parse_logical_or_expression()
        if self._match(TokenKind.QUESTION):
            true_expr = self._parse_expression()
            self._expect(TokenKind.COLON)
            false_expr = self._parse_conditional_expression()
            return ConditionalExpr(node, true_expr, false_expr)
        return node

    def _parse_logical_or_expression(self) -> Expr:
        return self._parse_logical_and_expression()

    def _parse_logical_and_expression(self) -> Expr:
        return self._parse_equality_expression()

    def _parse_equality_expression(self) -> Expr:
        node = self._parse_relational_expression()
        if self._match(TokenKind.EQ_OP):  # Only == in example for this level
            node = BinaryOp("==", node, self._parse_relational_expression())
        return node

    def _parse_relational_expression(self) -> Expr:
        node = self._parse_additive_expression()
        if self._match(TokenKind.LT_OP):  # Only < in example for this level
            node = BinaryOp("<", node, self._parse_additive_expression())
        return node

    def _parse_additive_expression(self) -> Expr:
        node = self._parse_multiplicative_expression()
        while self.cur.kind in (TokenKind.PLUS, TokenKind.MINUS):
            op = self.cur.text;
            self._adv()
            node = BinaryOp(op, node, self._parse_multiplicative_expression())
        return node

    def _parse_multiplicative_expression(self) -> Expr:
        node = self._parse_unary_expression()
        # Only '*' for multiplication in example at this precedence
        # `res *= x` is handled by assignment expression.
        # `y * 2` would be handled here if present.
        # `*(values + i)` is unary dereference of an additive expression.
        # The example `res *= x` is `res = res * x`.
        # So, if we parse `res` then `*=`, then `x`, that's assignment.
        # If we have `a * b`, this is the place.
        # The example has `fib(n-1) + fib(n-2)`. No standalone multiplication here.
        # Let's assume for now that if `*` appears, it's multiplicative.
        while self.cur.kind == TokenKind.STAR:
            # This needs to be careful not to consume STAR for dereference if unary is expected.
            # The structure `node = _parse_unary...` then `while self.cur.kind == STAR` is standard.
            op = self.cur.text;
            self._adv()
            node = BinaryOp(op, node, self._parse_unary_expression())
        return node

    def _parse_unary_expression(self) -> Expr:
        if self.cur.kind == TokenKind.INC_OP:
            op = self.cur.text;
            self._adv()
            # Example: ++i (prefix)
            return UnaryExpr(operator=op, operand=self._parse_unary_expression())
        elif self.cur.kind == TokenKind.STAR:
            op = self.cur.text;
            self._adv()
            # Example: *(values + i)
            return UnaryExpr(operator=op, operand=self._parse_unary_expression())
        elif self.cur.kind == TokenKind.PLUS or self.cur.kind == TokenKind.MINUS:
            op = self.cur.text;
            self._adv()
            return UnaryExpr(operator=op, operand=self._parse_unary_expression())
        else:
            return self._parse_postfix_expression()

    def _parse_postfix_expression(self) -> Expr:
        node = self._parse_primary_expression()
        while True:
            if self._match(TokenKind.LBRACK):
                idx_expr = self._parse_expression()
                self._expect(TokenKind.RBRACK)
                node = PostfixExprIndex(expression=node, index_expression=idx_expr)
            elif self._match(TokenKind.LPAREN):
                args: Optional[List[Expr]] = None
                if self.cur.kind != TokenKind.RPAREN:
                    arg_list_node = self._parse_argument_expression_list()
                    args = arg_list_node.expressions
                self._expect(TokenKind.RPAREN)
                node = PostfixExprCall(expression=node, arguments=args)
            elif self.cur.kind == TokenKind.INC_OP:  # i++
                # Check if it's truly postfix (no space/newline, etc.) - simplified here
                op_tok = self.cur;
                self._adv()
                node = PostfixExprIncDec(expression=node, operator=op_tok.text)
            else:
                break
        return node

    def _parse_primary_expression(self) -> Expr:
        if self.cur.kind == TokenKind.IDENT:
            name = self.cur.text;
            self._adv()
            return PrimaryExprIdentifier(name=name)
        elif self.cur.kind == TokenKind.I_CONSTANT:
            val = self.cur.text;
            tok_kind = self.cur.kind;
            self._adv()
            return PrimaryExprConstant(value=val, kind=tok_kind)
        elif self.cur.kind == TokenKind.F_CONSTANT:
            val = self.cur.text;
            tok_kind = self.cur.kind;
            self._adv()
            return PrimaryExprConstant(value=val, kind=tok_kind)
        elif self._match(TokenKind.LPAREN):
            expr = self._parse_expression()
            self._expect(TokenKind.RPAREN)
            return PrimaryExprParenthesized(expression=expr)
        else:
            raise ParserError(f"Unexpected token in primary expression: {self.cur}")

    def _parse_argument_expression_list(self) -> ArgumentExpressionList:
        args = [self._parse_assignment_expression()]
        while self._match(TokenKind.COMMA):
            args.append(self._parse_assignment_expression())
        return ArgumentExpressionList(expressions=args)


def main() -> None:
    with open("input.txt", "r", encoding="utf-8") as f:
        src = f.read()
    try:  
        lexer = Lexer(src)
        tokens = lexer.tokenize()

        parser = Parser(tokens)
        ast = parser.parse()

        print("\n--- AST ---")
        import pprint
        pp = pprint.PrettyPrinter(indent=2, width=120, sort_dicts=False)
        pp.pprint(ast)

    except (RuntimeError, ParserError) as e:
        print(f"\nError: {e}", file=sys.stderr)
        return


if __name__ == "__main__":
    main()
