import re
import enum

token_re = re.compile(r"\s*(?:(\d+)|([^\W\d_](?:\w*))|(.))")


class TokenType(enum.Enum):
    NUMBER = enum.auto()
    NAME = enum.auto()
    OPERATOR = enum.auto()
    EOF = enum.auto()


def tokenize(expr):
    for num, ident, op in token_re.findall(expr):
        if num:
            yield (TokenType.NUMBER, int(num))
        elif ident:
            yield (TokenType.NAME, ident)
        else:
            yield (TokenType.OPERATOR, op)


def prefix(token):
    match token:
        case (TokenType.NAME, "if"):
            def parse_if(p):
                cond = p.expression(0)
                p.match((TokenType.NAME, "then"))
                tr = p.expression(0)
                p.match((TokenType.NAME, "else"))
                fl = p.expression(0)
                return ("if", cond, tr, fl)

            return parse_if

        case (TokenType.OPERATOR, "("):
            def parse_paren(p):
                expr = p.expression(0)
                p.match((TokenType.OPERATOR, ")"))
                return expr

            return parse_paren

        case (TokenType.NUMBER, value) | (TokenType.NAME, value):
            return lambda _: value

def infix(token):
    match token:
        case (TokenType.OPERATOR, "?"):
            def parse_if(left, p):
                tr = p.expression(0)
                p.match((TokenType.OPERATOR, ":"))
                fl = p.expression(0)
                return ("if", left, tr, fl)
            return 10, parse_if

        case (TokenType.OPERATOR, "+"):
            return 10, lambda left, p: ("+", left, p.expression(10))

        case (TokenType.OPERATOR, "*"):
            return 20, lambda left, p: ("*", left, p.expression(20))

        case (TokenType.OPERATOR, "("):
            def parse_call(left, p):
                args = []

                while p.peek() != (TokenType.OPERATOR, ")"):
                    args.append(p.expression(0))

                    if p.peek() == (TokenType.OPERATOR, ","):
                        p.pop()

                p.match((TokenType.OPERATOR, ")"))

                return ("call", left, args)

            return 30, parse_call

        case (TokenType.EOF, _) | _:
            return 0, None

class Parser:
    def __init__(self, prefix, infix, tokens):
        self.prefix = prefix
        self.infix = infix
        self.tokens = tokens
        self.prev = None

    def pop(self):
        r = self.peek()
        self.prev = None
        return r

    def match(self, token):
        r = self.pop()
        if r != token:
            raise Exception(f"Expected {token}, got {r}")

    def peek(self):
        if self.prev is None:
            self.prev = next(self.tokens, (TokenType.EOF, ""))
        return self.prev

    def expression(self, rbp=0):
        left = self.prefix(self.pop())(self)

        lbp, parse = self.infix(self.peek())
        while rbp < lbp:
            self.pop()
            left = parse(left, self)
            lbp, parse = self.infix(self.peek())

        return left

