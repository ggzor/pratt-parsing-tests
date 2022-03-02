import pytest
from pratt import *


def test_tokenize():
    assert list(tokenize("3 + 4")) == [
        (TokenType.NUMBER, 3),
        (TokenType.OPERATOR, "+"),
        (TokenType.NUMBER, 4),
    ]


@pytest.mark.parametrize(
    "inp,exp",
    [
        ("3 + 4", ("+", 3, 4)),
        ("a + b", ("+", "a", "b")),
        ("3 + 2 * 4", ("+", 3, ("*", 2, 4))),
        ("2 * 4 + 3", ("+", ("*", 2, 4), 3)),
        ("a ? b : c", ("if", "a", "b", "c")),
        ("a ? b ? c : d : e", ("if", "a", ("if", "b", "c", "d"), "e")),
        ("if a then b else c", ("if", "a", "b", "c")),
        ("(a + b) * c", ("*", ("+", "a", "b"), "c")),
        ("a() + b", ("+", ("call", "a", []), "b")),
        ("b * a(1, 2)", ("*", "b", ("call", "a", [1, 2]))),
        (
            "a + b(c * (d + e), f)()",
            ("+", "a", ("call", ("call", "b", [("*", "c", ("+", "d", "e")), "f"]), [])),
        ),
    ],
)
def test_parse(inp, exp):
    assert Parser(prefix, infix, tokenize(inp)).expression() == exp
