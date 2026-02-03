"""Minimal S-expression parser for Swimmy local storage."""

from __future__ import annotations


class SexpParseError(ValueError):
    pass


def _is_delim(ch: str | None) -> bool:
    if ch is None:
        return True
    return ch.isspace() or ch in "()"


def _tokenize(text: str):
    tokens = []
    i = 0
    n = len(text)
    while i < n:
        ch = text[i]
        if ch.isspace():
            i += 1
            continue
        if ch == "(":
            tokens.append(("LPAREN", ch))
            i += 1
            continue
        if ch == ")":
            tokens.append(("RPAREN", ch))
            i += 1
            continue
        if ch == '"':
            i += 1
            buf = []
            while i < n:
                c = text[i]
                if c == "\\":
                    if i + 1 >= n:
                        raise SexpParseError("Invalid escape")
                    buf.append(text[i + 1])
                    i += 2
                    continue
                if c == '"':
                    i += 1
                    break
                buf.append(c)
                i += 1
            else:
                raise SexpParseError("Unterminated string")
            tokens.append(("STRING", "".join(buf)))
            continue
        if ch == "." and _is_delim(text[i - 1] if i > 0 else None) and _is_delim(text[i + 1] if i + 1 < n else None):
            tokens.append(("DOT", ch))
            i += 1
            continue
        start = i
        while i < n and not text[i].isspace() and text[i] not in "()":
            i += 1
        token = text[start:i]
        tokens.append(("SYMBOL", token))
    return tokens


class _Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def _peek(self):
        if self.pos >= len(self.tokens):
            return None
        return self.tokens[self.pos]

    def _next(self, expected=None):
        tok = self._peek()
        if tok is None:
            raise SexpParseError("Unexpected EOF")
        if expected and tok[0] != expected:
            raise SexpParseError(f"Expected {expected}, got {tok[0]}")
        self.pos += 1
        return tok

    def parse(self):
        tok = self._peek()
        if tok is None:
            raise SexpParseError("Empty input")
        if tok[0] == "LPAREN":
            return self._parse_list()
        if tok[0] == "STRING":
            self._next("STRING")
            return tok[1]
        if tok[0] == "SYMBOL":
            self._next("SYMBOL")
            return _atom(tok[1])
        raise SexpParseError(f"Unexpected token {tok[0]}")

    def _parse_list(self):
        self._next("LPAREN")
        tok = self._peek()
        if tok and tok[0] == "RPAREN":
            self._next("RPAREN")
            return []
        first = self.parse()
        tok = self._peek()
        if tok and tok[0] == "DOT":
            self._next("DOT")
            value = self.parse()
            self._next("RPAREN")
            return (first, value)
        items = [first]
        while True:
            tok = self._peek()
            if tok is None:
                raise SexpParseError("Unterminated list")
            if tok[0] == "RPAREN":
                self._next("RPAREN")
                break
            items.append(self.parse())
        return items


def _atom(token: str):
    low = token.lower()
    if low in ("nil", "null"):
        return None
    if low in ("t", "true", "#t"):
        return True
    if low in ("#f", "false"):
        return False
    try:
        if any(ch in token for ch in (".", "e", "E")):
            return float(token)
        return int(token)
    except ValueError:
        return token


def parse_sexp(text: str):
    tokens = _tokenize(text)
    parser = _Parser(tokens)
    result = parser.parse()
    return result


def _normalize_key(key):
    if isinstance(key, str):
        if key.startswith(":"):
            key = key[1:]
        return key.replace("-", "_").lower()
    return str(key)


def _sexp_to_python(obj):
    if isinstance(obj, tuple) and len(obj) == 2:
        return (_normalize_key(obj[0]), _sexp_to_python(obj[1]))
    if isinstance(obj, dict):
        return {k: _sexp_to_python(v) for k, v in obj.items()}
    if isinstance(obj, list):
        if obj and all(isinstance(x, tuple) and len(x) == 2 for x in obj):
            return {k: _sexp_to_python(v) for k, v in (_sexp_to_python(x) for x in obj)}
        if len(obj) % 2 == 0 and all(isinstance(obj[i], str) for i in range(0, len(obj), 2)):
            return {
                _normalize_key(obj[i]): _sexp_to_python(obj[i + 1])
                for i in range(0, len(obj), 2)
            }
        return [_sexp_to_python(x) for x in obj]
    return obj


def parse_sexp_alist(text: str):
    data = _sexp_to_python(parse_sexp(text))
    if not isinstance(data, dict):
        raise SexpParseError("Expected alist at top level")
    return data


def parse_sexp_list(text: str):
    data = _sexp_to_python(parse_sexp(text))
    if isinstance(data, dict) and isinstance(data.get("entries"), list):
        return data["entries"]
    if not isinstance(data, list):
        raise SexpParseError("Expected list at top level")
    return data


def load_sexp_alist(path: str):
    if not os.path.exists(path):
        return {}
    with open(path, "r", encoding="utf-8") as f:
        return parse_sexp_alist(f.read())


def load_sexp_list(path: str):
    if not os.path.exists(path):
        return []
    with open(path, "r", encoding="utf-8") as f:
        return parse_sexp_list(f.read())


import os  # placed at end to keep top-of-file minimal
