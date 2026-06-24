"""Minimal reader for Swimmy strategy files (``#S(STRATEGY :KEY value ...)``).

These files are SBCL ``print``-ed defstruct instances. We do not need a full
Common Lisp reader — just enough to recover the scalar slot values (metrics,
symbol, ranks) and to keep the structural slots (ENTRY/EXIT/INDICATORS) as a
re-serialised Lisp-ish string for human reference.

The reader is deliberately conservative: it never evaluates anything (no
``read-eval``), it only tokenises and walks the tree.
"""

from __future__ import annotations

from typing import Any


# ---- tokeniser -------------------------------------------------------------

def _tokenize(text: str):
    toks = []
    i, n = 0, len(text)
    while i < n:
        c = text[i]
        if c.isspace():
            i += 1
            continue
        if c == '"':
            j = i + 1
            buf = []
            while j < n and text[j] != '"':
                if text[j] == '\\' and j + 1 < n:
                    buf.append(text[j + 1])
                    j += 2
                    continue
                buf.append(text[j])
                j += 1
            toks.append(("str", "".join(buf)))
            i = j + 1
            continue
        if c == '(':
            toks.append(("lp", None))
            i += 1
            continue
        if c == ')':
            toks.append(("rp", None))
            i += 1
            continue
        if c == '#':
            # read the dispatch token, e.g. "#S" (the following "(" is separate)
            j = i + 1
            while j < n and not text[j].isspace() and text[j] not in '()"':
                j += 1
            toks.append(("hash", text[i:j]))
            i = j
            continue
        # bare atom (symbol / number / keyword)
        j = i
        while j < n and not text[j].isspace() and text[j] not in '()"':
            j += 1
        toks.append(("atom", text[i:j]))
        i = j
    return toks


# ---- atom coercion ---------------------------------------------------------

def _coerce_atom(tok: str) -> Any:
    up = tok.upper()
    if up == "NIL":
        return None
    if up == "T":
        return True
    # integer
    try:
        return int(tok)
    except ValueError:
        pass
    # float (handle common-lisp style like 1.0d0 / 1.0f0 defensively)
    norm = tok.replace("d", "e").replace("D", "e").replace("f", "e").replace("F", "e")
    try:
        return float(norm)
    except ValueError:
        pass
    return tok  # keep keywords / symbols verbatim


# ---- parser ----------------------------------------------------------------

class _Reader:
    def __init__(self, toks):
        self.toks = toks
        self.pos = 0

    def _peek(self):
        return self.toks[self.pos] if self.pos < len(self.toks) else ("eof", None)

    def read(self) -> Any:
        kind, val = self.toks[self.pos]
        self.pos += 1
        if kind == "hash":
            # a struct/dispatch: the following form is the body list
            body = self.read()
            return ("#struct", val, body)
        if kind == "lp":
            items = []
            while self._peek()[0] != "rp":
                if self._peek()[0] == "eof":
                    raise ValueError("unterminated list")
                items.append(self.read())
            self.pos += 1  # consume rp
            return items
        if kind == "str":
            return val
        if kind == "atom":
            return _coerce_atom(val)
        raise ValueError(f"unexpected token: {kind}")


def serialize(form: Any) -> str:
    """Re-serialise a parsed form back to a compact Lisp-ish string."""
    if form is None:
        return "NIL"
    if form is True:
        return "T"
    if isinstance(form, str):
        return form
    if isinstance(form, float):
        return repr(form)
    if isinstance(form, int):
        return str(form)
    if isinstance(form, tuple) and form and form[0] == "#struct":
        return f"#{form[1][1:] if form[1].startswith('#') else form[1]}({serialize(form[2])})"
    if isinstance(form, list):
        return "(" + " ".join(serialize(x) for x in form) + ")"
    return str(form)


def parse_strategy(text: str) -> dict:
    """Parse a ``#S(STRATEGY :KEY value ...)`` file into a plain dict.

    Keys are lower-cased slot names without the leading colon
    (e.g. ``:PROFIT-FACTOR`` -> ``profit-factor``). Structural slots are kept
    as their parsed form; scalars are coerced to int/float/str/bool/None.
    Raises ``ValueError`` if the text is not a struct form.
    """
    reader = _Reader(_tokenize(text))
    form = reader.read()
    if not (isinstance(form, tuple) and form and form[0] == "#struct"):
        raise ValueError("not a #S(...) struct form")
    body = form[2]
    if not isinstance(body, list) or not body:
        raise ValueError("empty struct body")
    # body[0] is the struct type name (STRATEGY); rest are :KEY value pairs
    out: dict = {"_struct": str(body[0])}
    items = body[1:]
    idx = 0
    while idx < len(items) - 1:
        key = items[idx]
        val = items[idx + 1]
        if isinstance(key, str) and key.startswith(":"):
            out[key[1:].lower()] = val
            idx += 2
        else:
            idx += 1  # skip stray token, stay robust
    return out
