"""Swimmy 蠱毒 (Gu-poison) strategy-factory export toolkit.

This package turns the *output* of the Swimmy strategy factory — the surviving
strategies in ``data/library/`` — into a trustworthy, machine-readable feed that
a downstream "bundle-of-edge" consumer can ingest.

The factory continuously breeds and culls strategies (the 蠱毒 / Gu-poison jar:
many candidates fight, the strongest survive). Its weakness is that "survived the
jar" is NOT the same as "deployable edge": most survivors are in-sample (IS) only
and have never cleared out-of-sample (OOS) / CPCV validation. This toolkit makes
that distinction explicit so nobody mistakes a marginal IS survivor for a verified
edge.

Modules:
    sexpr        - minimal reader for the ``#S(STRATEGY ...)`` Lisp struct files
    honest_gate  - risk/honesty gate that classifies a candidate (importable + CLI)
    export       - parses the library, runs the gate, emits JSON + Markdown feed
"""
