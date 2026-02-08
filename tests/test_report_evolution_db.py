import sqlite3
from pathlib import Path
import sys

ROOT = Path(__file__).resolve().parents[1]
TOOLS = ROOT / "tools"
if str(TOOLS) not in sys.path:
    sys.path.insert(0, str(TOOLS))

import report_evolution as re


def test_get_db_rank_counts():
    tmp = ROOT / "data" / "memory" / "test-report-evolution.db"
    if tmp.exists():
        tmp.unlink()
    conn = sqlite3.connect(tmp)
    try:
        cur = conn.cursor()
        cur.execute("CREATE TABLE strategies (rank TEXT)")
        cur.executemany(
            "INSERT INTO strategies (rank) VALUES (?)",
            [(":S",), (":S",), (":A",), (":B",), (":GRAVEYARD",), (None,)],
        )
        conn.commit()
    finally:
        conn.close()

    counts = re.get_db_rank_counts(tmp)
    assert counts["s"] == 2
    assert counts["a"] == 1
    assert counts["b"] == 1
    assert counts["graveyard"] == 1
    assert counts["unranked"] == 1
    assert counts["active"] == 4
