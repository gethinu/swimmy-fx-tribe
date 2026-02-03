import tempfile
from pathlib import Path
from mcp_gateway_observer import read_status_file


def main():
    with tempfile.TemporaryDirectory() as d:
        p = Path(d) / "swimmy_status"
        p.write_text("TIME: now\nTICKS: 1\n", encoding="utf-8")
        data = read_status_file(str(p))
        assert data["TIME"] == "now"
        assert data["TICKS"] == "1"


if __name__ == "__main__":
    main()
