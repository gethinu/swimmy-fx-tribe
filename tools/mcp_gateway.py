import uuid
from pathlib import Path
import sys

from mcp_gateway_config import GatewayConfig
from mcp_gateway_zmq import ZmqPublisher


def _resolve_base_dir() -> Path:
    here = Path(__file__).resolve()
    for parent in [here] + list(here.parents):
        if (parent / "swimmy.asd").exists() or (parent / "run.sh").exists():
            return parent
    return here.parent


BASE_DIR = _resolve_base_dir()
sys.path.insert(0, str(BASE_DIR / "src" / "python"))

from sexp_serialize import sexp_serialize


_publisher = None


def _get_publisher(endpoint: str) -> ZmqPublisher:
    global _publisher
    if _publisher is None:
        _publisher = ZmqPublisher(endpoint)
    return _publisher


def handle_trade_submit(_payload):
    return {"status": 403, "error": "trade-capable disabled"}


def handle_backtest_submit(payload, endpoint: str | None = None):
    cfg = GatewayConfig.from_env()
    endpoint = endpoint or cfg.zmq_endpoint
    request_id = payload.get("request_id") or str(uuid.uuid4())
    sexp = sexp_serialize({"action": "BACKTEST", **payload, "request_id": request_id})
    _get_publisher(endpoint).send(sexp)
    return {"status": 202, "request_id": request_id}
