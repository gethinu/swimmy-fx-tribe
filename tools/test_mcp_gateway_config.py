import os
from mcp_gateway_config import GatewayConfig


def main():
    os.environ["SWIMMY_MCP_API_KEY"] = "k"
    cfg = GatewayConfig.from_env()
    assert cfg.api_key == "k"
    assert cfg.zmq_endpoint == "tcp://localhost:5559"
    assert cfg.bind_host == "127.0.0.1"


if __name__ == "__main__":
    main()
