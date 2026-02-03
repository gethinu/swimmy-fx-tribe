from dataclasses import dataclass
import os


@dataclass
class GatewayConfig:
    api_key: str
    zmq_endpoint: str
    bind_host: str
    bind_port: int

    @classmethod
    def from_env(cls):
        return cls(
            api_key=os.getenv("SWIMMY_MCP_API_KEY", ""),
            zmq_endpoint=os.getenv("SWIMMY_MCP_ZMQ_ENDPOINT", "tcp://localhost:5559"),
            bind_host=os.getenv("SWIMMY_MCP_BIND_HOST", "127.0.0.1"),
            bind_port=int(os.getenv("SWIMMY_MCP_BIND_PORT", "8790")),
        )
