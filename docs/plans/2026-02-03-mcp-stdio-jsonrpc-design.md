# MCP stdio JSON-RPC Server Design

**Date:** 2026-02-03
**Scope:** MCP stdio server (JSON-RPC 2.0) for read-only + backtest-exec. Trade remains disabled.

## Background
We already have a local MCP gateway core that converts JSON inputs into S-expression and publishes to ZMQ 5559. What is missing is an MCP-standard stdio server that can be launched on-demand by an MCP host. The server should keep the internal S-expression contract intact while exposing a strict JSON-RPC 2.0 interface on stdio.

## Goals
- Provide MCP-standard JSON-RPC 2.0 over stdio (Content-Length framing).
- Keep internal contracts: S-expression + ZMQ 5559 unchanged.
- Support methods: health.ping, system.status, system.metrics, backtest.submit, backtest.status.
- Enforce API key authentication at the stdio boundary.
- Keep behavior deterministic and easy to test.

## Non-goals
- Live trading or trade-capable execution.
- HTTP/WebSocket server mode.
- Request-id correlation with backtest results (future work).
- Replacing internal S-expression storage with JSON.

## Architecture Summary
```
MCP Host
  -> stdio JSON-RPC (mcp_stdio_server.py)
  -> mcp_gateway.py (handlers)
  -> ZMQ PUB tcp://localhost:5559 (Guardian SUB)
  -> status/metrics readers (files)
```

## Components
1. **stdio JSON-RPC Server** (`tools/mcp_stdio_server.py`)
   - Parses Content-Length framed messages.
   - Rejects non-JSON-RPC input (Invalid Request).
   - Dispatches by method name.

2. **Gateway Core** (`tools/mcp_gateway.py`)
   - `handle_backtest_submit` converts JSON to S-expression and publishes to ZMQ.
   - `handle_trade_submit` returns 403 (disabled).

3. **Auth + Config** (`tools/mcp_gateway_config.py`)
   - Reads `SWIMMY_MCP_API_KEY` and `SWIMMY_MCP_ZMQ_ENDPOINT`.
   - Validates `params.api_key` on every request.

4. **Status / Metrics Readers**
   - `system.status`: `/tmp/swimmy_status` and `data/reports/backtest_status.txt`.
   - `system.metrics`: `data/system_metrics.sexp` via `src/python/sexp_utils.py`.

5. **Logging**
   - JSONL log (request_id, method, duration_ms, status, error).

## Methods
- `health.ping` -> `{ "status": "ok" }`
- `system.status` -> key/value summary from status files.
- `system.metrics` -> raw metrics from `system_metrics.sexp`.
- `backtest.submit` -> `{ "request_id": "..." }` (202-equivalent).
- `backtest.status` -> latest summary from `backtest_status.txt` with `mode="latest"`.
- `trade.submit` -> error `{ code: -32003, message: "trade-capable disabled" }`.

## Data Flow (backtest.submit)
1. Validate JSON-RPC request and API key.
2. Validate params (symbol/timeframe/candles_file, etc).
3. Generate request_id.
4. Convert to S-expression and publish to ZMQ 5559.
5. Return request_id in JSON-RPC result.

## Error Handling
- `-32600` Invalid Request (non JSON-RPC or invalid framing).
- `-32601` Method not found.
- `-32602` Invalid params.
- `-32001` Unauthorized (API key mismatch).
- `-32603` Internal error (details in logs only).

Missing status/metrics files return empty objects, not errors (availability first).

## Security
- API key required via `params.api_key`.
- No network binding (stdio only).
- Trade methods always rejected.

## Testing Strategy
1. **Unit**: framing parser, JSON-RPC validation, auth failure cases.
2. **Contract**: 403 for trade, request_id for backtest, `mode=latest` for backtest.status.
3. **Integration Smoke**: ZMQ publisher stub to confirm S-expression output.

## Open Questions
- How to correlate `request_id` with backtest results in the status file.
- Whether to add schema versioning for `system_metrics.sexp` responses.
