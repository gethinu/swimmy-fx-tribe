#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SexpRoute {
    ForwardToMt5,
    HandleInternally,
    Unknown,
}

fn normalize_sexp_key(raw: &str) -> String {
    let mut key = raw.trim();
    if let Some((_, tail)) = key.rsplit_once("::") {
        key = tail;
    } else if let Some((_, tail)) = key.rsplit_once(':') {
        key = tail;
    }
    key.trim_start_matches(':').to_ascii_lowercase()
}

fn lexpr_key_to_string(val: &lexpr::Value) -> Option<String> {
    match val {
        lexpr::Value::Symbol(s) => Some(normalize_sexp_key(&s.to_string())),
        lexpr::Value::Keyword(s) => Some(normalize_sexp_key(&s.to_string())),
        lexpr::Value::String(s) => Some(normalize_sexp_key(s)),
        _ => None,
    }
}

fn lexpr_to_string(val: &lexpr::Value) -> Option<String> {
    match val {
        lexpr::Value::String(s) => Some(s.to_string()),
        lexpr::Value::Symbol(s) => Some(s.to_string()),
        lexpr::Value::Keyword(s) => Some(s.to_string()),
        _ => None,
    }
}

fn lexpr_alist_to_map(
    val: &lexpr::Value,
) -> Result<std::collections::HashMap<String, lexpr::Value>, String> {
    let mut out = std::collections::HashMap::new();
    if matches!(val, lexpr::Value::Nil) {
        return Ok(out);
    }
    let list = val
        .to_vec()
        .ok_or_else(|| "expected list for alist".to_string())?;
    for item in list {
        if let Some((car, cdr)) = item.as_pair() {
            if let Some(key) = lexpr_key_to_string(car) {
                out.insert(key, cdr.clone());
            }
        } else {
            return Err("expected cons cell in alist".to_string());
        }
    }
    Ok(out)
}

pub fn route_sexp_message(message: &str) -> SexpRoute {
    let msg = message.trim();
    if !msg.starts_with('(') {
        return SexpRoute::Unknown;
    }

    let val: lexpr::Value = match lexpr::from_str(msg) {
        Ok(v) => v,
        Err(_) => return SexpRoute::Unknown,
    };
    let map = match lexpr_alist_to_map(&val) {
        Ok(m) => m,
        Err(_) => return SexpRoute::Unknown,
    };

    if let Some(t) = map.get("type").and_then(lexpr_to_string) {
        match t.as_str() {
            // MT5 execution protocol (S-expression)
            "ORDER_OPEN" | "CLOSE" | "CLOSE_SHORT_TF" | "REQ_HISTORY" | "GET_POSITIONS"
            | "GET_SWAP" | "HEARTBEAT" => SexpRoute::ForwardToMt5,
            _ => SexpRoute::Unknown,
        }
    } else if let Some(a) = map.get("action").and_then(lexpr_to_string) {
        match a.as_str() {
            // Internal guardian/brain protocol (S-expression)
            "BACKTEST" | "CPCV_VALIDATE" | "CACHE_DATA" | "CHECK_CLONE" | "UPDATE_CACHE" | "TRAIN"
            | "EVOLVE" | "PREDICT" => SexpRoute::HandleInternally,
            _ => SexpRoute::Unknown,
        }
    } else {
        SexpRoute::Unknown
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn order_open_routes_to_mt5() {
        let msg = r#"((type . "ORDER_OPEN") (instrument . "EURUSD") (side . "BUY") (lot . 0.01) (sl . 1.0) (tp . 2.0))"#;
        assert_eq!(route_sexp_message(msg), SexpRoute::ForwardToMt5);
    }

    #[test]
    fn heartbeat_routes_to_mt5() {
        let msg = r#"((type . "HEARTBEAT") (source . "BRAIN") (status . "OK"))"#;
        assert_eq!(route_sexp_message(msg), SexpRoute::ForwardToMt5);
    }

    #[test]
    fn req_history_routes_to_mt5() {
        let msg = r#"((type . "REQ_HISTORY") (symbol . "USDJPY") (tf . "M1") (count . 2000))"#;
        assert_eq!(route_sexp_message(msg), SexpRoute::ForwardToMt5);
    }

    #[test]
    fn get_positions_routes_to_mt5() {
        let msg = r#"((type . "GET_POSITIONS"))"#;
        assert_eq!(route_sexp_message(msg), SexpRoute::ForwardToMt5);
    }

    #[test]
    fn backtest_action_is_internal() {
        let msg = r#"((action . "BACKTEST") (symbol . "USDJPY"))"#;
        assert_eq!(route_sexp_message(msg), SexpRoute::HandleInternally);
    }
}
