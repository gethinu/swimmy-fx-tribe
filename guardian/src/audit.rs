use serde::{Serialize, Deserialize};
use std::fs::OpenOptions;
use std::io::Write;
use chrono::Utc;

#[derive(Serialize, Deserialize, Debug)]
pub struct AuditEntry {
    pub timestamp: String,
    pub order_summary: String,
    pub decision: String, // "PASS", "VETO", "ADJUST"
    pub reason: String,
    pub risk_metrics: String, // Snapshot of risk state e.g. "DailyPnL: -100, Lot: 0.1"
}

pub struct AuditLogger {
    file_path: String,
}

impl AuditLogger {
    pub fn new(file_path: &str) -> Self {
        AuditLogger {
            file_path: file_path.to_string(),
        }
    }

    pub fn log(&self, order: &str, decision: &str, reason: &str, valid_metrics: &str) {
        let entry = AuditEntry {
            timestamp: Utc::now().to_rfc3339(),
            order_summary: order.to_string(),
            decision: decision.to_string(),
            reason: reason.to_string(),
            risk_metrics: valid_metrics.to_string(),
        };

        if let Ok(json) = serde_json::to_string(&entry) {
            if let Ok(mut file) = OpenOptions::new()
                .create(true)
                .append(true)
                .open(&self.file_path) 
            {
                if let Err(e) = writeln!(file, "{}", json) {
                    eprintln!("Failed to write audit log: {}", e);
                }
            } else {
                eprintln!("Failed to open audit log file: {}", self.file_path);
            }
        }
    }
}
