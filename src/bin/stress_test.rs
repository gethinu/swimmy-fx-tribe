use std::time::Instant;
use serde_json::json;

fn main() {
    println!("🔥 Swimmy Stress Tester (Naval's Load Test)");
    println!("Goal: Flood Brain with 10k+ ticks/sec via ZMQ PUSH.");

    let ctx = zmq::Context::new();
    let socket = ctx.socket(zmq::PUSH).unwrap();
    
    // Brain Binds *:5555, so we Connect to localhost:5555
    let port = std::env::var("SWIMMY_PORT_SENSORY")
        .ok()
        .and_then(|v| v.parse::<u16>().ok())
        .unwrap_or(5555);
    let endpoint = format!("tcp://127.0.0.1:{}", port);
    println!("Connecting to {}...", endpoint);
    socket.connect(&endpoint).expect("Failed to connect to Brain");

    let total_messages = 100_000;
    let start = Instant::now();

    println!("🚀 Starting flood of {} messages...", total_messages);

    for i in 0..total_messages {
        let tick = json!({
            "type": "TICK",
            "symbol": "USDJPY",
            "bid": 150.0 + (i as f64 * 0.0001),
            "ask": 150.01 + (i as f64 * 0.0001),
            "timestamp": chrono::Utc::now().timestamp()
        });

        let msg = tick.to_string();
        socket.send(&msg, 0).unwrap();

        if i % 10000 == 0 {
            print!(".");
            use std::io::Write;
            std::io::stdout().flush().unwrap();
        }
    }

    let duration = start.elapsed();
    let seconds = duration.as_secs_f64();
    let rate = total_messages as f64 / seconds;

    println!("\n✅ Done!");
    println!("Time: {:.4}s", seconds);
    println!("Rate: {:.2} ticks/sec", rate);

    if rate > 10_000.0 {
        println!("🏆 PASS: Rate > 10k/sec");
    } else {
        println!("⚠️ WARN: Rate < 10k/sec");
    }
}
