import zmq, sys, time, re
ctx = zmq.Context()
push = ctx.socket(zmq.PUSH); push.connect("tcp://127.0.0.1:6580")
pull = ctx.socket(zmq.PULL); pull.bind("tcp://*:6581")
time.sleep(1.5)
def req(name, itype, extra):
    return ('((action . "BACKTEST") (request_id . "%s") '
            '(strategy . ((name . "%s") (sma_short . 55) (sma_long . 110) (sl . 0.005) (tp . 0.005) '
            '(volume . 0.01) (indicator_type . %s) (timeframe . 240)%s)) '
            '(candles_file . "data/historical/EURUSD_M1.csv"))' % (name, name, itype, extra))
push.send_string(req("K","keltner"," (band_mult . 2.0) (atr_period . 14) (atr_barrier_sl . 2.0) (atr_barrier_tp . 2.0)"))
push.send_string(req("S","sma",""))
got = {}
poller = zmq.Poller(); poller.register(pull, zmq.POLLIN)
deadline = time.time() + 300
while len(got) < 2 and time.time() < deadline:
    if poller.poll(3000):
        m = pull.recv_string()
        nm = re.search(r'strategy_name . "([^"]+)"', m); tr = re.search(r'trades . (\d+)', m); pf = re.search(r'profit_factor . ([0-9.eE+-]+)', m)
        if nm:
            got[nm.group(1)] = (tr.group(1) if tr else "?", pf.group(1) if pf else "?")
            print("RESULT", nm.group(1), "trades=", (tr.group(1) if tr else "?"), "pf=", (pf.group(1) if pf else "?"))
print("SMOKE keltner=%s sma=%s DISTINCT=%s" % (got.get("K"), got.get("S"), got.get("K")!=got.get("S")))
