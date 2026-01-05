#!/bin/bash
# Test heartbeat notification
cd /home/swimmy/swimmy
sbcl --noinform --non-interactive --load brain.lisp --eval "(heartbeat-now)" --quit 2>&1 | tail -20
