# Swimmy 蠱毒 Strategy Feed (CATALOG)

_Generated 2026-06-24T15:55:20Z · schema `swimmy.tribe.feed/v1`_

This catalogue is the **output of the Swimmy strategy factory**: strategies that survived breeding + culling, scored by an honest promotion gate. **"Survived the jar" is not "verified edge."** Read the verdict column.

## Summary

- Total survivors parsed: **168** (parse errors: 0)
- ✅ PASS: **0**  · 🟡 PROVISIONAL: **61**  · ❌ REJECT: **107**

**Gate** (DD-budget 12%, KPI 3%/mo, min trades 200, min PF 1.1, min Sharpe 0.1):

- **PASS** — clears sample-size + PF + has OOS/CPCV validation. Deployable candidate.
- **PROVISIONAL** — clears hard floors but is IS-only / thin. Needs forward proof before live.
- **REJECT** — fails a hard floor (under-sampled, PF<floor, or fails DD-budget).

## 🟡 PROVISIONAL (61)

| # | Name | Lib | Sym | TF | PF | Sharpe | Trades | OOS | Score |
|---|------|-----|-----|----|----|--------|--------|-----|-------|
| 1 | Bred-Bred--639-Gen48-N3981138750-17 | B | USDJPY | 36 | 1.177 | 0.393 | 5666 | no | 56.093 |
| 2 | Bred-Bred--895-Gen58-N3980554301-559 | B | USDJPY | 36 | 1.198 | 0.348 | 4095 | no | 55.225 |
| 3 | Bred-Bred--315-Gen52-N3980554313-591 | B | USDJPY | 36 | 1.183 | 0.357 | 5032 | no | 54.702 |
| 4 | Bred-Bred--158-Gen53-N3981004881-25 | B | USDJPY | 36 | 1.180 | 0.350 | 5347 | no | 54.152 |
| 5 | Bred-Bred--890-Gen55-N3980554341-651 | B | USDJPY | 36 | 1.173 | 0.354 | 5553 | no | 53.741 |
| 6 | Bred-Bred--271-Gen56-N3980554306-573 | B | USDJPY | 30 | 1.163 | 0.352 | 5594 | no | 52.707 |
| 7 | Bred-Bred--372-Gen56-N3980554212-364 | B | USDJPY | 36 | 1.172 | 0.340 | 5045 | no | 52.647 |
| 8 | Bred-Bred--519-Gen59-N3981053491-290 | B | USDJPY | 15 | 1.160 | 0.342 | 6220 | no | 52.06 |
| 9 | Hunted-H12-VWAPVR-50-150-USDJPY | B | USDJPY | 720 | 1.333 | 0.120 | 215 | no | 50.955 |
| 10 | Perfect-Order-SMA | LEGEND | USDJPY | 30 | 1.150 | 0.333 | 5415 | no | 50.254 |
| 11 | Bred-Bred--801-Gen61-N3981055033-20 | B | USDJPY | 15 | 1.154 | 0.315 | 6060 | no | 50.091 |
| 12 | Bred-Bred--716-Gen53-N3981004335-9 | B | USDJPY | 36 | 1.149 | 0.312 | 5327 | no | 49.187 |
| 13 | Bred-Bred--15-Gen47-N3980554228-396 | B | USDJPY | 36 | 1.146 | 0.321 | 4916 | no | 49.115 |
| 14 | Bred-Bred--135-Gen52-N3980554234-406 | B | USDJPY | 36 | 1.165 | 0.304 | 2781 | no | 48.864 |
| 15 | Bred-Bred--401-Gen57-N3981004890-34 | B | USDJPY | 30 | 1.141 | 0.320 | 5584 | no | 48.819 |
| 16 | Bred-Bred--311-Gen53-N3980553894-2 | B | USDJPY | 15 | 1.138 | 0.317 | 6016 | no | 48.55 |
| 17 | Bred-Bred--844-Gen54-N3980554309-582 | B | USDJPY | 15 | 1.136 | 0.315 | 6247 | no | 48.346 |
| 18 | Bred-Bred--820-Gen53-N3981013859-22 | B | USDJPY | 36 | 1.151 | 0.296 | 4612 | no | 48.215 |
| 19 | Bred-Bred--319-Gen57-N3981004897-74 | B | USDJPY | 30 | 1.135 | 0.312 | 5356 | no | 47.739 |
| 20 | Bred-Bred--753-Gen54-N3980554265-479 | B | USDJPY | 15 | 1.135 | 0.305 | 5904 | no | 47.577 |
| 21 | Bred-Bred--215-Gen56-N3980554068-264 | B | USDJPY | 15 | 1.133 | 0.310 | 5320 | no | 47.45 |
| 22 | Bred-Bred--33-Gen51-N3980554314-596 | B | USDJPY | 30 | 1.133 | 0.305 | 5970 | no | 47.444 |
| 23 | Bred-Bred--227-Gen56-N3981091527-165 | B | USDJPY | 36 | 1.149 | 0.281 | 4951 | no | 47.439 |
| 24 | Bred-Bred--685-Gen58-N3980554345-663 | B | USDJPY | 15 | 1.134 | 0.296 | 6472 | no | 47.233 |
| 25 | Bred-Bred--423-Gen54-N3981055033-13 | B | USDJPY | 36 | 1.133 | 0.299 | 5640 | no | 46.961 |
| 26 | Bred-Bred--623-Gen53-N3981004894-59 | B | USDJPY | 36 | 1.135 | 0.291 | 5478 | no | 46.721 |
| 27 | Bred-Bred--955-Gen58-N3981005301-13 | B | USDJPY | 15 | 1.128 | 0.294 | 5106 | no | 46.03 |
| 28 | Bred-Bred--43-Gen52-N3980553920-52 | B | USDJPY | 30 | 1.144 | 0.277 | 2923 | no | 45.614 |
| 29 | Bred-Bred--162-Gen60-N3981053491-293 | B | USDJPY | 20 | 1.137 | 0.260 | 4872 | no | 45.088 |
| 30 | Bred-Bred--479-Gen53-N3981053354-275 | B | USDJPY | 36 | 1.121 | 0.274 | 5664 | no | 44.598 |
| 31 | Bred-Bred--436-Gen57-N3980554249-445 | B | USDJPY | 15 | 1.118 | 0.279 | 5142 | no | 44.297 |
| 32 | Bred-Tripl-439-Gen1-N3981448858-451 | B | USDJPY | 60 | 1.111 | 0.266 | 7062 | no | 43.645 |
| 33 | Bred-Bred--855-Gen32-N3981140689-162 | B | USDJPY | 60 | 1.158 | 0.232 | 1621 | no | 43.444 |
| 34 | Bred-Bred--386-Gen36-N3981004925-197 | B | USDJPY | 20 | 1.116 | 0.273 | 4449 | no | 43.429 |
| 35 | Bred-Bred--953-Gen36-N3981489799-180 | B | USDJPY | 45 | 1.104 | 0.269 | 6348 | no | 42.876 |
| 36 | Simple-Momentum-Sync | LEGEND | USDJPY | 30 | 1.106 | 0.265 | 5864 | no | 42.694 |
| 37 | Bred-Bred--626-Gen53-N3981004891-39 | B | USDJPY | 30 | 1.126 | 0.251 | 2997 | no | 42.479 |
| 38 | Bred-Bred--323-Gen31-N3981140990-257 | B | USDJPY | 20 | 1.109 | 0.255 | 4773 | no | 42.018 |
| 39 | Bred-Bred--70-Gen60-N3981140036-25 | B | USDJPY | 90 | 1.156 | 0.208 | 1281 | no | 41.527 |
| 40 | Bred-Bred--307-Gen54-N3980554283-521 | B | USDJPY | 90 | 1.123 | 0.221 | 3991 | no | 41.375 |
| 41 | Bred-Bred--159-Gen54-N3981013878-133 | B | USDJPY | 20 | 1.102 | 0.246 | 4963 | no | 41.014 |
| 42 | Bred-MACD--890-Gen1-N3981449038-520 | B | USDJPY | 240 | 1.122 | 0.223 | 3011 | no | 40.76 |
| 43 | Bred-Bred--227-Gen36-N3981094382-96 | B | USDJPY | 20 | 1.103 | 0.237 | 4448 | no | 40.417 |
| 44 | Bred-Bred--689-Gen61-N3981094143-67 | B | USDJPY | 360 | 1.176 | 0.158 | 780 | no | 39.983 |
| 45 | Bred-MACD--387-Gen1-N3981096612-101 | B | USDJPY | 240 | 1.129 | 0.205 | 2350 | no | 39.98 |
| 46 | Bred-RSI-B-296-Gen1-N3981140644-150 | B | USDJPY | 240 | 1.114 | 0.209 | 3011 | no | 39.279 |
| 47 | Bred-Holy--612-Gen1-N3981448858-468 | B | USDJPY | 300 | 1.106 | 0.211 | 3571 | no | 38.941 |
| 48 | Bred-MACD--848-Gen1-N3981140644-147 | B | USDJPY | 240 | 1.118 | 0.188 | 2350 | no | 38.102 |
| 49 | Bred-Bred--293-Gen39-N3981006374-25 | B | USDJPY | 180 | 1.145 | 0.161 | 1135 | no | 37.886 |
| 50 | Bred-MACD--881-Gen58-N3981094756-45 | B | USDJPY | 240 | 1.117 | 0.187 | 2350 | no | 37.886 |
| 51 | Crossover-Plus-MACD | LEGEND | USDJPY | 1440 | 1.150 | 0.148 | 957 | no | 37.328 |
| 52 | Bred-MACD--592-Gen1-N3981105648-115 | B | USDJPY | 240 | 1.113 | 0.181 | 2350 | no | 37.185 |
| 53 | Bred-MACD--556-Gen1-N3981489341-93 | B | USDJPY | 240 | 1.112 | 0.182 | 2350 | no | 37.183 |
| 54 | Bred-MACD--680-Gen1-N3981105588-95 | B | USDJPY | 240 | 1.112 | 0.181 | 2350 | no | 37.145 |
| 55 | Bred-RSI-O-644-Gen1-N3981490099-254 | B | USDJPY | 240 | 1.101 | 0.183 | 3011 | no | 36.648 |
| 56 | Bred-MACD--960-Gen1-N3981107329-534 | B | USDJPY | 240 | 1.109 | 0.177 | 2350 | no | 36.612 |
| 57 | Bred-MACD--432-Gen1-N3981490717-410 | B | USDJPY | 240 | 1.109 | 0.177 | 2350 | no | 36.606 |
| 58 | Bred-MACD--720-Gen1-N3981141590-468 | B | USDJPY | 240 | 1.109 | 0.175 | 2350 | no | 36.458 |
| 59 | Bred-MACD--690-Gen1-N3981448007-180 | B | USDJPY | 240 | 1.108 | 0.176 | 2350 | no | 36.438 |
| 60 | Bred-Bred--587-Gen53-N3980553950-113 | B | USDJPY | 45 | 1.115 | 0.167 | 2015 | no | 36.386 |
| 61 | Bred-MACD--81-Gen1-N3981102574-406 | B | USDJPY | 240 | 1.104 | 0.176 | 2351 | no | 36.033 |

## ❌ REJECT (107)

| # | Name | Lib | Sym | TF | PF | Sharpe | Trades | OOS | Score |
|---|------|-----|-----|----|----|--------|--------|-----|-------|
| 1 | Bred-Bred--741-Gen31-N3980252703-108 | A | USDJPY | 3600 | 9.809 | 17.904 | 35 | yes | 452.21 |
| 2 | Bred-Bred--261-Gen36-N3980242439-83 | A | USDJPY | 3600 | 9.360 | 18.555 | 35 | yes | 449.128 |
| 3 | Bred-Bred--741-Gen31-N3980252703-108 | B | USDJPY | 3600 | 9.809 | 17.904 | 35 | no | 445.96 |
| 4 | Bred-Bred--261-Gen36-N3980242439-83 | B | USDJPY | 3600 | 9.360 | 18.555 | 35 | no | 442.878 |
| 5 | Bred-Bred--550-Gen30-N3980176629-115 | A | USDJPY | 3600 | 9.416 | 17.265 | 35 | yes | 434.397 |
| 6 | Bred-Bred--346-Gen31-N3980094353-514 | B | USDJPY | 3600 | 9.304 | 17.042 | 35 | yes | 428.822 |
| 7 | Bred-Bred--437-Gen31-N3980235907-169 | A | USDJPY | 3600 | 9.304 | 17.042 | 35 | yes | 428.822 |
| 8 | Bred-Bred--867-Gen35-N3980223264-106 | A | USDJPY | 3600 | 9.417 | 15.983 | 34 | yes | 418.386 |
| 9 | Bred-Bred--914-Gen29-N3980188079-337 | A | USDJPY | 3600 | 9.063 | 16.482 | 35 | yes | 415.779 |
| 10 | Bred-Bred--558-Gen58-N3981093371-68 | B | USDJPY | 3600 | 8.643 | 0.383 | 35 | no | 197.792 |
| 11 | Bred-Bred--514-Gen37-N3981013865-72 | B | USDJPY | 3600 | 5.898 | 0.309 | 34 | no | 128.231 |
| 12 | Bred-Bred--703-Gen31-N3981102865-495 | B | USDJPY | 10080 | 3.526 | 0.258 | 35 | no | 68.313 |
| 13 | Bred-Tripl-430-Gen1-N3981448858-453 | B | USDJPY | 10080 | 2.279 | 0.203 | 47 | no | 36.616 |
| 14 | Bred-RSI-S-967-Gen1-N3981105648-122 | B | USDJPY | 10080 | 2.024 | 0.212 | 94 | no | 30.703 |
| 15 | Bred-Bred--862-Gen36-N3981102574-397 | B | USDJPY | 10080 | 2.002 | 0.188 | 51 | no | 29.535 |
| 16 | Bred-Tripl-320-Gen1-N3981140086-42 | B | USDJPY | 10080 | 1.825 | 0.182 | 65 | no | 25.171 |
| 17 | Bladerunner | LEGEND | USDJPY | 10080 | 1.789 | 0.143 | 53 | no | 23.669 |
| 18 | Triple-EMA-Trend-Follow | LEGEND | USDJPY | 10080 | 1.752 | 0.141 | 53 | no | 22.722 |
| 19 | Triple-Screen-Proxy | LEGEND | USDJPY | 10080 | 1.577 | 0.183 | 118 | no | 19.298 |
| 20 | Hunted-D1-VWAPVR-80-180-GBPUSD | B | GBPUSD | 1440 | 1.594 | 0.067 | 32 | no | 17.565 |
| 21 | TEST-REFRESH-ACTIVE-SEXP-SYNC | LEGEND | USDJPY | 60 | 0.000 | 1.400 | 0 | no | 17.5 |
| 22 | Hunted-D1-VWAPVR-50-220-EURUSD | B | EURUSD | 1440 | 1.588 | 0.059 | 21 | no | 17.108 |
| 23 | Bred-RSI-M-924-Gen1-N3981489460-139 | B | USDJPY | 10080 | 1.500 | 0.151 | 119 | no | 16.987 |
| 24 | Bred-Bred--3-Gen59-N3981013859-30 | B | USDJPY | 15 | 1.096 | 0.247 | 6038 | no | 10.226 |
| 25 | Bred-CCI-T-180-Gen1-N3981489341-90 | B | USDJPY | 60 | 1.095 | 0.239 | 6523 | no | 10.119 |
| 26 | Bred-MACD--449-Gen2-N3981140940-248 | B | USDJPY | 90 | 1.093 | 0.242 | 6205 | no | 10.097 |
| 27 | Bred-MACD--36-Gen1-N3981102274-306 | B | USDJPY | 90 | 1.094 | 0.236 | 6205 | no | 10.044 |
| 28 | Sweet-Chariot-SMA-40 | LEGEND | USDJPY | 30 | 1.093 | 0.231 | 6710 | no | 10.004 |
| 29 | Bred-Tripl-505-Gen1-N3981449098-533 | B | USDJPY | 60 | 1.090 | 0.227 | 6716 | no | 9.873 |
| 30 | Pullback-Breakout | LEGEND | USDJPY | 60 | 1.084 | 0.230 | 7374 | no | 9.81 |
| 31 | Bred-RSI-V-22-Gen1-N3981489739-177 | B | USDJPY | 60 | 1.081 | 0.221 | 7373 | no | 9.616 |
| 32 | Bred-Bred--999-Gen36-N3981094143-59 | B | USDJPY | 20 | 1.090 | 0.219 | 4518 | no | 9.551 |
| 33 | Bred-Bred--162-Gen57-N3980554294-548 | B | USDJPY | 15 | 1.082 | 0.219 | 6096 | no | 9.52 |
| 34 | Bred-Blade-713-Gen1-N3981489799-183 | B | USDJPY | 60 | 1.082 | 0.212 | 6066 | no | 9.422 |
| 35 | Bred-Bred--433-Gen59-N3981004890-36 | B | USDJPY | 20 | 1.092 | 0.197 | 5060 | no | 9.383 |
| 36 | Bred-RSI-S-915-Gen1-N3981490717-418 | B | USDJPY | 240 | 1.088 | 0.199 | 5472 | no | 9.37 |
| 37 | Bred-Pullb-694-Gen1-N3981490044-224 | B | USDJPY | 60 | 1.078 | 0.203 | 6958 | no | 9.295 |
| 38 | Bred-Puria-921-Gen1-N3981449099-545 | B | USDJPY | 240 | 1.079 | 0.199 | 6767 | no | 9.248 |
| 39 | Bred-Bred--561-Gen55-N3980554331-643 | B | USDJPY | 60 | 1.068 | 0.181 | 10001 | no | 8.978 |
| 40 | MACD-Above-Zero-Cross | LEGEND | USDJPY | 240 | 1.097 | 0.164 | 2351 | no | 8.681 |
| 41 | MACD-Signal-Cross | LEGEND | USDJPY | 240 | 1.097 | 0.164 | 2351 | no | 8.681 |
| 42 | Bred-Bred--895-Gen59-N3981138750-16 | B | USDJPY | 15 | 1.066 | 0.178 | 6782 | no | 8.667 |
| 43 | Bred-Tripl-920-Gen1-N3981141590-459 | B | USDJPY | 60 | 1.064 | 0.176 | 7733 | no | 8.654 |
| 44 | Bred-CCI-T-426-Gen1-N3981105588-94 | B | USDJPY | 60 | 1.067 | 0.173 | 6580 | no | 8.609 |
| 45 | Bred-Bred--458-Gen61-N3981094143-68 | B | USDJPY | 12 | 1.065 | 0.171 | 7362 | no | 8.588 |
| 46 | Bred-MACD--769-Gen1-N3981094756-42 | B | USDJPY | 90 | 1.065 | 0.177 | 6205 | no | 8.564 |
| 47 | Bred-Blade-690-Gen1-N3981141291-360 | B | USDJPY | 60 | 1.063 | 0.164 | 6657 | no | 8.408 |
| 48 | MACD-Zero-Cross-Long | LEGEND | USDJPY | 240 | 1.088 | 0.149 | 2351 | no | 8.289 |
| 49 | MACD-Zero-Reject | LEGEND | USDJPY | 240 | 1.088 | 0.149 | 2351 | no | 8.289 |
| 50 | Fibonacci-EMA-Scalp | LEGEND | USDJPY | 10080 | 1.186 | 0.071 | 149 | no | 8.263 |
| 51 | Trend-Pullback-Entry | LEGEND | USDJPY | 15 | 1.071 | 0.151 | 4871 | no | 8.263 |
| 52 | Bred-Bred--425-Gen30-N3981102574-396 | B | USDJPY | 45 | 1.055 | 0.156 | 6889 | no | 8.129 |
| 53 | ('#struct', '#A', [[25], 'BASE-CHAR', '.', 'RECRUIT-RND-1768781166-12']) | A | USDJPY | 1440 | 0.000 | 0.471 | 61 | no | 8.125 |
| 54 | MACD-Expansion | LEGEND | USDJPY | 240 | 1.085 | 0.141 | 2350 | no | 8.11 |
| 55 | Bred-Puria-196-Gen1-N3981107329-542 | B | USDJPY | 60 | 1.054 | 0.153 | 7056 | no | 8.077 |
| 56 | CCI-Trend-Breakout | LEGEND | USDJPY | 240 | 1.072 | 0.125 | 2431 | no | 7.592 |
| 57 | Momentum-Burst | LEGEND | USDJPY | 1 | 0.469 | -0.124 | 1106006 | no | 7.555 |
| 58 | Elder-Impulse-Simulated | LEGEND | USDJPY | 1 | 0.518 | -5.175 | 794728 | no | 7.375 |
| 59 | Scalp-Cross-5-12 | LEGEND | USDJPY | 1 | 0.552 | -5.531 | 707491 | no | 7.312 |
| 60 | Session-Breakout-Proxy | LEGEND | USDJPY | 1 | 0.563 | -5.454 | 623779 | no | 7.244 |
| 61 | MACD-RSI-Confluence | LEGEND | USDJPY | 1 | 0.567 | -5.328 | 600531 | no | 7.223 |
| 62 | Conservative-Trend | LEGEND | USDJPY | 30 | 1.060 | 0.105 | 3147 | no | 7.181 |
| 63 | MA-Ribbon-Scalp | LEGEND | USDJPY | 10080 | 1.149 | 0.058 | 149 | no | 7.157 |
| 64 | RSI-Fast-Break | LEGEND | USDJPY | 1 | 0.569 | -5.587 | 508877 | no | 7.133 |
| 65 | RSI-Short-Reversion | LEGEND | USDJPY | 1 | 0.570 | -5.603 | 508987 | no | 7.133 |
| 66 | RSI-Trend-Shift | LEGEND | USDJPY | 1 | 0.568 | -5.530 | 496532 | no | 7.12 |
| 67 | RSI-2-Period-Connors | LEGEND | USDJPY | 1 | 0.564 | -5.474 | 483795 | no | 7.106 |
| 68 | Test-Legend-Reval | RETIRED | USDJPY | 1 | 0.204 | -50.251 | 477401 | no | 7.099 |
| 69 | TF-5-20 | LEGEND | USDJPY | 1 | 0.608 | -5.202 | 472995 | no | 7.094 |
| 70 | Low-Vol-Breakout | LEGEND | USDJPY | 1 | 0.605 | -5.183 | 460349 | no | 7.079 |
| 71 | Volatility-Trend-Follow | LEGEND | USDJPY | 1 | 0.605 | -5.155 | 460296 | no | 7.079 |
| 72 | TF-10-50 | LEGEND | USDJPY | 60 | 1.052 | 0.107 | 3223 | no | 7.016 |
| 73 | Fast-EMA-Cross-9-21 | LEGEND | USDJPY | 1 | 0.630 | -4.943 | 401726 | no | 7.005 |
| 74 | Medium-EMA-Cross-20-50 | LEGEND | USDJPY | 60 | 1.054 | 0.103 | 2779 | no | 6.947 |
| 75 | BB-Squeeze-Expansion | LEGEND | USDJPY | 1 | 0.646 | -4.511 | 320420 | no | 6.882 |
| 76 | BB-Breakout-Lower | LEGEND | USDJPY | 1 | 0.639 | -4.411 | 312657 | no | 6.869 |
| 77 | BB-Breakout-Upper | LEGEND | USDJPY | 1 | 0.639 | -4.411 | 312657 | no | 6.869 |
| 78 | ATR-Confirmed-Breakout | LEGEND | USDJPY | 1 | 0.742 | -3.005 | 186674 | no | 6.589 |
| 79 | RSI-Bull-Zone | LEGEND | USDJPY | 240 | 1.054 | 0.084 | 1669 | no | 6.419 |
| 80 | Silver-Cross-20-100 | LEGEND | USDJPY | 1 | 0.808 | -1.725 | 104360 | no | 6.273 |
| 81 | Death-Cross-50-200 | LEGEND | USDJPY | 1 | 0.873 | -0.793 | 48309 | no | 5.855 |
| 82 | Golden-Cross-50-200 | LEGEND | USDJPY | 1 | 0.873 | -0.793 | 48309 | no | 5.855 |
| 83 | Trend-Scalp-1M | LEGEND | USDJPY | 10080 | 1.116 | 0.035 | 78 | no | 5.711 |
| 84 | RSI-Overbought-Reversal | LEGEND | USDJPY | 240 | 1.023 | 0.048 | 3328 | no | 5.581 |
| 85 | RSI-Oversold-Reversal | LEGEND | USDJPY | 240 | 1.023 | 0.048 | 3328 | no | 5.581 |
| 86 | Puria-Method-Proxy | LEGEND | USDJPY | 240 | 1.016 | 0.036 | 3747 | no | 5.323 |
| 87 | RSI-Stoch-Reversal | LEGEND | USDJPY | 240 | 1.008 | 0.018 | 3422 | no | 4.846 |
| 88 | RSI-Volatility-Break | LEGEND | USDJPY | 240 | 1.008 | 0.018 | 3422 | no | 4.846 |
| 89 | RSI-Momentum-Break | LEGEND | USDJPY | 240 | 0.971 | -0.063 | 3535 | no | 4.435 |
| 90 | Holy-Grail-Proxy | LEGEND | USDJPY | 60 | 1.002 | 0.005 | 2778 | no | 4.426 |
| 91 | TEST-REFRESH-RECON-B | A | USDJPY | 1 | 0.000 | 0.300 | 0 | no | 3.75 |
| 92 | TEST-REFRESH-RECON-DB-B | A | USDJPY | 1 | 0.000 | 0.300 | 0 | no | 3.75 |
| 93 | Stoch-Extreme-Dip | LEGEND | USDJPY | 1440 | 0.974 | -0.025 | 723 | no | 3.574 |
| 94 | Stoch-Pop | LEGEND | USDJPY | 1440 | 1.000 | -0.000 | 723 | no | 3.574 |
| 95 | Stoch-Momentum-Cross | LEGEND | USDJPY | 1440 | 0.968 | -0.031 | 715 | no | 3.568 |
| 96 | Stoch-Overbought-Entry | LEGEND | USDJPY | 1440 | 0.971 | -0.029 | 714 | no | 3.567 |
| 97 | Stoch-Oversold-Entry | LEGEND | USDJPY | 1440 | 0.971 | -0.029 | 714 | no | 3.567 |
| 98 | Aggressive-Reversal | LEGEND | USDJPY | 10080 | 0.869 | -0.058 | 130 | no | 2.642 |
| 99 | BB-Lower-Bounce | LEGEND | USDJPY | 10080 | 0.863 | -0.061 | 129 | no | 2.638 |
| 100 | BB-RSI-Reversion-Combo | LEGEND | USDJPY | 10080 | 0.845 | -0.070 | 129 | no | 2.638 |
| 101 | BB-Upper-Rejection | LEGEND | USDJPY | 10080 | 0.863 | -0.061 | 129 | no | 2.638 |
| 102 | Double-Bollinger-Trend | LEGEND | USDJPY | 10080 | 0.863 | -0.061 | 129 | no | 2.638 |
| 103 | Extreme-Reversion-BB | LEGEND | USDJPY | 10080 | 0.872 | -0.058 | 125 | no | 2.621 |
| 104 | Bollinger-Band-Walk | LEGEND | USDJPY | 10080 | 0.920 | -0.035 | 124 | no | 2.617 |
| 105 | TestStrat | S | USDJPY | 1 | 0.000 | 0.000 | 0 | no | 0.0 |
| 106 | Legend-London-Breakout-V1 | LEGEND | USDJPY | 3600 | 0.000 | 0.000 | 0 | no | 0.0 |
| 107 | Legend-RSI-Reversion-V1 | LEGEND | USDJPY | 300 | 0.000 | 0.000 | 0 | no | 0.0 |

