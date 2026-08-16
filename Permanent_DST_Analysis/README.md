# Locking the Clock — Impact on Mississippi

What would happen to Mississippi mornings and evenings if the U.S. stopped
changing clocks twice a year? There are **two** proposals, and they do opposite
things. This analysis charts both against today's law for Jackson, MS.

Mississippi is in the **Central Time Zone**. Today it switches between:

- **CST (UTC−6)** — standard time, in winter (fall-back)
- **CDT (UTC−5)** — daylight time, in summer (spring-forward)

| Proposal | Clock | What changes | Effect |
|---|---|---|---|
| **Permanent DST** (front-runner) | CDT (UTC−5) all year | **Winter** (summer already on CDT) | Later winter sunrises **and** later winter sunsets |
| **Permanent standard time** (scientist-favored) | CST (UTC−6) all year | **Summer** (winter already on CST) | Earlier summer sunrises **and** earlier summer sunsets |

## Legislative status (as of July 15, 2026)

- The U.S. **House passed the Sunshine Protection Act 308–117 on July 14, 2026**,
  which would make **permanent daylight saving time**. President Trump backs it
  and is expected to sign if it reaches his desk.
- It now goes to the **Senate, where its odds are unclear** — there is bipartisan
  opposition (Sen. Tom Cotton is asking leadership not to bring it up), citing
  dark winter-morning commutes. In 2022 the Senate passed permanent DST by
  unanimous consent and it then **died in the House**.
- A **competing permanent-standard-time bill** (Reps. Scanlon, D-PA & Harrigan,
  R-NC) also exists; the **AMA, the American Academy of Sleep Medicine, and
  Stanford researchers** favor permanent standard time as the healthier option.

So permanent **DST** is the version with momentum, but passage is not certain, and
permanent **standard time** remains a live, medically-preferred alternative — which
is why this analysis charts both.

Sources: [The Hill](https://thehill.com/homenews/house/5968255-house-sunshine-protection-act-daylight-saving-time/) ·
[CBS News](https://www.cbsnews.com/news/daylight-saving-time-permanent-house-vote/) ·
[CNN](https://www.cnn.com/2026/07/14/politics/house-vote-daylight-savings-time) ·
[Yahoo / experts on standard time](https://www.yahoo.com/news/politics/article/congress-might-make-daylight-saving-time-permanent-experts-say-another-approach-could-be-even-better-212046022.html)

## The bottom line for Jackson, MS

A common mix-up is "permanent DST = later sunrises **and earlier sunsets**." Not so
— that describes permanent **standard** time. The two proposals are exact opposites:

| | Current law | Permanent DST | Permanent standard |
|---|---|---|---|
| Latest winter sunrise | ~7:18 AM | **~8:03 AM** | ~7:18 AM (unchanged) |
| Earliest winter sunset | ~4:55 PM | **~5:55 PM** | ~4:55 PM (unchanged) |
| Earliest summer sunrise | ~5:53 AM | ~5:53 AM (unchanged) | **~4:53 AM** |
| Latest summer sunset | ~8:12 PM | ~8:12 PM (unchanged) | **~7:12 PM** |
| Days sunrise after 7:30 AM | 0 | **105** | 0 |
| Days sunset after 6:00 PM | 243 | **329** | 243 |

- **Permanent DST** buys bright winter evenings (sun up past 6 PM nearly year-round)
  at the cost of **dark winter mornings** — sun rising after 7:30 AM for ~105 days.
- **Permanent standard time** keeps mornings bright but pulls summer evenings ~1 hr
  earlier (sunset before 7:15 PM even at the solstice) — the "earlier sunsets" many
  people first picture.

## National context (the US map)

The US map is a 2×2 grid — **sunrise** (top) and **sunset** (bottom) × **permanent
DST** (left) and **permanent standard time** (right). Its reds fall on a diagonal,
which is the whole point: **in winter you can brighten the morning or the evening,
not both.**

- **Dark mornings (permanent DST).** Worse the farther **north** a state is and the
  farther **west** it sits in its zone. The sun would not rise until **after 9 AM**
  in a whole band: North Dakota **9:30**, Michigan & South Dakota **9:18**, Idaho
  **9:15**, Indiana **9:08**, Nebraska **9:07**, Montana **9:05**, Minnesota
  **9:01**. Mississippi (**8:03**) is moderate.
- **Dark evenings (permanent standard time).** Worse the farther **north** and the
  farther **east** in a zone. The sun would set before ~4:30 PM in the Northeast and
  Pacific Northwest: Maine **3:55**, New Hampshire **4:10**, Washington **4:12**,
  Vermont **4:13**, Massachusetts **4:16**. Mississippi's winter sunset stays ~4:55.

Permanent DST fixes the dark-evening problem but creates the dark-morning one;
permanent standard time does the reverse. That geographic spread — sun after 9 AM
in the north-central states, sunset before 4 PM in New England — is exactly what
the two sides of the Senate debate point to.

**Alaska is the extreme** (shown as an inset): under permanent DST, Anchorage's
sun wouldn't rise until **~11:16 AM**. **Hawaii barely moves** (near the equator)
and is exempt anyway. **Arizona is exempt** too — it keeps standard time year-round,
so it's the one state that doesn't change, marked on the DST maps.

A second national map (`06_us_map_daycounts.png`) restates the Jackson trade-off
chart (figure 02) for every state: **days per year** the sun rises after 7:30 AM
(the cost) and stays up past 6:00 PM (the benefit), today vs. permanent DST.
Standouts: **Indiana would have 196 mornings a year** with sunrise after 7:30 AM
(Kentucky 190, Michigan 188, the Dakotas ~186–187; Mississippi ~105), while
**17 states** — including Michigan, Indiana, Texas, and Florida — would see the
sun set after 6 PM **every day of the year**. Counts use each state's
representative point, so Mississippi's differ slightly from the Jackson figures.

## Files

```
Permanent_DST_Analysis/
├── README.md                                            ← this file
├── scripts/
│   ├── compute_and_plot.py                              ← NOAA sun math + Jackson figures (Python)
│   ├── linkedin_graphic.py                              ← LinkedIn-ready social graphic (Python)
│   ├── us_map.R                                         ← national choropleth: winter extremes (R)
│   └── us_map_daycounts.R                               ← national choropleth: day counts (R)
├── data/
│   ├── jackson_ms_sun_times_2025.csv                    ← daily times, all 3 scenarios
│   ├── us_states_winter_daylight.csv                    ← per-state winter sunrise & sunset
│   └── us_states_daycounts.csv                          ← per-state days/yr past the 7:30 AM & 6 PM thresholds
└── plots/
    ├── 01_sunrise_sunset_current_vs_permanent_dst.png   ← hero: DST vs today
    ├── 02_dark_mornings_vs_lighter_evenings.png         ← the DST trade-off, in day-counts
    ├── 03_three_scenarios_dst_vs_standard.png           ← all three: DST vs standard vs today
    ├── 04_us_map_winter_daylight.png                    ← US 2×2 map: sunrise & sunset × DST & standard
    ├── 05_linkedin_daylight_shift.png                   ← social-media one-glancer (4:5 portrait, 1440×1800)
    └── 06_us_map_daycounts.png                          ← US 2×2 map: figure 02's day counts, all 50 states
```

## Method

The Jackson figures compute sunrise / sunset directly from the **NOAA
solar-position equations** (no external astronomy package), for Jackson, MS
(32.30° N, 90.18° W) across a representative non-leap year (2025). Times are placed
on the wall clock under each timezone scenario. Colors use a colorblind-safe
blue / orange / violet set (validated: worst-pair CVD ΔE ≈ 97), with line-style
and direct labels as secondary encoding.

The **US map** (`us_map.R`) uses `{suncalc}` for sun times, `{maps}` +
`{mapdata}` (worldHires) for boundaries, and `{patchwork}` to stack the two rows.
Each state gets a representative point (geographic center) and its predominant time
zone. The metrics are the **latest winter-morning sunrise** (top row) and the
**earliest winter-evening sunset** (bottom row) under each proposal; in both rows the
color scale is set so **red = more darkness during waking hours**. **Arizona does not
observe DST**, so it is computed as staying on standard time even in the permanent-DST
scenario (unchanged between the two columns, and marked "AZ exempt" on the DST maps).
**Alaska** (at Anchorage) and **Hawaii** (at Honolulu, also exempt) are shown as
name-labeled insets — not to scale — pulled from `{mapdata}` worldHires, since the
base `{maps}` state boundaries don't include them. Their exact times are in the CSV
(all 50 states).

## Reproduce

```bash
python3 scripts/compute_and_plot.py   # Jackson figures 01–03 (needs numpy, matplotlib)
python3 scripts/linkedin_graphic.py   # social graphic 05  (needs numpy, matplotlib)
Rscript   scripts/us_map.R            # US map 04        (needs suncalc, maps, ggplot2, dplyr, tidyr)
Rscript   scripts/us_map_daycounts.R  # US map 06        (same R packages)
```

On the MacBook Air, the Python environment lives at `~/.venvs/sunrise_dst`
(Homebrew Python + numpy + matplotlib):

```bash
~/.venvs/sunrise_dst/bin/python scripts/compute_and_plot.py
~/.venvs/sunrise_dst/bin/python scripts/linkedin_graphic.py
```

Author: Rodney Cuevas
