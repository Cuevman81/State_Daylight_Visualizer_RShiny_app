#!/usr/bin/env python3
"""
LinkedIn-optimized single graphic: what permanent DST does to a Mississippi
winter day. 4:5 portrait (1440x1800) for maximum feed real estate.

Top   — the shortest day in Jackson as two day-timelines (today vs permanent DST)
Bottom— the trade-off in two stat tiles (dark mornings vs brighter evenings)

Author: Rodney Cuevas  |  Sun math: NOAA solar equations
"""

import datetime as dt
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import FancyBboxPatch, Rectangle
import os

LAT, LON, YEAR = 32.2988, -90.1848, 2025
DST_START_DOY = dt.date(YEAR, 3, 9).timetuple().tm_yday
DST_END_DOY   = dt.date(YEAR, 11, 2).timetuple().tm_yday

# Palette (validated: worst-pair CVD dE 96.7)
C_CURRENT = "#2a78d6"; C_PERM = "#eb6834"
INK = "#0b0b0b"; INK2 = "#52514e"; MUTED = "#898781"
GRID = "#dcdbd4"; SURFACE = "#fcfcfb"; NIGHT = "#e8ecf5"; TILE = "#f4f3ee"

# ----------------------------------------------------------------------------
# NOAA sunrise/sunset (same equations as compute_and_plot.py)
# ----------------------------------------------------------------------------
def julian_day(year, month, day):
    if month <= 2:
        year -= 1; month += 12
    A = year // 100
    B = 2 - A + A // 4
    return int(365.25 * (year + 4716)) + int(30.6001 * (month + 1)) + day + B - 1524.5

def sun_rise_set_utc_minutes(date, lat, lon):
    jd = julian_day(date.year, date.month, date.day) + 0.5
    T = (jd - 2451545.0) / 36525.0
    L0 = (280.46646 + T * (36000.76983 + 0.0003032 * T)) % 360.0
    M = 357.52911 + T * (35999.05029 - 0.0001537 * T)
    e = 0.016708634 - T * (0.000042037 + 0.0000001267 * T)
    Mr = np.radians(M)
    C = (np.sin(Mr) * (1.914602 - T * (0.004817 + 0.000014 * T))
         + np.sin(2 * Mr) * (0.019993 - 0.000101 * T) + np.sin(3 * Mr) * 0.000289)
    true_long = L0 + C
    omega = 125.04 - 1934.136 * T
    app_long = true_long - 0.00569 - 0.00478 * np.sin(np.radians(omega))
    eps0 = 23.0 + (26.0 + (21.448 - T * (46.815 + T * (0.00059 - T * 0.001813))) / 60.0) / 60.0
    eps = eps0 + 0.00256 * np.cos(np.radians(omega))
    decl = np.degrees(np.arcsin(np.sin(np.radians(eps)) * np.sin(np.radians(app_long))))
    y = np.tan(np.radians(eps / 2.0)) ** 2
    L0r = np.radians(L0)
    eq_time = 4.0 * np.degrees(
        y * np.sin(2 * L0r) - 2 * e * np.sin(Mr)
        + 4 * e * y * np.sin(Mr) * np.cos(2 * L0r)
        - 0.5 * y * y * np.sin(4 * L0r) - 1.25 * e * e * np.sin(2 * Mr))
    lat_r, decl_r = np.radians(lat), np.radians(decl)
    cos_ha = (np.cos(np.radians(90.833)) / (np.cos(lat_r) * np.cos(decl_r))
              - np.tan(lat_r) * np.tan(decl_r))
    ha = np.degrees(np.arccos(cos_ha))
    solar_noon = 720.0 - 4.0 * lon - eq_time
    return solar_noon - 4.0 * ha, solar_noon + 4.0 * ha

# Full-year series for the day-count stats
sr_cur, sr_perm, ss_cur, ss_perm = [], [], [], []
d, end = dt.date(YEAR, 1, 1), dt.date(YEAR, 12, 31)
while d <= end:
    doy = d.timetuple().tm_yday
    off_cur = -5.0 if DST_START_DOY <= doy < DST_END_DOY else -6.0
    sr, ss = sun_rise_set_utc_minutes(d, LAT, LON)
    sr_cur.append((sr + off_cur * 60) / 60.0);  ss_cur.append((ss + off_cur * 60) / 60.0)
    sr_perm.append((sr - 5 * 60) / 60.0);       ss_perm.append((ss - 5 * 60) / 60.0)
    d += dt.timedelta(days=1)
sr_cur, sr_perm = np.array(sr_cur), np.array(sr_perm)
ss_cur, ss_perm = np.array(ss_cur), np.array(ss_perm)

n_dark_cur  = int(np.sum(sr_cur  >= 7.5));  n_dark_perm  = int(np.sum(sr_perm  >= 7.5))
n_light_cur = int(np.sum(ss_cur  >= 18.0)); n_light_perm = int(np.sum(ss_perm >= 18.0))

# Winter solstice (shortest day) on both clocks
sr_utc, ss_utc = sun_rise_set_utc_minutes(dt.date(YEAR, 12, 21), LAT, LON)
sr_c, ss_c = (sr_utc - 360) / 60.0, (ss_utc - 360) / 60.0   # CST (UTC-6)
sr_p, ss_p = (sr_utc - 300) / 60.0, (ss_utc - 300) / 60.0   # CDT (UTC-5)
dur_min = int(round((ss_c - sr_c) * 60))

def clock(h, am_pm=True):
    m = int(round(h * 60)) % 1440
    hh, mm = m // 60, m % 60
    disp = hh % 12 or 12
    return f"{disp}:{mm:02d} {'AM' if hh < 12 else 'PM'}" if am_pm else f"{disp}:{mm:02d}"

# ----------------------------------------------------------------------------
# Figure — 4:5 portrait
# ----------------------------------------------------------------------------
plt.rcParams.update({
    "font.family": "sans-serif",
    "font.sans-serif": ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
    "figure.facecolor": SURFACE, "axes.facecolor": SURFACE,
    "savefig.facecolor": SURFACE, "text.color": INK,
})
fig = plt.figure(figsize=(7.2, 9.0))

# --- header ---
fig.patches.append(Rectangle((0.075, 0.9805), 0.105, 0.0055, transform=fig.transFigure,
                             facecolor=C_PERM, edgecolor="none"))
fig.text(0.075, 0.958, "LOCKING THE CLOCK  ·  JACKSON, MISSISSIPPI",
         fontsize=9.5, fontweight="bold", color=INK2)
fig.text(0.075, 0.921, "What permanent daylight saving time",
         fontsize=21.5, fontweight="bold", color=INK)
fig.text(0.075, 0.889, "would do to a Mississippi winter day",
         fontsize=21.5, fontweight="bold", color=INK)
fig.text(0.075, 0.8545,
         "The U.S. House just passed the Sunshine Protection Act, 308–117. If it becomes law,",
         fontsize=10.8, color=INK2)
fig.text(0.075, 0.8355,
         "Mississippi stays on CDT all winter instead of falling back to CST — pushing both",
         fontsize=10.8, color=INK2)
fig.text(0.075, 0.8165,
         "sunrise and sunset an hour later. The shortest day of the year in Jackson:",
         fontsize=10.8, color=INK2)

# --- day-timeline panel ---
ax = fig.add_axes([0.075, 0.538, 0.88, 0.25])
XLO, XHI = 5.0, 20.6
Y_CUR, Y_PERM, H = 1.45, 0.25, 0.52
ax.set_xlim(XLO, XHI); ax.set_ylim(-0.32, 2.62)

ticks = list(range(6, 21, 2))
ax.set_xticks(ticks)
ax.set_xticklabels(["6 AM", "8 AM", "10 AM", "Noon", "2 PM", "4 PM", "6 PM", "8 PM"],
                   fontsize=9, color=INK2)
ax.set_yticks([])
for s in ax.spines.values():
    s.set_visible(False)
ax.tick_params(length=0)
for t in ticks:
    ax.plot([t, t], [-0.12, 1.75], color=GRID, lw=0.8, zorder=1)

# surface-colored bbox masks the dashed reference line where it crosses text
MASK = dict(facecolor=SURFACE, edgecolor="none", pad=1.5)

for y, sr, ss, col, lab in [
        (Y_CUR,  sr_c, ss_c, C_CURRENT, "TODAY — winter falls back to standard time (CST)"),
        (Y_PERM, sr_p, ss_p, C_PERM,    "PERMANENT DST — winter stays on daylight time (CDT)")]:
    ax.add_patch(Rectangle((XLO, y - H / 2), XHI - XLO, H, facecolor=NIGHT,
                           edgecolor="none", zorder=2))
    ax.add_patch(FancyBboxPatch((sr, y - H / 2), ss - sr, H,
                                boxstyle="round,pad=0,rounding_size=0.07",
                                mutation_aspect=0.35, facecolor=col,
                                edgecolor="none", zorder=3))
    ax.text(XLO, y + H / 2 + 0.13, lab, fontsize=9.8, fontweight="bold",
            color=INK, va="bottom", zorder=6, bbox=MASK)
    ax.text(sr - 0.14, y, clock(sr), fontsize=11, fontweight="bold",
            color=INK, ha="right", va="center", zorder=6)
    ax.text(ss + 0.14, y, clock(ss), fontsize=11, fontweight="bold",
            color=INK, ha="left", va="center", zorder=6)

# school-bus reference line (7:30 AM) — crosses daylight today, darkness under DST
ax.plot([7.5, 7.5], [-0.24, 2.40], color=INK2, lw=1.1, ls=(0, (4, 3)), zorder=4)
ax.text(7.62, 2.44, "7:30 AM — school buses & morning commutes",
        fontsize=8.6, color=INK2, va="bottom", zorder=6)

# the +1 hour shift arrows, in the clear lane between the two rows
for x0 in (sr_c, ss_c):
    ax.annotate("", xy=(x0 + 1, 1.0), xytext=(x0, 1.0),
                arrowprops=dict(arrowstyle="->", color=INK2, lw=1.2), zorder=5)
    ax.text(x0 + 1 + 0.14, 1.0, "+1 hr", fontsize=8.6, color=INK2,
            ha="left", va="center", zorder=6, bbox=MASK)

fig.text(0.075, 0.489,
         f"Same {dur_min // 60} h {dur_min % 60:02d} min of daylight either way — "
         "permanent DST just moves it an hour later on the clock.",
         fontsize=10.3, color=INK2, style="italic")

# --- stat tiles ---
def tile(x0, x1, y0, y1, kicker, big, desc1, desc2, base):
    fig.patches.append(FancyBboxPatch((x0, y0), x1 - x0, y1 - y0,
                                      transform=fig.transFigure,
                                      boxstyle="round,pad=0,rounding_size=0.012",
                                      facecolor=TILE, edgecolor=GRID, linewidth=1.0))
    fig.text(x0 + 0.028, y1 - 0.042, kicker, fontsize=9.6, fontweight="bold", color=INK2)
    fig.text(x0 + 0.028, y1 - 0.135, big, fontsize=45, fontweight="bold", color=INK)
    fig.text(x0 + 0.028, y1 - 0.172, desc1, fontsize=10.4, color=INK2)
    fig.text(x0 + 0.028, y1 - 0.194, desc2, fontsize=10.4, color=INK2)
    fig.text(x0 + 0.028, y0 + 0.028, base, fontsize=9.4, color=MUTED)

TY0, TY1 = 0.145, 0.425
tile(0.075, 0.4975, TY0, TY1,
     "THE COST — DARK MORNINGS", f"{n_dark_perm} days",
     "a year the sun would rise", "after 7:30 AM",
     f"on today's clock: {n_dark_cur}")
tile(0.5325, 0.955, TY0, TY1,
     "THE BENEFIT — BRIGHT EVENINGS", f"{n_light_perm} days",
     "a year the sun would still be", "up at 6:00 PM",
     f"on today's clock: {n_light_cur}")

fig.text(0.075, 0.032,
         "Jackson, MS (32.30°N, 90.18°W) · shortest day = Dec 21 · day counts over a full year",
         fontsize=8.3, color=MUTED)
fig.text(0.075, 0.014,
         "Sun position: NOAA solar equations · Analysis: Rodney Cuevas",
         fontsize=8.3, color=MUTED)

BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
out = os.path.join(BASE, "plots", "05_linkedin_daylight_shift.png")
fig.savefig(out, dpi=200)
print("wrote", out, f"({n_dark_cur}->{n_dark_perm} dark mornings, "
      f"{n_light_cur}->{n_light_perm} bright evenings)")
