#!/usr/bin/env python3
"""
Permanent Daylight Saving Time — impact on Mississippi mornings & evenings.

Compares two clock-time scenarios for Jackson, MS over a full year:
  1. CURRENT LAW   -> CST (UTC-6) in winter, CDT (UTC-5) in summer (spring-forward / fall-back)
  2. PERMANENT DST -> CDT (UTC-5) year-round (the Sunshine Protection Act scenario)

Sunrise / sunset are computed from the NOAA solar-position algorithm (no external
astronomy package required), then expressed on the wall clock under each scenario.

Author: Rodney Cuevas  |  Sun math: NOAA solar equations
"""

import datetime as dt
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from matplotlib.ticker import FuncFormatter
import csv
import os

# ----------------------------------------------------------------------------
# Location & year
# ----------------------------------------------------------------------------
CITY = "Jackson, Mississippi"
LAT = 32.2988          # degrees N
LON = -90.1848         # degrees E (negative = West)
YEAR = 2025            # representative non-leap year

# US DST 2025: 2nd Sunday of March (Mar 9) 02:00  ->  1st Sunday of Nov (Nov 2) 02:00
# Day-of-year of spring-forward and fall-back (Nov 2 falls back at 2am -> that morning is CST)
DST_START_DOY = dt.date(YEAR, 3, 9).timetuple().tm_yday   # first CDT day
DST_END_DOY   = dt.date(YEAR, 11, 2).timetuple().tm_yday  # fall-back day -> standard

# ----------------------------------------------------------------------------
# Palette (validated colorblind-safe, from the data-viz design system)
# ----------------------------------------------------------------------------
C_CURRENT = "#2a78d6"   # blue  -> Current law (baseline)
C_PERM    = "#eb6834"   # orange-> Permanent DST (the front-runner)
C_STD     = "#4a3aa7"   # violet-> Permanent standard time (scientist-favored)
INK       = "#0b0b0b"
INK2      = "#52514e"
MUTED     = "#898781"
GRID      = "#e1e0d9"
SURFACE   = "#fcfcfb"
NIGHT     = "#eef2f8"   # faint wash for the "still dark" / shaded winter region

# ----------------------------------------------------------------------------
# NOAA solar geometry  -> sunrise/sunset in minutes UTC for a given date
# ----------------------------------------------------------------------------
def julian_day(year, month, day):
    if month <= 2:
        year -= 1
        month += 12
    A = year // 100
    B = 2 - A + A // 4
    return int(365.25 * (year + 4716)) + int(30.6001 * (month + 1)) + day + B - 1524.5

def sun_rise_set_utc_minutes(date, lat, lon):
    """Return (sunrise, sunset) as minutes after 00:00 UTC. None if sun never rises/sets."""
    jd = julian_day(date.year, date.month, date.day) + 0.5  # noon of the day
    T = (jd - 2451545.0) / 36525.0

    L0 = (280.46646 + T * (36000.76983 + 0.0003032 * T)) % 360.0
    M = 357.52911 + T * (35999.05029 - 0.0001537 * T)
    e = 0.016708634 - T * (0.000042037 + 0.0000001267 * T)
    Mr = np.radians(M)
    C = (np.sin(Mr) * (1.914602 - T * (0.004817 + 0.000014 * T))
         + np.sin(2 * Mr) * (0.019993 - 0.000101 * T)
         + np.sin(3 * Mr) * 0.000289)
    true_long = L0 + C
    omega = 125.04 - 1934.136 * T
    app_long = true_long - 0.00569 - 0.00478 * np.sin(np.radians(omega))

    eps0 = 23.0 + (26.0 + (21.448 - T * (46.815 + T * (0.00059 - T * 0.001813))) / 60.0) / 60.0
    eps = eps0 + 0.00256 * np.cos(np.radians(omega))

    decl = np.degrees(np.arcsin(np.sin(np.radians(eps)) * np.sin(np.radians(app_long))))

    y = np.tan(np.radians(eps / 2.0)) ** 2
    L0r = np.radians(L0)
    eq_time = 4.0 * np.degrees(
        y * np.sin(2 * L0r)
        - 2 * e * np.sin(Mr)
        + 4 * e * y * np.sin(Mr) * np.cos(2 * L0r)
        - 0.5 * y * y * np.sin(4 * L0r)
        - 1.25 * e * e * np.sin(2 * Mr)
    )  # minutes

    lat_r = np.radians(lat)
    decl_r = np.radians(decl)
    # -0.833 deg accounts for refraction + solar disk radius
    cos_ha = (np.cos(np.radians(90.833)) / (np.cos(lat_r) * np.cos(decl_r))
              - np.tan(lat_r) * np.tan(decl_r))
    if cos_ha > 1 or cos_ha < -1:
        return None, None  # polar day/night (never at MS latitude)
    ha = np.degrees(np.arccos(cos_ha))  # degrees

    solar_noon = 720.0 - 4.0 * lon - eq_time  # minutes UTC
    sunrise = solar_noon - 4.0 * ha
    sunset = solar_noon + 4.0 * ha
    return sunrise, sunset

# ----------------------------------------------------------------------------
# Build the annual series
# ----------------------------------------------------------------------------
dates = []
sr_cur, ss_cur = [], []     # Current law
sr_perm, ss_perm = [], []   # Permanent DST  (CDT all year)
sr_std, ss_std = [], []     # Permanent standard time (CST all year)

d = dt.date(YEAR, 1, 1)
end = dt.date(YEAR, 12, 31)
while d <= end:
    doy = d.timetuple().tm_yday
    is_dst = DST_START_DOY <= doy < DST_END_DOY

    sr_utc, ss_utc = sun_rise_set_utc_minutes(d, LAT, LON)

    # Current law: CDT(-5) in summer, CST(-6) in winter
    off_cur = -5.0 if is_dst else -6.0
    off_perm = -5.0   # Permanent DST:  CDT (-5) all year -> latest clock times
    off_std = -6.0    # Permanent std:  CST (-6) all year -> earliest clock times

    dates.append(d)
    sr_cur.append((sr_utc + off_cur * 60.0) / 60.0)   # hours on the wall clock
    ss_cur.append((ss_utc + off_cur * 60.0) / 60.0)
    sr_perm.append((sr_utc + off_perm * 60.0) / 60.0)
    ss_perm.append((ss_utc + off_perm * 60.0) / 60.0)
    sr_std.append((sr_utc + off_std * 60.0) / 60.0)
    ss_std.append((ss_utc + off_std * 60.0) / 60.0)
    d += dt.timedelta(days=1)

dates = np.array(dates)
sr_cur, ss_cur = np.array(sr_cur), np.array(ss_cur)
sr_perm, ss_perm = np.array(sr_perm), np.array(ss_perm)
sr_std, ss_std = np.array(sr_std), np.array(ss_std)
x = mdates.date2num(dates)

# ----------------------------------------------------------------------------
# Write the tidy data CSV
# ----------------------------------------------------------------------------
BASE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
DATA_DIR = os.path.join(BASE, "data")
PLOT_DIR = os.path.join(BASE, "plots")

def hhmm(h):
    if h is None or np.isnan(h):
        return ""
    m = int(round(h * 60)) % (24 * 60)
    return f"{m // 60:02d}:{m % 60:02d}"

csv_path = os.path.join(DATA_DIR, f"jackson_ms_sun_times_{YEAR}.csv")
with open(csv_path, "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["date", "in_dst_current_law",
                "sunrise_current", "sunset_current",
                "sunrise_permanent_dst", "sunset_permanent_dst",
                "sunrise_permanent_standard", "sunset_permanent_standard"])
    for i, dd in enumerate(dates):
        doy = dd.timetuple().tm_yday
        w.writerow([dd.isoformat(), int(DST_START_DOY <= doy < DST_END_DOY),
                    hhmm(sr_cur[i]), hhmm(ss_cur[i]),
                    hhmm(sr_perm[i]), hhmm(ss_perm[i]),
                    hhmm(sr_std[i]), hhmm(ss_std[i])])
print("wrote", csv_path)

# ----------------------------------------------------------------------------
# Shared styling helpers
# ----------------------------------------------------------------------------
plt.rcParams.update({
    "font.family": "sans-serif",
    "font.sans-serif": ["Helvetica Neue", "Helvetica", "Arial", "DejaVu Sans"],
    "figure.facecolor": SURFACE,
    "axes.facecolor": SURFACE,
    "savefig.facecolor": SURFACE,
    "axes.edgecolor": MUTED,
    "text.color": INK,
    "axes.labelcolor": INK2,
    "xtick.color": INK2,
    "ytick.color": INK2,
})

def clock_fmt(h, _pos=None):
    m = int(round(h * 60)) % (24 * 60)
    hh, mm = m // 60, m % 60
    suffix = "AM" if hh < 12 else "PM"
    disp = hh % 12
    if disp == 0:
        disp = 12
    return f"{disp}:{mm:02d}"

def style_time_axis(ax, lo, hi, step=1.0):
    ax.set_ylim(lo, hi)
    ax.set_yticks(np.arange(lo, hi + 0.001, step))
    ax.yaxis.set_major_formatter(FuncFormatter(clock_fmt))
    ax.xaxis.set_major_locator(mdates.MonthLocator())
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%b"))
    ax.set_xlim(x[0], x[-1])
    ax.grid(True, which="major", color=GRID, linewidth=0.8, zorder=0)
    for s in ("top", "right"):
        ax.spines[s].set_visible(False)
    for s in ("left", "bottom"):
        ax.spines[s].set_color(MUTED)
    ax.tick_params(length=0)

# Winter (currently-CST) spans, for shading the region where the two scenarios differ
w1_end = mdates.date2num(dt.date(YEAR, 3, 9))
w2_start = mdates.date2num(dt.date(YEAR, 11, 2))

# ============================================================================
# FIGURE 1 — hero: sunrise & sunset, current law vs permanent DST
# ============================================================================
fig, (axr, axs) = plt.subplots(2, 1, figsize=(12.5, 10.6), sharex=True)
fig.subplots_adjust(left=0.085, right=0.965, top=0.845, bottom=0.075, hspace=0.34)

for ax in (axr, axs):
    ax.axvspan(x[0], w1_end, color=NIGHT, zorder=0)
    ax.axvspan(w2_start, x[-1], color=NIGHT, zorder=0)

# --- Sunrise panel ---
style_time_axis(axr, 5, 8.5, 0.5)
axr.plot(x, sr_cur, color=C_CURRENT, lw=2.4, zorder=5, label="Current law  (CST winter / CDT summer)")
axr.plot(x, sr_perm, color=C_PERM, lw=2.4, zorder=6, label="Permanent DST  (CDT year-round)")
# Fill the winter gap between the two sunrise curves
axr.fill_between(x, sr_cur, sr_perm, where=(sr_perm > sr_cur + 1e-6),
                 color=C_PERM, alpha=0.13, zorder=2)
axr.axhline(7.0, color=MUTED, lw=1.0, ls=(0, (4, 3)), zorder=1)
axr.text(x[2], 7.04, "7:00 AM  —  typical school-bus / commute hour",
         color=INK2, fontsize=9, va="bottom", ha="left")
axr.text(0.0, 1.10, "SUNRISE", transform=axr.transAxes,
         fontsize=13, fontweight="bold", color=INK)
axr.text(0.0, 1.025, "Later on the clock all winter under permanent DST — sun rises after 7 AM for months",
         transform=axr.transAxes, fontsize=10.5, color=INK2)

# direct end labels
axr.text(x[-1] - 2, sr_perm[-1] + 0.03, "Permanent DST", color=C_PERM,
         fontsize=9.5, fontweight="bold", va="bottom", ha="right")
axr.text(x[-1] - 2, sr_cur[-1] - 0.03, "Current law", color=C_CURRENT,
         fontsize=9.5, fontweight="bold", va="top", ha="right")

# legend in the empty center-top of the sunrise panel
axr.legend(loc="upper center", bbox_to_anchor=(0.53, 0.99), frameon=False,
           fontsize=10, ncol=1, handlelength=1.7, labelcolor=INK2,
           borderaxespad=0.0)

# --- Sunset panel ---
style_time_axis(axs, 16.5, 20.5, 0.5)
axs.plot(x, ss_cur, color=C_CURRENT, lw=2.4, zorder=5)
axs.plot(x, ss_perm, color=C_PERM, lw=2.4, zorder=6)
axs.fill_between(x, ss_cur, ss_perm, where=(ss_perm > ss_cur + 1e-6),
                 color=C_PERM, alpha=0.13, zorder=2)
axs.axhline(18.0, color=MUTED, lw=1.0, ls=(0, (4, 3)), zorder=1)
axs.text(x[2], 18.04, "6:00 PM", color=INK2, fontsize=9, va="bottom", ha="left")
axs.text(0.0, 1.10, "SUNSET", transform=axs.transAxes,
         fontsize=13, fontweight="bold", color=INK)
axs.text(0.0, 1.025, "Also later under permanent DST — winter evenings gain an hour of daylight (sunsets do NOT get earlier)",
         transform=axs.transAxes, fontsize=10.5, color=INK2)
axs.text(x[-1] - 2, ss_perm[-1] + 0.03, "Permanent DST", color=C_PERM,
         fontsize=9.5, fontweight="bold", va="bottom", ha="right")
axs.text(x[-1] - 2, ss_cur[-1] - 0.03, "Current law", color=C_CURRENT,
         fontsize=9.5, fontweight="bold", va="top", ha="right")

# winter region annotation (low-left, inside the shaded band)
axs.text(mdates.date2num(dt.date(YEAR, 1, 6)), 16.62,
         "shaded = winter months\ncurrently on CST", fontsize=8.5,
         color=MUTED, ha="left", va="bottom", linespacing=1.3)

# Figure title block
fig.text(0.085, 0.955, "Permanent Daylight Saving Time in Mississippi: Darker Mornings, Brighter Evenings in Winter",
         fontsize=16, fontweight="bold", color=INK, ha="left")
fig.text(0.085, 0.915,
         f"Sunrise & sunset on the wall clock · {CITY} ({LAT:.2f}°N, {abs(LON):.2f}°W) · {YEAR}. "
         "Summer is already on daylight time, so only the winter months change.",
         fontsize=10.5, color=INK2, ha="left")
fig.text(0.085, 0.028, "Sun position: NOAA solar equations  ·  Author: Rodney Cuevas",
         fontsize=8.5, color=MUTED, ha="left")

fig1_path = os.path.join(PLOT_DIR, "01_sunrise_sunset_current_vs_permanent_dst.png")
fig.savefig(fig1_path, dpi=200)
plt.close(fig)
print("wrote", fig1_path)

# ============================================================================
# FIGURE 2 — the trade-off in counts: dark mornings vs brighter evenings
# ============================================================================
def count_after(series, hour):
    return int(np.sum(series >= hour))

morning_thresholds = [7.0, 7.25, 7.5]     # 7:00, 7:15, 7:30 AM
evening_thresholds = [17.5, 18.0]         # 5:30, 6:00 PM

fig2, (bx1, bx2) = plt.subplots(1, 2, figsize=(12.5, 6.2))
fig2.subplots_adjust(left=0.075, right=0.97, top=0.80, bottom=0.13, wspace=0.22)

def grouped_bars(ax, labels, cur_vals, perm_vals, title, subtitle):
    idx = np.arange(len(labels))
    bw = 0.38
    b1 = ax.bar(idx - bw / 2, cur_vals, bw, color=C_CURRENT, zorder=3,
                label="Current law")
    b2 = ax.bar(idx + bw / 2, perm_vals, bw, color=C_PERM, zorder=3,
                label="Permanent DST")
    ax.set_xticks(idx)
    ax.set_xticklabels(labels, fontsize=10.5, color=INK)
    ax.set_ylabel("Days per year", fontsize=10, color=INK2)
    ax.set_ylim(0, max(max(cur_vals), max(perm_vals)) * 1.28 + 1)
    ax.grid(True, axis="y", color=GRID, linewidth=0.8, zorder=0)
    for s in ("top", "right", "left"):
        ax.spines[s].set_visible(False)
    ax.spines["bottom"].set_color(MUTED)
    ax.tick_params(length=0)
    for bars in (b1, b2):
        for b in bars:
            h = b.get_height()
            ax.text(b.get_x() + b.get_width() / 2, h + 0.6, f"{int(h)}",
                    ha="center", va="bottom", fontsize=10, fontweight="bold",
                    color=INK)
    ax.set_title(title, loc="left", fontsize=12.5, fontweight="bold", color=INK, pad=18)
    ax.text(0.0, 1.02, subtitle, transform=ax.transAxes, fontsize=9.8, color=INK2)

m_labels = ["after 7:00 AM", "after 7:15 AM", "after 7:30 AM"]
m_cur = [count_after(sr_cur, t) for t in morning_thresholds]
m_perm = [count_after(sr_perm, t) for t in morning_thresholds]
grouped_bars(bx1, m_labels, m_cur, m_perm,
             "THE COST — dark mornings",
             "Number of days the sun rises this late")

e_labels = ["after 5:30 PM", "after 6:00 PM"]
e_cur = [count_after(ss_cur, t) for t in evening_thresholds]
e_perm = [count_after(ss_perm, t) for t in evening_thresholds]
grouped_bars(bx2, e_labels, e_cur, e_perm,
             "THE BENEFIT — lighter evenings",
             "Number of days the sun sets this late")

fig2.text(0.075, 0.955, "The Permanent-DST Trade-Off for Mississippi",
          fontsize=16.5, fontweight="bold", color=INK, ha="left")
fig2.text(0.075, 0.905,
          f"{CITY}, {YEAR}. Permanent daylight time shifts both sunrise and sunset an hour later each winter.",
          fontsize=10.3, color=INK2, ha="left")
fig2.text(0.075, 0.02, "Sun position: NOAA solar equations  ·  Author: Rodney Cuevas",
          fontsize=8.5, color=MUTED, ha="left")

h2, l2 = bx1.get_legend_handles_labels()
fig2.legend(h2, l2, loc="upper right", bbox_to_anchor=(0.97, 0.965),
            frameon=False, fontsize=10.5, labelcolor=INK2, handlelength=1.4,
            ncol=2, columnspacing=1.6)

fig2_path = os.path.join(PLOT_DIR, "02_dark_mornings_vs_lighter_evenings.png")
fig2.savefig(fig2_path, dpi=200)
plt.close(fig2)
print("wrote", fig2_path)

# ============================================================================
# FIGURE 3 — three scenarios: current law vs permanent DST vs permanent standard
# ============================================================================
BAND = "#efedf4"  # faint fill for the 1-hour "range of choice" between proposals

fig3, (cx, cs) = plt.subplots(2, 1, figsize=(12.5, 10.8), sharex=True)
fig3.subplots_adjust(left=0.085, right=0.965, top=0.845, bottom=0.075, hspace=0.34)

def three_panel(ax, cur, perm, std, lo, hi, label, subtitle,
                lbl_perm, lbl_std, lbl_cur_xy):
    style_time_axis(ax, lo, hi, 0.5)
    # the 1-hour band the whole debate lives inside (perm = later edge, std = earlier edge)
    ax.fill_between(x, std, perm, color=BAND, zorder=1)
    ax.plot(x, perm, color=C_PERM, lw=2.6, zorder=5,
            label="Permanent DST  (CDT all year — front-runner)")
    ax.plot(x, std, color=C_STD, lw=2.6, zorder=5,
            label="Permanent standard time  (CST all year — scientist-favored)")
    ax.plot(x, cur, color=C_CURRENT, lw=2.2, ls=(0, (5, 2.5)), zorder=7,
            label="Current law  (today — switches each spring & fall)")
    ax.text(0.0, 1.10, label, transform=ax.transAxes,
            fontsize=13, fontweight="bold", color=INK)
    ax.text(0.0, 1.025, subtitle, transform=ax.transAxes, fontsize=10.5, color=INK2)
    # direct labels on the exposed segments of each proposal curve
    ax.annotate(lbl_perm[0], xy=lbl_perm[1], fontsize=9.5, fontweight="bold",
                color=C_PERM, ha=lbl_perm[2], va=lbl_perm[3])
    ax.annotate(lbl_std[0], xy=lbl_std[1], fontsize=9.5, fontweight="bold",
                color=C_STD, ha=lbl_std[2], va=lbl_std[3])
    ax.annotate("Current law", xy=lbl_cur_xy, fontsize=9.5, fontweight="bold",
                color=C_CURRENT, ha="center", va="center")

jun = mdates.date2num(dt.date(YEAR, 6, 21))
jan25 = mdates.date2num(dt.date(YEAR, 1, 25))
dec10 = mdates.date2num(dt.date(YEAR, 12, 10))
apr20 = mdates.date2num(dt.date(YEAR, 4, 20))
oct5 = mdates.date2num(dt.date(YEAR, 10, 5))

# --- Sunrise ---
three_panel(
    cx, sr_cur, sr_perm, sr_std, 4.5, 8.5,
    "SUNRISE",
    "Permanent DST = the latest sunrises (dark winter mornings) · Permanent standard time = the earliest",
    lbl_perm=("Permanent DST", (dec10, sr_perm[(dates == dt.date(YEAR, 12, 10)).argmax()] + 0.12), "center", "bottom"),
    lbl_std=("Permanent standard time", (jun, sr_std[(dates == dt.date(YEAR, 6, 21)).argmax()] - 0.12), "center", "top"),
    lbl_cur_xy=(apr20, sr_cur[(dates == dt.date(YEAR, 4, 20)).argmax()] - 0.28),
)
cx.axhline(7.0, color=MUTED, lw=1.0, ls=(0, (4, 3)), zorder=2)
cx.text(x[2], 7.04, "7:00 AM", color=INK2, fontsize=9, va="bottom", ha="left")
cx.legend(loc="upper center", bbox_to_anchor=(0.5, 0.995), frameon=False,
          fontsize=9.7, ncol=1, handlelength=2.1, labelcolor=INK2, borderaxespad=0.0)

# --- Sunset ---
three_panel(
    cs, ss_cur, ss_perm, ss_std, 16.5, 20.5,
    "SUNSET",
    "Permanent standard time = the earliest sunsets — this is the “earlier sunsets” you first pictured",
    lbl_perm=("Permanent DST", (jun, ss_perm[(dates == dt.date(YEAR, 6, 21)).argmax()] + 0.12), "center", "bottom"),
    lbl_std=("Permanent standard time", (jun, ss_std[(dates == dt.date(YEAR, 6, 21)).argmax()] - 0.12), "center", "top"),
    lbl_cur_xy=(oct5, ss_cur[(dates == dt.date(YEAR, 10, 5)).argmax()] + 0.30),
)

# Figure title block
fig3.text(0.085, 0.955, "Two Ways to “Lock the Clock” in Mississippi — and What Each Does to Your Day",
          fontsize=16, fontweight="bold", color=INK, ha="left")
fig3.text(0.085, 0.915,
          f"Current law vs. two permanent-time proposals · Jackson, MS · {YEAR} — the proposals sit one "
          "hour apart (shaded band); today’s clock hops between them.",
          fontsize=10.5, color=INK2, ha="left")
fig3.text(0.085, 0.028,
          "Permanent DST changes the WINTER; permanent standard time changes the SUMMER.  "
          "Sun position: NOAA solar equations  ·  Author: Rodney Cuevas",
          fontsize=8.5, color=MUTED, ha="left")

fig3_path = os.path.join(PLOT_DIR, "03_three_scenarios_dst_vs_standard.png")
fig3.savefig(fig3_path, dpi=200)
plt.close(fig3)
print("wrote", fig3_path)

# ----------------------------------------------------------------------------
# Console summary of the key numbers
# ----------------------------------------------------------------------------
def extremes(series):
    return hhmm(np.min(series)), hhmm(np.max(series))

# "Winter" = the days current law is on standard time. The latest sunrise of
# the whole year is NOT a winter one: under current law it falls on the first
# and last days of DST (Mar 9 / Nov 1, 07:18 CDT). Printing that on the
# "winter" line is how 7:18 got into the README tables instead of 7:03.
winter = np.array([not (DST_START_DOY <= dd.timetuple().tm_yday < DST_END_DOY)
                   for dd in dates])

print("\n=== KEY NUMBERS (", CITY, YEAR, ") ===")
print("Latest winter sunrise  | current law:", hhmm(np.max(sr_cur[winter])),
      "| permanent DST:", hhmm(np.max(sr_perm[winter])),
      "| permanent standard:", hhmm(np.max(sr_std[winter])))
print("Latest sunrise all year (current law, first/last DST days):", extremes(sr_cur)[1])
print("Earliest winter sunset | current law:", hhmm(np.min(ss_cur[winter])),
      "| permanent DST:", hhmm(np.min(ss_perm[winter])),
      "| permanent standard:", hhmm(np.min(ss_std[winter])))
print("Days sunrise after 7:00 AM  | current:", count_after(sr_cur, 7.0),
      "| permanent DST:", count_after(sr_perm, 7.0))
print("Days sunrise after 7:30 AM  | current:", count_after(sr_cur, 7.5),
      "| permanent DST:", count_after(sr_perm, 7.5))
print("Days sunset after 6:00 PM   | current:", count_after(ss_cur, 18.0),
      "| permanent DST:", count_after(ss_perm, 18.0))
print("\n--- Permanent STANDARD time (year-round CST) ---")
print("Earliest summer sunrise | current:", extremes(sr_cur)[0],
      "| permanent standard:", extremes(sr_std)[0])
print("Earliest summer sunset  | current:", extremes(ss_cur)[0],
      "| permanent standard:", extremes(ss_std)[0])
print("Latest summer sunset    | current:", extremes(ss_cur)[1],
      "| permanent standard:", extremes(ss_std)[1])

# Sanity: summer solstice (DST changes winter, standard time changes summer)
si = (dates == dt.date(YEAR, 6, 21)).argmax()
print("\nSanity — Jun 21: current sunrise", hhmm(sr_cur[si]), "sunset", hhmm(ss_cur[si]),
      "| permanent standard sunrise", hhmm(sr_std[si]), "sunset", hhmm(ss_std[si]),
      "(DST identical to current in summer)")
di = (dates == dt.date(YEAR, 12, 21)).argmax()
print("Sanity — Dec 21: current sunrise", hhmm(sr_cur[di]), "sunset", hhmm(ss_cur[di]),
      "| permanent DST sunrise", hhmm(sr_perm[di]), "sunset", hhmm(ss_perm[di]),
      "(standard time identical to current in winter)")
