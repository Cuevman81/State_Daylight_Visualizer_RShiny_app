# Celestial Almanac Dashboard
An R Shiny App that generates comprehensive celestial visualizations for any U.S. location. It calculates and plots solar, lunar, and astronomical phenomena, including daylight curves, moon phases, dark sky windows, and photography light periods.

<img width="1968" height="2586" alt="image" src="https://github.com/user-attachments/assets/ddde3665-3b51-4974-bcfd-a1c45da2e7f4" />

---

## About This Project
This application provides a comprehensive, interactive almanac of celestial data. What began as a simple daylight visualizer has evolved into a full-featured dashboard that integrates solar, lunar, and regional trends with specialized tools for astronomers and photographers.

## Features

### 1. Solar Analysis
*   **Annual Daylight Curves**: Compare daylight hours at specific latitudes.
*   **Sunrise & Sunset**: Daily times with automatic Daylight Saving Time (DST) detection.
*   **Equation of Time**: Visualize the deviation between solar noon and clock noon.
*   **Daylight Symmetry**: See the near-perfect symmetry of daylight around the summer solstice.
*   **Twilight Exploration**: Detailed durations for Civil and Nautical twilight.

### 2. Lunar Analysis
*   **Phase Calendar**: Monthly moon phase visualization with traditional full moon names.
*   **Lunar Events**: Tracking for Supermoons, Micro-moons, Blue Moons, and Black Moons.
*   **Illumination & Distance**: Daily Earth-Moon distance and illumination percentages.
*   **Visibility Schedule**: Exact moonrise and moonset times for your specific location.

### 3. Specialized Tools
*   **Sky Watcher**: Dark sky observation windows (nautical dusk-to-dawn) filtered by moon interference.
*   **Photography Planner**: Ideal windows for Golden Hour and Blue Hour photography.

### 4. Regional Trends
*   **Daylight Heatmaps**: State-wide grids showing the monthly change in daylight minutes.
*   **Latitudinal Comparison**: Compare Annual Daylight Curves for North, Center, and South points within a state.

## Technical Highlights
*   **Interactive Mapping**: Click anywhere on the map to analyze coordinates or select a state for regional data.
*   **Parallel Processing**: High-resolution state grids are calculated using the `{furrr}` package for maximum speed.
*   **Persistent Caching**: Calculations are cached to `.rds` files, making repeated analysis near-instant.
*   **Modern UI**: Built with `{bslib}` using the "Cyborg" theme for a sleek, dark-mode experience.

## How to Run Locally
### Prerequisites
-   R (version 4.1 or newer recommended)
-   RStudio

### Installation & Setup
1.  **Clone the repository:**
    ```bash
    git clone https://github.com/Cuevman81/State_Daylight_Visualizer_RShiny_app.git
    cd State_Daylight_Visualizer_RShiny_app
    ```
2.  **Install required packages:**
    ```R
    install.packages(c("shiny", "bslib", "bsicons", "thematic", "suncalc", "ggplot2", 
                       "dplyr", "lubridate", "sf", "maps", "metR", "tidyr", "purrr", 
                       "ggrepel", "lutz", "tools", "furrr", "leaflet", "emojifont", 
                       "DT", "glue", "shinycssloaders"))
    ```
3.  **Run the app:**
    Open the `app.R` file in RStudio and click **Run App**. The `cache/` directory will be created automatically on first run.

## Credits 
-   **Author:** Rodney Cuevas 
-   **Data Source:** `{suncalc}` R Package
