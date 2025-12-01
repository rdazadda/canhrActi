# canhrActi

Comprehensive accelerometer data analysis for physical activity, sleep, and circadian rhythm research.

Developed by the **Center for Alaska Native Health Research (CANHR)**, University of Alaska Fairbanks.

## Features

- **Physical Activity**: Wear time detection, intensity classification, MVPA, energy expenditure
- **Sleep Analysis**: Cole-Kripke & Sadeh algorithms with sleep period detection
- **Circadian Rhythm**: L5, M10, IS, IV, RA, and cosinor analysis
- **Multi-Device Support**: ActiGraph (.agd, .gt3x), GENEActiv, Axivity, CSV
- **Interactive Dashboard**: Shiny-based GUI for easy analysis
- **ActiLife Compatible**: Produces results compatible with ActiLife software

## Installation

```r
remotes::install_github("rdazadda/canhrActi")
```

## Quick Start

```r
library(canhrActi)

# Analyze accelerometer data
results <- canhrActi("participant.agd")
print(results$overall_summary)
print(results$daily_summary)

# Sleep analysis
sleep <- canhrActi.sleep("participant.agd")

# Circadian rhythm analysis
results <- canhrActi("participant.agd", calculate_circadian = TRUE)
print(results$circadian)

# Launch interactive dashboard
run_dashboard()
```

## Citation

```
Azadda, R.D., Grogan-Kaylor, A., & Lee, K. (2025). canhrActi:
  Comprehensive Accelerometer Data Analysis for Physical Activity and Sleep Research.
  R package version 0.3.0. https://github.com/rdazadda/canhrActi
```

## Contact

- **Email**: rdazadda@alaska.edu
- **Issues**: [GitHub Issues](https://github.com/rdazadda/canhrActi/issues)

---

**Center for Alaska Native Health Research (CANHR)** | University of Alaska Fairbanks
