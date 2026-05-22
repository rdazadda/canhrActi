# canhrActi

Comprehensive analysis of ActiGraph accelerometer data for physical activity, sleep, and circadian rhythm research. Developed by the Center for Alaska Native Health Research.

<strong>Use canhrActi:</strong> &nbsp;
[Web app](https://rdazadda-canhracti.share.connect.posit.cloud/) &nbsp;||&nbsp;
[Windows download](https://github.com/rdazadda/canhrActi/releases/latest) &nbsp;||&nbsp;
[macOS download](https://github.com/rdazadda/canhrActi/releases/latest) &nbsp;||&nbsp;
[Linux download](https://github.com/rdazadda/canhrActi/releases/latest) &nbsp;||&nbsp;
[R package](#r-package)
<br/>

<strong>(See installation instructions below.)</strong>

canhrActi reads ActiGraph `.agd` files and computes wear time, activity intensity classifications, MVPA, energy expenditure, sleep periods, and circadian rhythm metrics. Output is compatible with ActiLife software. The same analysis pipeline is available three ways: as a hosted web app, as a desktop installer that bundles R, or as an R package for scripting.

## Features

- Wear time detection — Troiano 2007, Choi 2011, CANHR 2025
- Activity intensity cut-points — Freedson, Sasaki VM3, Crouter, Evenson, Puyau, and others
- Sleep scoring — Cole-Kripke and Sadeh, with Tudor-Locke period detection
- MET prediction — twelve published algorithms
- Circadian metrics — L5, M10, IS, IV, RA, phi, SRI
- Sedentary fragmentation — ASTP, SATP, Gini, Clauset power-law exponent
- Interactive Shiny dashboard

## Quick Start

```r
library(canhrActi)

results <- canhrActi("participant.agd")
print(results$daily_summary)

sleep <- canhrActi.sleep("participant.agd")

run_dashboard()
```

## Installation

### Web app

Open <https://rdazadda-canhracti.share.connect.posit.cloud/> in any modern browser. Nothing to install.

### Windows

1. Download `canhrActi-Setup-<version>.exe` from the [releases page](https://github.com/rdazadda/canhrActi/releases).
2. Double-click to install. Windows SmartScreen may show a warning the first time — click "More info" then "Run anyway".
3. Launch canhrActi from the Start menu.

### macOS (Apple Silicon)

1. Download `canhrActi-<version>-mac-arm64.dmg` from the [releases page](https://github.com/rdazadda/canhrActi/releases).
2. Open the .dmg and drag canhrActi to Applications.
3. The first time you open it, right-click the icon in Applications and pick **Open** — this approves the unsigned app with Gatekeeper.

### Linux

1. Download `canhrActi-<version>.AppImage` from the [releases page](https://github.com/rdazadda/canhrActi/releases).
2. Make it executable: `chmod +x canhrActi-*.AppImage`
3. Run it: `./canhrActi-*.AppImage`

### R package

```r
remotes::install_github("rdazadda/canhrActi")
```

Requires R >= 4.1. The desktop installers above bundle R for you; install the package directly only if you want to script canhrActi from your own R session.

## Citation

```
Azadda, R.D., Grogan-Kaylor, A., & Lee, K. (2026). canhrActi:
  Comprehensive Accelerometer Data Analysis for Physical Activity and Sleep Research.
  R package version 0.3.0. https://github.com/rdazadda/canhrActi
```

## Contact

- Email: rdazadda@alaska.edu
- Issues: <https://github.com/rdazadda/canhrActi/issues>

---

Center for Alaska Native Health Research (CANHR)
