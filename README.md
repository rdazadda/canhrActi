# canhrActi

Validated algorithms for ActiGraph accelerometer data analysis in physical activity research

## Overview

**`canhrActi`** implements scientifically validated algorithms for processing ActiGraph accelerometer data with automated analysis of physical activity intensity, wear time detection, and batch processing capabilities.

## Installation

```r
# From GitHub
remotes::install_github("rdazadda/canhrActi")

# From source
install.packages("path/to/canhrActi_0.2.0.tar.gz", repos = NULL, type = "source")
```

**Requirements**: R ≥ 4.1 and packages: `gsignal`, `DBI`, `RSQLite`

## Quick Start

```r
library(canhrActi)

# Single file analysis
results <- canhrActi("participant001.agd")
print(results$overall_summary)
print(results$intensity_summary)
print(results$daily_summary)

# Export to ActiLife format
export_canhrActi(results, output_dir = "reports")

# Batch processing
batch <- canhrActi("C:/My Data Folder")
print(batch$summary)
```

## Examples of Workflows

### Workflow 1: Standard Analysis (Choi + Freedson)
```r
# Gold standard for adult accelerometry
results <- canhrActi("participant.agd",
                     wear_time_algorithm = "choi",
                     intensity_algorithm = "freedson1998")
```

### Workflow 2: Conservative Analysis (Troiano + Freedson)
```r
# NHANES methodology
results <- canhrActi("participant.agd",
                     wear_time_algorithm = "troiano",
                     intensity_algorithm = "freedson1998")
```

### Workflow 3: CANHR Custom Algorithms
```r
# Population-specific adjustments
results <- canhrActi("participant.agd",
                     wear_time_algorithm = "CANHR2025",
                     intensity_algorithm = "CANHR")
```

## Algorithm Specifications

### Activity Counts
ActiGraph official algorithm (Neishabouri et al., 2022)

### Wear Time Detection

**Choi (2011)** - Default
- 90-minute zero window
- 2-minute spike tolerance (≤100 counts)
- 30-minute upstream/downstream validation

**Troiano (2007)** - NHANES standard
- 60-minute zero window
- 2-minute spike tolerance (≤100 counts)

**CANHR 2025** - Extended parameters
- 120-minute zero window
- 3-minute spike tolerance (≤150 counts)
- 45-minute upstream/downstream validation

### Intensity Classification

**Freedson (1998)** - Adult standard (axis 1)
- Sedentary: 0-99 CPM
- Light: 100-1951 CPM
- Moderate: 1952-5724 CPM
- Vigorous: 5725-9498 CPM
- Very Vigorous: ≥9499 CPM

**CANHR** - Custom cutpoints
- Sedentary: 0-150 CPM
- Light: 151-2200 CPM
- Moderate: 2201-6000 CPM
- Vigorous: 6001-10000 CPM
- Very Vigorous: ≥10001 CPM

## Output Structure

### Single File Results
- `overall_summary` - Total statistics across all valid days
- `intensity_summary` - Distribution of intensity levels
- `daily_summary` - Per-day wear time and activity breakdown
- `subject_info` - Participant demographics
- `epoch_data` - Minute-by-minute data
- `valid_days` - Dates meeting minimum wear time
- `parameters` - Analysis settings

### Batch Processing Results
- `summary` - Combined table (ActiLife-compatible)
- `participants` - Individual results for each participant
- `n_participants` - Number successfully analyzed
- `group_stats` - Group-level statistics

## Learn More

- [Installation Guide](https://github.com/rdazadda/canhrActi#installation)
- [Function Reference](https://github.com/rdazadda/canhrActi/tree/main/man)

## Citation

If you use **`canhrActi`** in your research, please cite:

```
Azadda, R.D., Grogan-Kaylor, A., & Lee, K. (2025). canhrActi:
  Validated Algorithms for ActiGraph Accelerometer Data Analysis.
  R package version 0.2.0. https://github.com/rdazadda/canhrActi
```

## References

- Neishabouri, A., Nguyen, J., Samuelsson, J., et al. (2022). Quantification of acceleration as activity counts in ActiGraph wearable. *Scientific Reports*, 12(1):11958.

- Choi, L., Liu, Z., Matthews, C.E., & Buchowski, M.S. (2011). Validation of accelerometer wear and nonwear time classification algorithm. *Medicine & Science in Sports & Exercise*, 43(2):357-364.

- Troiano, R.P., Berrigan, D., Dodd, K.W., et al. (2008). Physical activity in the United States measured by accelerometer. *Medicine & Science in Sports & Exercise*, 40(1):181-188.

- Freedson, P.S., Melanson, E., & Sirard, J. (1998). Calibration of the Computer Science and Applications, Inc. accelerometer. *Medicine & Science in Sports & Exercise*, 30(5):777-781.

## Acknowledgments

This package was developed by the Numbers Team at the Center for Alaska Native Health Research (CANHR), University of Alaska Fairbanks.

## Contact

- **Email**: rdazadda@alaska.edu
- **Report Issues**: [GitHub Issues](https://github.com/rdazadda/canhrActi/issues)

## License

MIT License - see [LICENSE.md](LICENSE.md)

---

**Affiliation**: Center for Alaska Native Health Research (CANHR)
**Institution**: University of Alaska Fairbanks
