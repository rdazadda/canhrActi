# canhrActi

Validated algorithms for ActiGraph accelerometer data analysis in physical activity and sleep research

**Supports both raw .gt3x files and pre-processed .agd files!**


## Installation

```r
# From GitHub
remotes::install_github("rdazadda/canhrActi")

# From source
install.packages("path/to/canhrActi_0.2.0.tar.gz", repos = NULL, type = "source")
```

**Requirements**: R ≥ 4.1 and packages: `gsignal`, `DBI`, `RSQLite`, `read.gt3x`

## Quick Start

```r
library(canhrActi)

# Single file analysis (.agd or .gt3x - auto-detected!)
results <- canhrActi("participant001.agd")  # Pre-processed data
# OR
results <- canhrActi("participant001.gt3x")  # Raw acceleration data

print(results$overall_summary)
print(results$intensity_summary)
print(results$daily_summary)

# Export to ActiLife format
export_canhrActi(results, output_dir = "reports")

# Batch processing (automatically finds both .agd and .gt3x files)
batch <- canhrActi("C:/My Data Folder")
print(batch$summary)

# Sleep analysis (works with both file types)
sleep <- canhrActi.sleep("participant001.agd")
# OR
sleep <- canhrActi.sleep("participant001.gt3x")
print(sleep$sleep_periods)
```

## File Format Support

### .agd Files (Pre-processed Data)
- Exported from ActiLife software
- Contains pre-calculated activity counts
- Smaller file size, faster to process
- Requires ActiLife software to create

### .gt3x Files (Raw Acceleration Data)
- Direct output from ActiGraph devices
- Contains raw X, Y, Z acceleration (30-100 Hz)
- **No ActiLife software needed!**
- Larger files, slightly slower processing
- Provides access to unfiltered data for custom analysis

**Note:** Both file types produce identical results when using the same algorithms. The package automatically detects which type you're using!

## Analysis Workflows

### Workflow 1: Complete Analysis with METs & Energy Expenditure
```r
# Most comprehensive - includes METs and kcal calculations
results <- canhrActi("participant.agd",
                     wear_time_algorithm = "choi",
                     intensity_algorithm = "freedson1998",
                     calculate_mets = TRUE,
                     mets_algorithm = "freedson.vm3")

print(results$overall_summary)
print(results$mets_summary)          # Average METs, total kcal
print(results$energy_expenditure_summary)  # kcal by intensity
```

### Workflow 2: Crouter Algorithm for Mixed Activities
```r
# Best for free-living activities (walking + household tasks)
results <- canhrActi("participant.agd",
                     wear_time_algorithm = "choi",
                     intensity_algorithm = "freedson1998",
                     calculate_mets = TRUE,
                     mets_algorithm = "crouter")  # Distinguishes locomotion vs lifestyle
```

### Workflow 3: CANHR Custom Algorithms
```r
# CANHR-specific algorithms for specialized populations
results <- canhrActi("participant.agd",
                     wear_time_algorithm = "CANHR2025",
                     intensity_algorithm = "CANHR",
                     calculate_mets = TRUE,
                     mets_algorithm = "freedson.vm3")
```

## Sleep Analysis

### Sleep Scoring Algorithms

```r
# Cole-Kripke algorithm (adults 35-65 years)
sleep <- canhrActi.sleep("participant.agd",
                         sleep_algorithm = "cole.kripke")

# Sadeh algorithm (children/adolescents 10-25 years)
sleep <- canhrActi.sleep("child.agd",
                         sleep_algorithm = "sadeh")

# View sleep periods and metrics
print(sleep$sleep_periods)
```

### Batch Sleep Analysis

```r
# Process entire folder
batch <- canhrActi.sleep("C:/Sleep Study Data",
                         sleep_algorithm = "cole.kripke",
                         output_dir = "sleep_results")
print(batch$summary)
```

**Documentation:** https://actigraphcorp.my.site.com/support/s/article/What-is-Batch-Sleep-and-how-does-it-work

## METs & Energy Expenditure

### Metabolic Equivalent of Task (METs) Algorithms

Calculate METs and energy expenditure (kcal) using validated prediction equations:

```r
results <- canhrActi("participant.agd",
                     calculate_mets = TRUE,
                     mets_algorithm = "freedson.vm3")

# View METs and kcal summaries
print(results$mets_summary)                   # Average METs, total kcal
print(results$energy_expenditure_summary)     # kcal breakdown by intensity
print(results$daily_summary$total_kcal)       # Daily kcal expenditure
```

### Available METs Algorithms

**Freedson VM3** (default, recommended):
- Formula: METs = 0.000863 × VM + 0.668876
- Validated: Treadmill walking/running (Sasaki et al., 2011)
- Best for: General adult activities, tri-axial data

**Crouter 2-Regression**:
- Uses coefficient of variation to distinguish activity types
- Locomotion (walking) vs lifestyle activities (household tasks)
- Best for: Mixed free-living activities
- Reference: Crouter et al. (2010)

**Freedson Vertical Axis**:
- Formula: METs = 1.439008 + (0.000795 × counts/min)
- Original 1998 algorithm for single-axis comparison
- Reference: Freedson et al. (1998)

**Swartz**:
- Formula: METs = 2.606 + (0.0006863 × counts/min)
- Validated on 70 participants, 28 activities (lifestyle + sports)
- Reference: Swartz et al. (2000)

### Energy Expenditure Calculation

Converts METs to kilocalories using standard formula:

**kcal/min = METs × 3.5 × body mass (kg) / 200**

Body mass is automatically extracted from file metadata. Results include:
- Total energy expenditure (kcal)
- Energy expenditure by intensity level (sedentary, light, moderate, vigorous)
- Daily and hourly kcal summaries

```r
# Example: Compare METs algorithms
vm3 <- canhrActi("file.agd", mets_algorithm = "freedson.vm3")
crouter <- canhrActi("file.agd", mets_algorithm = "crouter")

cat("Freedson VM3:", vm3$mets_summary$total_kcal, "kcal\n")
cat("Crouter:", crouter$mets_summary$total_kcal, "kcal\n")
```


## Citation

If you use **`canhrActi`** in your research, please cite:

```
Azadda, R.D., Grogan-Kaylor, A., & Lee, K. (2025). canhrActi:
  Validated Algorithms for ActiGraph Accelerometer Data Analysis.
  R package version 0.2.0. https://github.com/rdazadda/canhrActi
```

## References

- Neishabouri, A., Nguyen, J., Samuelsson, J. et al. Quantification of acceleration as activity counts in ActiGraph wearable. Sci Rep 12, 11958 (2022). https://doi.org/10.1038/s41598-022-16003-x


## Acknowledgments
This package was developed by the Numbers Team at the Center for Alaska Native Health Research (CANHR), University of Alaska Fairbanks.

## Contact

- **Email**: rdazadda@alaska.edu
- **Report Issues**: [GitHub Issues](https://github.com/rdazadda/canhrActi/issues)


---

**Affiliation**: Center for Alaska Native Health Research (CANHR)
**Institution**: University of Alaska Fairbanks
