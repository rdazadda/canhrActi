# canhrActi

Validated algorithms for ActiGraph accelerometer data analysis in physical activity research


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

## Analysis Workflows

### Workflow 1: (Choi + Freedson)
```r
# 
results <- canhrActi("participant.agd",
                     wear_time_algorithm = "choi",
                     intensity_algorithm = "freedson1998")
```

### Workflow 2: (Troiano + Freedson)
```r
# 
results <- canhrActi("participant.agd",
                     wear_time_algorithm = "troiano",
                     intensity_algorithm = "freedson1998")
```

### Workflow 3: CANHR Custom Algorithms
```r
# 
results <- canhrActi("participant.agd",
                     wear_time_algorithm = "CANHR2025",
                     intensity_algorithm = "CANHR")
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
