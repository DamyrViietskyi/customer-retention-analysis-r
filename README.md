# Customer Retention Analysis

**R · tidyverse · ggplot2 · lubridate**

A customer analytics project for a food-service business.

How do acquisition channels, ordering frequency, and time since the last order relate to repeat purchasing and customer value?

The analysis builds eight charts covering acquisition, repeat-purchase cohorts, average order value, spending concentration, contact availability and potentially inactive high-value customers. The at-risk label uses a rule based on spending and recency. It is a descriptive flag rather than a fitted churn model.

## Run the demo

```r
install.packages(c("tidyverse", "lubridate"))
```

Run `Rscript analysis.R` from the project root. Charts are written to `figures/`. The included CSV contains 180 synthetic customer records. Customer identifiers are fictional and contact availability is represented by flags.

## Author

I completed the entire project independently, from data preparation and analysis to the R code and all eight visualizations.
