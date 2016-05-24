# Replication material for: 'Factor-based forecasting in the presence of outliers: Are factors better selected and estimated by the median than the mean?'
## Johannes Tang Kristensen.


[Link to the paper](http://dx.doi.org/10.1515/snde-2012-0049)

---

This repository contains the material necessary to replicate the empirical __application__ in: 'Factor-based forecasting in the presence of outliers: Are factors better selected and estimated by the median than the mean?' The code is a partial re-write
of the code originally released with the paper. Since the original version of the code changes in the __KFAS__ package
has caused the code for the Kalman-filter-based benchmark model included in the paper to break. I have, therefore,
not included that model here. However, the original code for the model can be found at the link above.

### Required packages 

The __forecastexp__ package is required for the estimation and the __macrods__ package
provides the data functions:

```r
library('devtools')
install_github('johannestang/forecastexp')
install_github('johannestang/macrods')
```

In addition the following packages should be installed : _pryr_, and _xtable_, both available from CRAN.   

Note: Total runtime is approximately 5 hours when using a 20 core machine (2 E5-2680 v2 CPUs @ 2.8 GHz). 

### /app

+ __forecastapp.R__ estimates the models and produces all output. 
+ __simple.R__ function for forecasting using simple means/medians.
+ __FRED.rda__ the dataset. 

