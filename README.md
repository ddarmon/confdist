confdist is an R package for performing inference using [confidence functions](https://www.stat.rutgers.edu/home/mxie/RCPapers/insr.12000.pdf).

# Catalog of Functions

* `t.conf()` for a single mean and the difference of two means (paired and unpaired).
* `prop.conf()` for a single binomial proportion.
* `var.conf()` for a single variance.
* `exp.conf()` for an exponential rate.

# Installation

To install confdist, run

```
install.packages('devtools') # Only if you haven't installed devtools already

devtools::install_github(ddarmon/confdist)
```

# Usage

The functions in confdist are modeled after base R functions like `t.test()`, `prop.test()`, etc.

```{r}
library(confdist)

# Construct confidence functions for a population mean.

conf <- t.conf(x = c(-2, -1, 0, 1, 2))

conf$pconf(0) # Right-sided P-value for mu = 0

1 - conf$pconf(0) # Left-sided P-value for mu = 0

1 - conf$cconf(0) # Two-sided P-value for mu = 0

conf$qconf(c(0.025, 0.975)) # 95% confidence interval for mu
```

# References

* Tore Schweder and Nils Lid Hjort. *Confidence, Likelihood, Probability*. Cambridge University Press, 2016.

* Kesar Singh, Minge Xie, and William E. Strawderman. "Confidence distribution (CD) -- distribution estimator of a parameter." *Complex Datasets and Inverse Problems*. Institute of Mathematical Statistics, 2007. 132-150. [Reprint](https://projecteuclid.org/download/pdf_1/euclid.lnms/1196794948)

* Min‐ge Xie and Kesar Singh. "Confidence distribution, the frequentist distribution estimator of a parameter: A review." *International Statistical Review* 81.1 (2013): 3-39. [Reprint](https://www.stat.rutgers.edu/home/mxie/RCPapers/insr.12000.pdf)