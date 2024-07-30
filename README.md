# metaHelper: Transforms Statistical Measures Commonly Used for Meta-Analysis

Official Git repository of R package **metaHelper**

[![CRAN Version](https://www.r-pkg.org/badges/version/metaHelper)](https://cran.r-project.org/package=metaHelper)
[![Monthly Downloads](https://cranlogs.r-pkg.org/badges/metaHelper)](https://cranlogs.r-pkg.org/badges/metaHelper)
[![Total Downloads](https://cranlogs.r-pkg.org/badges/grand-total/metaHelper)](https://cranlogs.r-pkg.org/badges/grand-total/metaHelper)


## Authors

[Robert Emprechtinger](https://orcid.org/0000-0003-3114-9812),
[Guido Schwarzer](https://orcid.org/0000-0001-6214-9087),
Ulf Tölch,
Günther Schreder,
Gerald Gartlehner


## Description

**metaHelper** helps calculate statistical values commonly used in meta-analysis. It provides several methods to compute different forms of standardized mean differences, as well as other values such as standard errors and standard deviations. The methods used in this package are described in the following references:

- [Altman D G, Bland J M. (2011), DOI: 10.1136/bmj.d2090](https://doi.org/10.1136/bmj.d2090) 
- [Borenstein, M., Hedges, L.V., Higgins, J.P.T. and Rothstein, H.R. (2009), DOI: 10.1002/9780470743386.ch4](https://doi.org/10.1002/9780470743386.ch4)
- [Chinn S. (2000), DOI: 10.1002/1097-0258(20001130)19:22%3C3127::aid-sim784%3E3.0.co;2-m](https://doi.org/10.1002/1097-0258(20001130)19:22%3C3127::aid-sim784%3E3.0.co;2-m)
- [Cochrane Handbook (2011)](https://handbook-5-1.cochrane.org/front_page.htm)
- [Cooper, H., Hedges, L. V., & Valentine, J. C. (2009)](https://psycnet.apa.org/record/2009-05060-000)
- [Cohen, J. (1977)](https://psycnet.apa.org/record/1987-98267-000)
- [Ellis, P.D. (2009)](https://www.psychometrica.de/effect_size.html)
- [Goulet-Pelletier, J.-C., & Cousineau, D. (2018), DOI: 10.20982/tqmp.14.4.p242](https://doi.org/10.20982/tqmp.14.4.p242)
- [Hedges, L. V. (1981), DOI: 10.2307/1164588](https:/doi.org/10.2307/1164588)
- [Hedges L. V., Olkin I. (1985), DOI: 10.1016/C2009-0-03396-0](https://doi.org/10.1016/C2009-0-03396-0)
- [Murad M H, Wang Z, Zhu Y, Saadi S, Chu H, Lin L et al. (2023), DOI: 10.1136/bmj-2022-073141 ](https://doi.org/10.1136/bmj-2022-073141)
- [Mayer M (2023)](https://search.r-project.org/CRAN/refmans/confintr/html/ci_proportion.html)
- [Stackoverflow (2014)](https://stats.stackexchange.com/questions/82720/confidence-interval-around-binomial-estimate-of-0-or-1)
- [Stackoverflow (2018)](https://stats.stackexchange.com/q/338043)


## Installation

### Current stable [![CRAN Version](https://www.r-pkg.org/badges/version/metaHelper)](https://cran.r-project.org/package=metaHelper) release:
```r
install.packages("metaHelper")
```

### Current GitHub release:

Installation using R package
[**remotes**](https://cran.r-project.org/package=remotes):
```r
install.packages("remotes")
remotes::install_github("RobertEmprechtinger/metaHelper")
```

### Bug Reports:

```r
bug.report(package = "metaHelper")
```

The bug.report function is not supported in RStudio. Please send an email to Robert Emprechtinger <<emprechtinger@stateofhealth.at>> if you use RStudio.

You can also report bugs on GitHub under [Issues](https://github.com/RobertEmprechtinger/metaHelper/issues/).
