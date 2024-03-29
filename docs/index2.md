--- 
title: "The {squidSim} R Package Vignette"
author: "Joel Pick"
date: "2022-09-20"
site: bookdown::bookdown_site
output: bookdown::gitbook
documentclass: book
biblio-style: apalike
link-citations: yes
github-repo: squidgroup/squidSim
description: "This is a vignette for using the squidSim R package."
---


# The {squidSim} R package {.unnumbered #intro}

The {squidSim} R package is designed to simplify data simulation from a highly flexible set of models, including:

- Correlated and interacting predictor variables
- Non-Gaussian response variables (Poisson and binomial)
- Crossed and nested hierarchical structures
- Random intercepts and slopes
- Univariate and multivariate data
- Within level-specific residual variance (DHGLMs)
- Additive genetic effects (animal models)
- Phylogenetic effects with different models of evolution
- Temporal and spatial autocorrelation
- Missing data (MNAR, MAR and MCAR)
- Temporal sampling

The main idea is that anything you can model using a linear mixed effect model framework (assuming underlying multivariate normality) you can simulate using the {squidSim} package.

<!-- ### SQuID ethos {-}

Create population and then sample from it
<br>

<div style="float: width: 40%; text-align: center;" >
  Make Hierarchical Structure<br>
         ↓<br>
    Simulate data<br>
         ↓<br>
     Sample Data<br>
</div>
<br>
 -->

### Why use {squidSim}? {-}
For people confident with programming in R, simulating data doesn't provide a huge challenge. However, to those less confident with programming, or the process of data simulation more generally, starting with simulations can seem like a daunting task. The {squidSim} R package is designed to facilitate that transition, and to focus attention on the data structure and parameters needed for simulation, rather than the programming knowledge.

{squidSim} also provides a useful tool for experienced programmers. One problematic aspect of collaborative coding (or reviewing someone else's code) is that many people have very contrasting programming styles. Another motivation for the {squidSim} package is that it provides a consistent framework for simulations, which can be interpreted by many people rather than having to decipher someone's personal code. 


### Using the vignette {-}
If you are new to using the {squidSim} package, we recommend that you read Sections \@ref(linearmod) and \@ref(hierarchical) to familiarise yourself with the {squidSim} package before moving onto the more advanced topics. The later sections assume an certain level of understanding of how the functions work. 

The vignette assumes that you have a working knowledge of R, in particular being comfortable using vectors, matrices and lists.

<!-- 
## Functionality

This is a summary of the desired functionality laid out in the different SQuID working group documents. 

|     |     |
| --- | --- |
| No limit in the number of predictors  | ✅ |
| Interactions between predictors | ✅ |
| Temporal effects | |
|    - Linear/non-linear/cyclical| ✅ |
|    - Temporal autocorrelation| ❌ |
| Non-Gaussian data | ✅ |
| IxE / random slopes | ✅ |
| Within level-specific residual variance (DGLMM) | ✅ |
|     - Equation on residual variance | ✅ |
| No limit in the number of responses | ✅ |
| Recursive relationships | ❌ |
| Environmental dependency of cov matrices | Needs Discussion |
| Additive genetic effects | ✅ |
|     - Including indirect genetic effects | ✅ |
| G,E correlations | In Progress |
| Phylogenetic effects | In Progress |
|     - Different models of evolution | ❌ |

<br> 
-->


### Installation {-}

The {squidSim} package is currently only available on github:
```r
devtools::install_github("squidgroup/squidSim")
library(squidSim)
```

### Issues and bugs {-}
It would be great if you could report any suggestions, issues or bugs using github issues https://github.com/squidgroup/squidSim/issues. It is worth checking to see if anyone else has a similar problem first, and adding comments to their issue, before starting a new one. 




