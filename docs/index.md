--- 
title: "The {squidSim} R Package Vignette"
author: "Joel Pick"
date: "2024-12-11"
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
Starting with simulations can seem like a daunting task. The {squidSim} R package is designed to facilitate that transition, and to focus attention on the data structure and parameters needed for simulation, rather than programming knowledge.

{squidSim} also provides a useful tool for experienced programmers. One problematic aspect of collaborative coding (or reviewing someone else's code) is that many people have very contrasting programming styles. A major motivation for the {squidSim} package is that it provides a consistent framework for simulations, which can be interpreted by many people rather than having to decipher someone's personal code. 


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
It would be great if you could report any suggestions, issues or bugs; [here](https://github.com/squidgroup/squidSim/issues) for issues relating to the package, and [here](https://github.com/squidgroup/squidSim_vignette/issues) for issues relating to this vignette. It is worth checking to see if anyone else has a similar problem first, and adding comments to their issue, before starting a new one. 






## `simulate_population()` function {-}

The heart of the {squidSim} R package is the `simulate_population()` function, which we can use to simulate hierarchical, population level data. We provide the function with a set of parameters, a hierarchical data structure (if we are simulating hierarchical data), and various other optional arguments, which are listed below. 

The `simulate_population()` function simulates predictors at each hierarchical level, using provided mean and variance-covariance (vcov) parameters, from a multivariate normal distribution. These predictors are then scaled by the beta parameters, and added together to create the response. The arguments that can be provided to the `simulate_population()` function (along with their defaults) are:

```r
simulate_population(
  data_structure, 
  n, 
  parameters, 
  n_response=1, 
  response_names,
  family="gaussian", 
  link="identity", 
  model, 
  known_predictors, 
  pedigree, 
  pedigree_type, 
  phylogeny, 
  phylogeny_type, 
  cov_str, 
  sample_type,
  sample_param,
  n_pop=1
)
```

Each of these will be covered in more detail in the following sections. Briefly, `n` and `data_structure` refer to the size and structure of the data being simulated - `data_structure` is covered in more detail in Section \@ref(hierarchical). `parameters` is a list of parameters to be used in the simulation and is described in detail in Section \@ref(linearmod). `n_response` refers the number of response variable to be simulated and is covered in detail in the section on multivariate models (Section \@ref(multivariate)). `response_names` controls what the simulated response variables are named, and is described in Sections \@ref(linearmod) and \@ref(multivariate). `family` and `link` refer to simulating non Gaussian response variables and are covered in Section \@ref(nonGaussian). `model` allows for the specification of more complex models and is covered in Section \@ref(modeleq). `known_predictors` allows for existing data to be incorporated into the simulations and is covered in \@ref(knownpreds).

`pedigree` and `pedigree_type` relate to simulating genetic effects and are covered in Section \@ref(animal), `phylogeny` and `phylogeny_type`, relate to simulating phylogenetic effects and are covered in Section \@ref(phylogenetic) and `cov_str` relates to simulating a general covariance structure and is covered in multiple sections, including \@ref(animal), \@ref(phylogenetic), \@ref(temporalauto) and \@ref(spatialauto).

`sample_type` and `sample_param` relate to different sampling methods and are covered in Section \@ref(sampling)

`n_pop` relates to the number of populations, or datasets, that you want to simulate for each parameter set. This is covered in Section \@ref(npop).

<br>

## Terminology and notation {-}

We try to be consistent as possible throughout the vignette with our terminology and notation. 


{squidSim} refers to an R package

`simulate_population()` refers to a function

`parameters` refers more generally to code



## Mathematical Notation {-}

We try to use a consistent notation in equations throughout the manuscript, which we try to explain as we go. For the sake of clarity we have outlined everything here.

### General rules {-}
Small letters (e.g. $x$) denote scalars

Bold, small letters (e.g. $\boldsymbol{x}$) denote vectors

Capital letters (e.g. $X$) denote matrices

| <span style="display: inline-block; width:120px">Letter/Symbol </span>  |  Usage                              |
|:-----------------|:------------------------------------|
|$\sigma$          | standard deviation                  |
|$\Sigma$          | covariance matrix                   |
|$\sigma_{x_1x_2}$ | covariance between variables x1 and x2|
|$\mu$             | mean                                |
|$\epsilon$        | residual                            |
|$\beta$           | slope                               |
|$x$               | predictor variable                  |
|$y$               | response variable                   |
|                  |                                     |


### Notation for a linear mixed model {-}

There are several ways to write out an equation for a linear model. First we can write out all the different variables:

$y_i = \beta_0 + \beta_1 x_{1,i} + \beta_2 x_{2,i} + \beta_3 x_{3,i} + \epsilon_i$

where each observation (denoted by the index $i$) of our response variable ($y_i$) is the sum of an intercept ($\beta_0$; value of the response when the predictor variables are all 0), the associated value of our predictor variables ($x_{1i}$, $x_{2i}$, $x_{3i}$; which also vary at the level of the observation), each with a certain magnitude and direction of their effect (effect size or slope; $\beta_1$ etc), and some unexplained, residual variation ($\epsilon_i$). 

We can also write this in matrix notation:

$\boldsymbol{y} = X\boldsymbol{\beta} + \boldsymbol{\epsilon}$

where $X$ is a matrix of predictors and $\boldsymbol{\beta}$ is a (column) vector of slopes/effect sizes. This matrix notation is a bit more compact and relates most easily the structure of the `simulate_population()` function. However it becomes more complex when we have things varying at different levels, as we have to start getting design matrices for the random effects involved e.g.

$\boldsymbol{y} = X\boldsymbol{\beta} + Z\boldsymbol{u} + \boldsymbol{\epsilon}$

which we would rather avoid here as it has little relation to the squidSim code.

We can therefore combine the index and matrix notation. This is maybe a little more complex, but it's compact and flexible and relates well to the `simulate_population()` function.

$y_{i} = \beta_0 + \boldsymbol{x}_{i} \boldsymbol{\beta} + \epsilon_{i}$

where $\boldsymbol{x}_{i}$ is the $i$th row of $X$. I have deliberately separated the intercept ($\beta_0$) out here, for the purpose of comparing with the structure of `simulate_population()`. Then for models that have predictors vary at different levels we can have 

$y_{ij} = \beta_0 + \boldsymbol{x}_{i} \boldsymbol{\beta}_x + \boldsymbol{u}_j \boldsymbol{\beta}_u + \epsilon_{ij}$

<!-- Its not completely clear to me if $\boldsymbol{x}_{i}$ should be $\boldsymbol{x}_{ij}$?  -->
Instead of having predictors at different levels, we might have 'random effects'

$y_{ij} = \beta_0 + \boldsymbol{x}_{i} \boldsymbol{\beta}_x + u_j + \epsilon_{ij}$

at multiple levels

$y_{ijk} = \beta_0 + \boldsymbol{x}_{i} \boldsymbol{\beta}_x + u_j + w_k + \epsilon_{ijk}$

<!-- or a mixture of predictors and random effects

$y_{ij} = \beta_0 + \boldsymbol{x}_{i} \boldsymbol{\beta}_x + \boldsymbol{u}_j \boldsymbol{\beta}_u + w_j + \epsilon_{ij}$

Having $u$ and $w$ varying at the same level to me implies that dont covary, which they could. So something like 

$y_{ij} = \beta_0 + \boldsymbol{x}_{i} \boldsymbol{\beta}_x + \boldsymbol{u}_j \boldsymbol{\beta}_u + u_{0j} + \epsilon_{ij}$

might work - individual intercepts at that level as well as predictors? Also 
 -->

### Distributions {-}

We can write distribution equations as:

$x_{1i} \sim \mathcal{N}(\mu, \sigma^2)$

or

$\boldsymbol{x}_i \sim \mathcal{N}(\boldsymbol{\mu}_x, \Sigma_x)$

<!-- or

$\boldsymbol{x} \sim \mathcal{N}(\boldsymbol{\mu}_x, \boldsymbol{\sigma}_\x \text{I})$

 -->


### Interactions / Random regression {-}

$y_{ij} = \beta_0 + \beta_1x_{i} + u_{1j} + x_{i}u_{2j} + \epsilon_{ij}$

$x_{i} \sim \mathcal{N}(\mu_x, \sigma^2_x)$

$\boldsymbol{u}_j \sim \mathcal{N}(0, \Sigma_u)$

$\epsilon_{ij} \sim \mathcal{N}(0, \sigma^2_{\epsilon})$


### Multi-response {-}

$\boldsymbol{y}_{ij} = \boldsymbol{\beta}_0 + \boldsymbol{x}_{i} B_x + \boldsymbol{u}_j + \boldsymbol{\epsilon}_{ij}$

$\boldsymbol{x}_i \sim \mathcal{N}(\boldsymbol{\mu}_x, \Sigma_x)$

$\boldsymbol{u}_j \sim \mathcal{N}(0, \Sigma_u)$

$\boldsymbol{\epsilon}_{ij} \sim \mathcal{N}(0, \Sigma_{\epsilon})$

where $\boldsymbol{\beta}_0$ is a vector of intercepts of length q (number of responses) $B$ is a $p*q$ (p - number of predictors) matrix of $\beta$s

