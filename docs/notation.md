---
title: squidSim Notation
---

Small letters (e.g. $x$) denote scalars

Bold, small letters (e.g. $\boldsymbol{x}$) denote vectors

Capital letters (e.g. $X$) denote matrices

| <span style="display: inline-block; width:10px">Letter/Symbol </span>  |  Usage                              |
|:-----------------|:------------------------------------|
|$\sigma$          | standard deviation                  |
|$\Sigma$          | covariance matrix                   |
|$\mu$             | mean                                |
|$\epsilon$        | residual                            |
|$\beta$           | slope                               |
|$x$               | predictors                          |
|                  |                                     |


Three ways to write out an equation for a linear model. First we can write out all the different variables:

$y_i = \beta_0 + \beta_1 x_{1,i} + \beta_2 x_{2,i} + \beta_3 x_{3,i} + \epsilon_i$


where each observation (denoted by the index $i$) of our response variable ($y_i$) is the sum of an intercept ($\beta_0$; value of the response when the predictor variables are all 0), the associated value of our predictor variables ($x_{1i}$, $x_{2i}$, $x_{3i}$; which also vary at the level of the observation), each with a certain magnitude and direction of their effect (effect size or slope; $\beta_1$ etc), and some unexplained, residual variation ($\epsilon_i$). 

Second, We can also write this in matrix notation:

$\boldsymbol{y} = X\boldsymbol{\beta} + \boldsymbol{\epsilon}$

where $X$ is a matrix of predictors and $\boldsymbol{\beta}$ is a (column) vector of slopes/effect sizes. This matrix notation is a bit more compact and relates most easily the structure of the `simulate_population()` function. However it becomes more complex when we have things varying at different levels, as we have to start getting design matrices for the random effects involved e.g.

$\boldsymbol{y} = X\boldsymbol{\beta} + Z\boldsymbol{u} + \boldsymbol{\epsilon}$

which I think we would rather avoid (and has little relation to the squidSim code).


We can therefore combine the index and matrix notation. This is maybe a little more complex, but it's compact and flexible and still relates well to the `simulate_population()` function.

$y_{i} = \beta_0 + \boldsymbol{x}_{i} \boldsymbol{\beta} + \epsilon_{i}$

where $\boldsymbol{x}_{i}$ is the $i$th row of $X$. I have deliberately separated $\beta_0$ out here, for the purpose of comparing with the structure of the code. Then for models that have predictors vary at different levels we can have 

$y_{ij} = \beta_0 + \boldsymbol{x}_{i} \boldsymbol{\beta}_x + \boldsymbol{u}_j \boldsymbol{\beta}_u + \epsilon_{ij}$

Its not completely clear to me if $\boldsymbol{x}_{i}$ should be $\boldsymbol{x}_{ij}$? Instead of having predictors at different levels, we might have 'random effects'

$y_{ij} = \beta_0 + \boldsymbol{x}_{i} \boldsymbol{\beta}_x + u_j + \epsilon_{ij}$

at multiple levels

$y_{ijk} = \beta_0 + \boldsymbol{x}_{i} \boldsymbol{\beta}_x + u_j + w_k + \epsilon_{ijk}$

or a mixture of predictors and random effects

$y_{ij} = \beta_0 + \boldsymbol{x}_{i} \boldsymbol{\beta}_x + \boldsymbol{u}_j \boldsymbol{\beta}_u + w_j + \epsilon_{ij}$

Having $u$ and $w$ varying at the same level to me implies that dont covary, which they could. So something like 

$y_{ij} = \beta_0 + \boldsymbol{x}_{i} \boldsymbol{\beta}_x + \boldsymbol{u}_j \boldsymbol{\beta}_u + u_{0j} + \epsilon_{ij}$

might work - individual intercepts at that level as well as predictors? Also 


### Distributions

We can write distribution equations as:

$x_{1i} \sim \mathcal{N}(\mu, \sigma^2)$

or

$\boldsymbol{x}_i \sim \mathcal{N}(\boldsymbol{\mu}_x, \Sigma_x)$

or

$\boldsymbol{\epsilon} \sim \mathcal{N}(\boldsymbol{\mu}_x, \boldsymbol{\sigma}_\epsilon \text{I})$




### Random regression

$y_{ij} = \beta_0 + \beta_1x_{i} + u_{1j} + x_{i}u_{2j} + \epsilon_{ij}$

$x_{i} \sim \mathcal{N}(\mu_x, \sigma^2_x)$

$\boldsymbol{u}_i \sim \mathcal{N}(0, \Sigma_u)$

$\epsilon_{i} \sim \mathcal{N}(0, \sigma^2_{\epsilon})$


### Multivariate

$\boldsymbol{y}_{ij} = \boldsymbol{\beta}_0 + \boldsymbol{x}_{i} B_x + \boldsymbol{u}_j + \boldsymbol{\epsilon}_{ij}$

$\boldsymbol{x}_i \sim \mathcal{N}(\boldsymbol{\mu}_x, \Sigma_x)$

$\boldsymbol{u}_i \sim \mathcal{N}(0, \Sigma_u)$

$\boldsymbol{\epsilon}_{i} \sim \mathcal{N}(0, \Sigma_{\epsilon})$

where $\boldsymbol{\beta}_0$ is a vector of intercepts of length q (number of responses) $B$ is a $p*q$ (p - number of predictors) matrix of $\beta$s









<pre><code class='language-r'><code>squid_data <- simulate_population(<br>&nbsp;&nbsp;n=2000,<br>&nbsp;&nbsp;response_name = "body_mass",<br>&nbsp;&nbsp;parameters = list(<br>&nbsp;&nbsp;&nbsp;&nbsp;<span style="background-color:#ffff7f">intercept</span>=10,<br>&nbsp;&nbsp;&nbsp;&nbsp;observation = list(<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;names = c("temperature","rainfall", "wind"),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;beta = c(0.5,-0.3, 0.4) &nbsp;&nbsp;&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;),<br>&nbsp;&nbsp;&nbsp;&nbsp;residual = list(<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;vcov = 1<br>&nbsp;&nbsp;&nbsp;&nbsp;)<br>&nbsp;&nbsp;)<br>)</code></code></pre>
