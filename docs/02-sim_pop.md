# Simulating from linear models {#linearmod}




In this section, we will look at simulating data simple data from linear models, to familiarise ourselves with how {squidSim} works.


## `simulate_population` function

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

Each of these will be covered in more detail in the following sections. Briefly, `n` and `data_structure` refer to the size and structure of the data being simulated - `data_structure` is covered in more detail in Section \@ref(hierarchical). `parameters` is a list of parameters to be used in the simulation and is described in detail in the following parts of this section. `n_response` refers the number of response variable to be simulated and is covered in detail in the section on multivariate models (Section \@ref(multivariate)). `response_names` controls what the simulated response variables are named, and is described in the following parts of this section and Section \@ref(multivariate). `family` and `link` refer to simulating non Gaussian response variables and are covered in Section \@ref(nonGaussian). `model` allows for the specification of more complex models and is covered in Section \@ref(modeleq). `known_predictors` allows for existing data to be incorporated into the simulations and is covered in \@ref(knownpreds).

`pedigree` and `pedigree_type` relate to simulating genetic effects and are covered in Section \@ref(animal), `phylogeny` and `phylogeny_type`, relate to simulating phylogenetic effects and are covered in Section \@ref(phylogenetic)and `cov_str` relates to simulating a general covariance structure and is covered in multiple sections, including \@ref(animal), \@ref(phylogenetic), \@ref(temporalauto) and \@ref(spatialauto).

`sample_type` and `sample_param` relate to different sampling methods and are covered in Section \@ref(sampling)

`n_pop` relates to the number of populations, or datasets, that you want to simulate for each parameter set. This is covered in Section \@ref(npop).

<br>

## Simple Linear Model

<!-- <div class="panel panel-success">
<div class="panel-heading">
**Biological example**
</div>
<div class="panel-body">
We are interested in how some environmental variables affect adult body mass.  
</div>
</div> -->

We will start simulating data without any hierarchical structure, i.e. everything varies at the level of the *observation*. Let's imagine a situation where body mass is affected by some environmental variables - temperature, rainfall and wind. We can write this out in the form of a linear model: 

$$
y_i = \beta_0 + \beta_1 x_{1i} + \beta_2 x_{2i} + \beta_3 x_{3i} + \epsilon_i
$$

where each observation (denoted by the index $i$) of our response variable ($y_i$) is the sum of an intercept ($\beta_0$; value of the response when the predictor variables are all 0), the associated value of our predictor variables ($x_{1i}$, $x_{2i}$, $x_{3i}$; which also vary at the level of the observation), each with a certain magnitude and direction of their effect (effect size or slope; $\beta_1$ etc), and some unexplained, residual variation ($\epsilon_i$). 

We can write this in more compact notation, 
$$
y_{i} = \beta_0 + \boldsymbol{x}_{i} \boldsymbol{\beta} + \epsilon_{i}
$$

where $\boldsymbol{x}_{i}$ is a (row) vector of $x_{1i}$, $x_{2i}$ $x_{3i}$ etc, or equivalently row $i$ in the matrix of predictors $X$, 
$$
\boldsymbol{x}_{i} = \begin{bmatrix}
x_{1i} & x_{2i} & x_{3i}
\end{bmatrix}
$$
and $\boldsymbol{\beta}$ is a (column) vector of slopes/effect sizes
$$
\boldsymbol{\beta} = \begin{bmatrix}
\beta_1 \\
\beta_2 \\
\beta_3 
\end{bmatrix}
$$

 We will use this notation in the vignette, as it is a bit more compact, relates most easily the structure of the `simulate_population()` function, and can incorporate the flexibility needed for the different model structures.

If we want to simulate from this model, we can assume that these predictor variables are multivariate normally distributed, with given means ($\mu$) and a covariance structure ($\Sigma_x$), and the residuals are normally distributed with a given variance ($\sigma^2_\epsilon$)
$$
\boldsymbol{x}_i \sim \mathcal{N}(\boldsymbol{\mu}_x, \Sigma_x)
$$
$$
\epsilon_i \sim N(0,\sigma^2_\epsilon)
$$

where $$
\boldsymbol{\mu}_x = \begin{bmatrix}
\mu_{x_1} \\
\mu_{x_2} \\
\mu_{x_3} 
\end{bmatrix}
,
\Sigma_x = \begin{bmatrix}
\sigma^2_{x_1} & \sigma_{x_1x_2} & \sigma_{x_1x_3}\\
\sigma_{x_1x_2} & \sigma^2_{x_2} & \sigma_{x_2x_3}\\
\sigma_{x_1x_3} & \sigma_{x_2x_3} & \sigma^2_{x_3}
\end{bmatrix}
$$


<!-- 
<div class="alert alert-info">


</div>
 -->
The key to simulating data using the `squidSim` package is correctly specifying the parameters (from the equations above that would be $\beta_0$, $\boldsymbol{\beta}$, $\boldsymbol{\mu}_x$, $\Sigma_x$, $\sigma^2_\epsilon$). These parameters are given to the `simulate_population` function as a nested *list*. Within the main parameter list, there are named lists corresponding to different hierarchical levels, containing the parameters for the predictors at that level - here we are just focussing on the observation level (see Section \@ref(hierarchical) for examples with hierarchical structure). Parameters for the residual *must* be specified, all other levels are optional. In addition to the named lists relating to hierarchical levels, a vector for intercepts and a list for interactions can be added. Intercepts are demonstrated in the examples below, and interactions in Section \@ref(interactions). Many of the components in the parameter list don't need to be specified and default values will be created.

Let's simulate from the above model. 
First we can specify a sample size or data_structure. As we don't have any hierarchical data structure yet (see Section \@ref(hierarchical)), we have to specify the sample size with the `n` argument to the `simulate_population` function (e.g. 2000).
```r
simulate_population(
  n=2000,
  ...
)
```
We can also give the response ($y$) variable a name, `body_mass` (this is not needed, and defaults to `y` if not specified).
```r
simulate_population(
  n=2000,
  response_name = "body_mass",
  ...
)
```
We then need to add in our parameter list:
```r
simulate_population(
  n=2000,
  response_name = "body_mass",
  parameters = list(
    ...
  )
)
```

<!-- <mark style="background-color: lightblue">Correlated and interacting predictor variables</mark> -->

To fill in our parameter list, lets think about our model
$$
y_{i} = \color{red}{\beta_0}+ \color{blue}{\boldsymbol{x}_{i} \boldsymbol{\beta}} + \color{orange}{\epsilon_{i}}
$$
$$
\boldsymbol{x}_i \sim \mathcal{N}(\boldsymbol{\mu}_x, \Sigma_x)
$$
$$
\epsilon_i \sim N(0,\sigma^2_\epsilon)
$$
 or in words:



<span style='color: red;'>intercept</span> + <span style='color: blue;'>observation</span> level predictors + <span style='color: orange;'>residual</span>

These names correspond to names in our parameter list. To simulate our environmental predictors that vary at the level of the observation, we can use the `observation`  slot in the parameter list, as well as specifying an intercept and residual variance in the `intercept` and `residual` slots, respectively. 
The global intercept ($\beta_0$) is given by specifying an intercept vector in the parameter list e.g.

```r
    intercept=10
```

For both observation and residual we create a **list** containing the respective parameters. For the observation list, we can specify the names of these variables as a vector (these can be anything - I like giving things actual names, but could also be x1, x2 and x3) and, in the simplest case, the $\beta$ values as a vector.

```r
    observation = list(
      names = c("temperature","rainfall", "wind"),
      beta = c(0.5,-0.3, 0.4)    
    )
```
By default, these predictors are simulated as i.i.d. unit normals (mean=0, var=1, cov=0), so 
$$
\boldsymbol{\mu}_x = \begin{bmatrix}
0 \\
0 \\
0 
\end{bmatrix}
, 
\Sigma_x = \begin{bmatrix}
1 & 0 & 0\\
0 & 1 & 0\\
0 & 0 & 1
\end{bmatrix}
$$

Note that the order of the names and betas has to match. We can then specify the residual variance, here as 1 (but can be anything). `vcov` refers to the variance-covariance matrix, which for the residuals is only a single variance until we have multiple response variables (Section \@ref(multivariate)).
```r
    residual = list(
          vcov = 1
        )
```
We can then put this all together: 


```r
squid_data <- simulate_population(
  n=2000,
  response_name = "body_mass",
  parameters = list(
    intercept=10,
    observation = list(
      names = c("temperature","rainfall", "wind"),
      beta = c(0.5,-0.3, 0.4)    
    ),
    residual = list(
      vcov = 1
    )
  )
)
```

Let's compare the code back to the model:

$$
y_{i} = \color{red}{\beta_0}+ \color{blue}{\boldsymbol{x}_{i} \boldsymbol{\beta}} + \color{orange}{\epsilon_{i}}
$$
$$
\boldsymbol{x}_i \sim \mathcal{N}(\boldsymbol{\mu}_x, \Sigma_x)
$$
$$
\epsilon_i \sim N(0,\sigma^2_\epsilon)
$$


<pre><code class='language-r'><code>squid_data <- simulate_population(<br>&nbsp;&nbsp;n=2000,<br>&nbsp;&nbsp;response_name = "body_mass",<br>&nbsp;&nbsp;parameters = list(<br>&nbsp;&nbsp;&nbsp;&nbsp;<span style="color:red">intercept=10</span>,<br>&nbsp;&nbsp;&nbsp;&nbsp;<span style="color:blue">observation = list(<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;names = c("temperature","rainfall", "wind"),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;beta = c(0.5,-0.3, 0.4) &nbsp;&nbsp;&nbsp;<br>&nbsp;&nbsp;&nbsp;&nbsp;)</span>,<br>&nbsp;&nbsp;&nbsp;&nbsp;<span style="color:orange">residual = list(<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;vcov = 1<br>&nbsp;&nbsp;&nbsp;&nbsp;)</span><br>&nbsp;&nbsp;)<br>)</code></code></pre>



This generates a squid object, which when run returns a friendly message:

```r
squid_data
```

```
## Data simulated using squid 
## 
##               /\             
##              /  \            
##             / /\ \           
##             \/  \/            
##             /    \           
##             |    |          
##             |    |          
##      0      |    |      0     
##      /      \____/      \    
##     {     __/(  )\__     }   
##      \___/__\_\/_/__\___/    
##       / / / /    \ \ \ \     
##      / / / {      } \ \ \    
##     { { /   \    /   \ } }   
##     }  \     0  0     /  {   
##  0_/  { \_0        0_/ }  \_0
##        \              /      
##         }            {       
##        /              \      
##       0                0
```
 and contains all our simulation parameters as well as the simulated data. At this point we want to be able to access the simulated data. There are then some further functions which we can use to access the data and simulation parameters. We can extract the simulated data using `get_population_data()` The generated response is returned, along with simulated predictors and the data structure (not relevant here). 

```r
data <- get_population_data(squid_data)
head(data)
```

```
##   body_mass temperature   rainfall        wind   residual squid_pop
## 1  9.433268   0.1202786 -0.5196830 -1.14632338 -0.3242467         1
## 2 10.405154   0.1012308 -0.8007777  1.51713712 -0.4925495         1
## 3 10.987656  -0.1122113 -0.8444896  0.07885182  0.7588745         1
## 4  9.993290   1.0793134 -0.2973694  0.57091264 -0.8639427         1
## 5  9.355990  -1.0806266 -1.6723143 -0.43078909 -0.4330749         1
## 6 11.363373   0.4950392 -0.7938427 -0.35485322  1.0196424         1
```

Later on we will explore how to simulate data for multiple populations with the same parameters (Section \@ref(npop)). `squid_pop` is an identifier for the population number, but is not relevant here.

We can plot what we have simulated:

```r
library(scales)
par(mfrow=c(1,3))
plot(body_mass ~ temperature + rainfall + wind, data, pch=19, cex=0.5, col=alpha(1,0.5))
```

<img src="02-sim_pop_files/figure-html/lm1-1.png" width="960" />

and run a linear model to check that we get back the betas that we simulated:

```r
coef(lm(body_mass ~ temperature + rainfall + wind,data))
```

```
## (Intercept) temperature    rainfall        wind 
##  10.0270102   0.4926117  -0.2689095   0.4281766
```

We can also check the means and variances of the predictors


```r
predictors <- data[,c("temperature","rainfall","wind")]
colMeans(predictors)
```

```
## temperature    rainfall        wind 
## 0.051991960 0.012194077 0.003326721
```

```r
cov(predictors)
```

```
##              temperature     rainfall        wind
## temperature  1.007546744  0.007557108 -0.01681224
## rainfall     0.007557108  1.034793352 -0.02012959
## wind        -0.016812242 -0.020129592  0.96907758
```


Its worth noting that these values are not *exactly* what we simulated. That is to be expected - simulation involves *randomly* generating data, which means that here will be stochasticity in the simulated sample, and in our estimates of the underlying parameters.
<!-- 
We can extract the parameters we used for the simulations

```r
# get_parameters(squid_data)
```
 -->

### Adding more information about the predictors

We can also specify the predictors as having different means and variances. In the observation list, `mean` and `vcov` specify the means and covariance matrix of the predictors. If the predictors were uncorrelated, we can just specify the variances as a vector (the diagonal elements of the covariance matrix), and the function assumes the covariances are 0 (see section \@ref(corpred) for correlated predictors). Below we have three predictors, temperature, rainfall and wind, with means 10, 1 and 20 respectively, variances 1, 0.1 and 2, respectively, and betas 0.5,-3 and 0.4, a residual variance 0.8 and a global intercept of 10:

$$
y_{i} = beta_0+ \boldsymbol{x}_{i} \boldsymbol{\beta} + \epsilon_{i}
$$
$$
\boldsymbol{x}_i \sim \mathcal{N}(\boldsymbol{\mu}_x, \Sigma_x)
$$
$$
\epsilon_i \sim N(0,\sigma^2_\epsilon)
$$

$$
\color{red}{\\beta_0=10}
,
\color{blue}{\boldsymbol{\mu}_x = \begin{bmatrix}
10 \\
1 \\
20 
\end{bmatrix}}
, 
\color{CornflowerBlue}{\Sigma_x = \begin{bmatrix}
1 & 0 & 0\\
0 & 0.1 & 0\\
0 & 0 & 2
\end{bmatrix}}
,
\color{purple}{\boldsymbol{\beta} = \begin{bmatrix}
0.5 \\
-3 \\
0.4 
\end{bmatrix}}
,
\color{orange}{\sigma^2_\epsilon=0.8}
$$





<pre><code class='language-r'><code>squid_data <- simulate_population(<br>&nbsp;&nbsp;n=2000,<br>&nbsp;&nbsp;response_name = "body_mass",<br>&nbsp;&nbsp;parameters=list(<br>&nbsp;&nbsp;&nbsp;&nbsp;<span style="color:red">intercept = 10</span>,<br>&nbsp;&nbsp;&nbsp;&nbsp;observation=list(<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;names = c("temperature","rainfall", "wind"),<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style="color:blue">mean = c(10,1,20)</span>,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style="color:CornflowerBlue">vcov = c(1,0.1,2)</span>,<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style="color:purple">beta = c(0.5,-3,0.4)</span><br>&nbsp;&nbsp;&nbsp;&nbsp;),<br>&nbsp;&nbsp;&nbsp;&nbsp;residual=list(<br>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span style="color:orange">vcov = 0.8</span><br>&nbsp;&nbsp;&nbsp;&nbsp;)<br>&nbsp;&nbsp;)<br>)</code></code></pre>


```r
data <- get_population_data(squid_data)

coef(lm(body_mass ~ temperature + rainfall + wind, data))
```

```
## (Intercept) temperature    rainfall        wind 
##   9.9675354   0.4968908  -2.9504829   0.3988289
```

```r
library(scales)
par(mfrow=c(1,3))
plot(body_mass ~ temperature + rainfall + wind, data, pch=19, cex=0.5, col=alpha(1,0.5))
```

<img src="02-sim_pop_files/figure-html/lm2-1.png" width="960" />

Again, we can check that the means and variances of the predictors are being simulated as we think they should be


```r
predictors <- data[,c("temperature","rainfall","wind")]
colMeans(predictors)
```

```
## temperature    rainfall        wind 
##    9.978511    1.009647   20.020599
```

```r
cov(predictors)
```

```
##              temperature     rainfall        wind
## temperature  1.010319996 -0.002579299 -0.02075833
## rainfall    -0.002579299  0.097598486  0.01058003
## wind        -0.020758326  0.010580032  1.98499755
```

It can be complicated to keep up with how these different values combine to give the mean and variance of the response. To help with this, the `simulated_variance()` function calculates the expected mean and variance of the response variable, as well as breaking down the contribution of different predictors and hierarchical levels to the these.

```r
 simulated_variance(squid_data)
```

```
## Contribution of the simulated predictors to the mean and variance in the response
## 
## Simulated Mean: 20 
## Simulated Variance: 2.27 
## 
## Contribution of different hierarchical levels to grand mean and variance:
##             mean  var
## intercept     10 0.00
## observation   10 1.47
## residual       0 0.80
## 
## 
## Contribution of different predictors to grand mean and variance:
##             mean  var
## intercept     10 0.00
## temperature    5 0.25
## rainfall      -3 0.90
## wind           8 0.32
## residual       0 0.80
```

<!-- ![](lm_explain.pdf){#lm_explain width=100% max-height=2000px} -->







## Correlated predictors {#corpred}
We can also simulate correlations between these predictors, as `vcov` specifies the variance/covariance matrix of the predictors.


```r
squid_data <- simulate_population(
  n=2000,
  response_name = "body_mass",
  parameters=list(
    intercept=10,
    observation=list(
      names=c("temperature","rainfall", "wind"),
      mean = c(10,1 ,20),
      vcov =matrix(c(
        1, 0, 1,
        0,0.1,0,
        1, 0, 2
        ), nrow=3 ,ncol=3,byrow=TRUE),
      beta =c(0.5,-3,0.4)
    ),
    residual=list(
      vcov=1
    )
  )
)

data <- get_population_data(squid_data)

library(scales)
par(mfrow=c(1,3))
plot(body_mass ~ temperature + rainfall + wind, data, pch=19, cex=0.5, col=alpha(1,0.5))
```

<img src="02-sim_pop_files/figure-html/lm3-1.png" width="960" />

```r
coef(lm(body_mass ~ temperature + rainfall + wind, data))
```

```
## (Intercept) temperature    rainfall        wind 
##   9.6613926   0.5520228  -3.0318926   0.3924588
```

<style>
div.blue { background-color:#fcba03; border-radius: 5px; padding: 20px;}
</style>
<div class = "blue">
**Matrices in R**

To code a matrix in R we use the `matrix` function (see `?matrix`). This takes a vector of values, and arranges then in a matrix, with dimensions specified with `nrow` and `ncol`. By default it fills the matrix by column, which can be changed by specifying `byrow=TRUE`. For big matrices this can be petty annoying. The`Tri2M()` function from the package `MCMCglmm` allows you to just give the lower or upper half of the matrix, and it will fill the rest out for you. For example, we can make a correlation matrix using:

```r
Tri2M(c(1,0.5,1,0.3,0.2,1), lower.tri = FALSE, diag=TRUE)
```

```
##      [,1] [,2] [,3]
## [1,]  1.0  0.5  0.3
## [2,]  0.5  1.0  0.2
## [3,]  0.3  0.2  1.0
```

</div>



Instead of specifying a variance-covariance matrix (`vcov`), we can also specify a variance-correlation matrix (variance on the diagonals and correlations on the off-diagonals), using `vcorr`


```r
squid_data <- simulate_population(
  n=2000,
  response_name = "body_mass",
  parameters=list(
    intercept=10,
    observation=list(
      names=c("temperature","rainfall", "wind"),
      mean = c(10,1,20),
      vcorr =matrix(c(
         1,  -0.2, 0.5,
       -0.2,  0.1, 0.3,
        0.5,  0.3,  2
        ), nrow=3 ,ncol=3,byrow=TRUE),
      beta =c(0.5,-3,0.4)
    ),
    residual=list(
      vcov=1
    )
  )
)

data <- get_population_data(squid_data)

cor(data[,c("temperature","rainfall", "wind")])
```

```
##             temperature   rainfall      wind
## temperature   1.0000000 -0.1771586 0.5265241
## rainfall     -0.1771586  1.0000000 0.2964456
## wind          0.5265241  0.2964456 1.0000000
```

Through simulating correlated predictors, we can also simulate more interesting phenomena. For example, we may want to simulate the effect of a correlated missing predictor. Here, rain and wind, but not temperature, affect adult body mass, but only temperature and rainfall are measured:



```r
squid_data <- simulate_population(
  n=2000,
  response_name = "body_mass",
  parameters=list(
    intercept=10,
    observation=list(
      names=c("temperature","rainfall", "wind"),
      mean = c(10,1 ,20),
      vcov =matrix(c(
        1, 0, 1,
        0,0.1,0,
        1, 0, 2
        ), nrow=3 ,ncol=3,byrow=TRUE),
      beta =c(0.5,-3,0.4)
    ),
    residual=list(
      vcov=1
    )
  )
)

data <- get_population_data(squid_data)

library(scales)
par(mfrow=c(1,3))
plot(body_mass ~ temperature + rainfall + wind, data, pch=19, cex=0.5, col=alpha(1,0.5))
```

<img src="02-sim_pop_files/figure-html/lm5-1.png" width="960" />

```r
coef(lm(body_mass ~ temperature + rainfall, data))
```

```
## (Intercept) temperature    rainfall 
##  13.8797350   0.9172929  -3.0237208
```

```r
coef(lm(body_mass ~ temperature + rainfall + wind, data))
```

```
## (Intercept) temperature    rainfall        wind 
##   9.8925896   0.5104998  -3.0682767   0.4054890
```

We can also use this to induce measurement error in a predictor - we can simulate the true variable with a certain affect on the response, and another correlated variable - the measured variable - with no direct effect on the response. The correlation between these two variables represents the measurement error (the repeatability of the variable is the correlation squared).

<!-- measurement error - correlated variable #ffe58f-->





## Interactions and non-linear effects {#interactions}


### Interactions

$$
y_i = \beta_0 + \beta_1 * x_{1,i} + \beta_2 * x_{2,i} + \beta_3 * x_{1,i}* x_{2,i} + \epsilon_i
$$

We can specify the interaction between two predictors by adding an `interactions` list to the parameters list. Interactions can then be specified between two named variables using ":". Interactions can be between predictors at the same or different hierarchical level. 


```r
squid_data <- simulate_population(
  n=2000,
  response_name = "body_mass",
  parameters=list(
    observation=list(
      names=c("temperature","rainfall"),
      beta = c(0.5,0.3)
    ),
    residual=list(
      vcov=0.3
    ),
    interactions=list(
      names=c("temperature:rainfall"),
      beta = c(-0.1)
    )
  )
)

data <- get_population_data(squid_data)
head(data)
```

```
##     body_mass temperature    rainfall   residual temperature:rainfall squid_pop
## 1  0.61286657  -0.5130807  0.08637747  0.8390618          -0.04431861         1
## 2  0.05508649   0.0454231  0.62357042 -0.1518637           0.02832450         1
## 3 -1.47608473   0.1901009 -0.65525396 -1.3870154          -0.12456439         1
## 4  0.36916582   0.2297521 -1.34934586  0.6280920          -0.31001502         1
## 5  0.21970698   0.3136446 -0.17009161  0.1085773          -0.05334831         1
## 6 -0.57941884  -0.6966629  1.30167142 -0.7122714          -0.90682620         1
```

```r
coef(lm(body_mass ~ temperature * rainfall, data))
```

```
##          (Intercept)          temperature             rainfall 
##           0.01206604           0.51817160           0.30862876 
## temperature:rainfall 
##          -0.09310298
```

### Non-linear effects
Polynomial (quadratic, cubic, etc) functions are essentially interactions with the same predictor. They can therefore be specified in the same way:


```r
squid_data <- simulate_population(
  n=2000,
  response_name = "body_mass",
  parameters=list(
    observation=list(
      names=c("temperature"),
      beta = c(0.5)
    ),
    interactions=list(
      names=c("temperature:temperature"),
      beta = c(-0.3)
    ),
    residual=list(
      vcov=0.3
    )
  )
)
data <- get_population_data(squid_data)

plot(body_mass ~ temperature, data, pch=19, cex=0.5, col=alpha(1,0.5))
```

<img src="02-sim_pop_files/figure-html/non-linear-1.png" width="576" />

```r
coef(lm(body_mass ~ temperature + I(temperature^2), data))
```

```
##      (Intercept)      temperature I(temperature^2) 
##      -0.01455692       0.50008624      -0.29110356
```

<br>



## Transformations
We may want to simulate predictors that are not normally distributed. Although the underlying simulation procedure assumes multivariate normality, the predictors can be transformed, before they are scaled by the beta values. To do this we can provide the transformation function to the functions option of a given parameter list, as a character vector. The given function needs to be a known function in R. The below code will exponentiate rainfall (using the `exp` function), before it is scaled by its beta (here 2).


```r
squid_data <- simulate_population(
  n=2000,
  response_name = "body_mass",
  parameters=list(
    observation=list(
      names=c("temperature","rainfall"),
      functions=c(NA,"exp"),
      beta = c(0.5,0.3)
    ),
    residual=list(
      vcov=0.3
    )
  )
)

data <- get_population_data(squid_data)
head(data)
```

```
##      body_mass temperature  rainfall   residual squid_pop
## 1 -0.064509335   0.5710181 0.1999356 -0.4099991         1
## 2  2.462030755   0.6058391 2.9579145  1.2717369         1
## 3  0.006202806   0.1656373 0.2127724 -0.1404476         1
## 4  1.693172098   0.5018255 0.5597596  1.2743314         1
## 5 -0.176483495  -0.3194462 1.0456051 -0.3304419         1
## 6  0.727497591   0.4707398 0.3507821  0.3868931         1
```

```r
hist(data$rainfall, breaks=100)
```

<img src="02-sim_pop_files/figure-html/unnamed-chunk-14-1.png" width="672" />

If a covariance between variables is specified, this covariance is on the untransformed (Gaussian) scale (as the variables are simulated as multivariate normal), NOT on the transformed scale, so care should be taken with this. For example:


```r
squid_data <- simulate_population(
  n=2000,
  response_name = "body_mass",
  parameters=list(
    observation=list(
      names=c("temperature","rainfall"),
      vcov=matrix(c(1,0.7,0.7,1), nrow=2,byrow=TRUE),
      functions=c(NA,"exp"),
      beta = c(0.5,0.3)
    ),
    residual=list(
      vcov=0.3
    )
  )
)

data <- get_population_data(squid_data)

cov(data$temperature,data$rainfall)
```

```
## [1] 1.185632
```

```r
cov(data$temperature,log(data$rainfall))
```

```
## [1] 0.7230695
```
The simulate covariance can be recovered on the back-transformed predictor.

The `simulated_variance()` function will also no longer be accurate, as the calculations are based on variables on the untransformed scale.


## Known Predictors {#knownpreds}

We might have the situation where we don't want to simulate a predictor, rather use existing data to simulate a response variable from. This has the advantage that any quirks of existing data (like a strange distribution) can be maintained. These predictors can be fed into the `simulate_population()` function, using the `known_predictors` argument. This argument takes a list, with one item, called `predictors`, a matrix or dataframe of predictors and one item called `beta`, a vector with the beta values for the respective predictors. Importantly, the predictors have to be the same length as number of observations in the simulated data. We can demonstrate this using the blue tit data set that comes with the MCMCglmm package.


```r
library(MCMCglmm)
data(BTdata)

head(BTdata)
```

```
##        tarsus       back  animal     dam fosternest  hatchdate  sex
## 1 -1.89229718  1.1464212 R187142 R187557      F2102 -0.6874021  Fem
## 2  1.13610981 -0.7596521 R187154 R187559      F1902 -0.6874021 Male
## 3  0.98468946  0.1449373 R187341 R187568       A602 -0.4279814 Male
## 4  0.37900806  0.2555847 R046169 R187518      A1302 -1.4656641 Male
## 5 -0.07525299 -0.3006992 R046161 R187528      A2602 -1.4656641  Fem
## 6 -1.13519543  1.5577219 R187409 R187945      C2302  0.3502805  Fem
```
We can see that in this dataset there are several continuous predictors. Here we will use "hatchdate" and "tarsus".


```r
squid_data <- simulate_population(
  n = nrow(BTdata),
  response_name = "body_mass",
  parameters = list(
    observation =list(
      names = c("temperature","rainfall"),
      beta = c(0.5,0.3)
    ),
    residual = list(
      vcov = 0.3
    )
  ),
  known_predictors = list(
    predictors = BTdata[,c("hatchdate","tarsus")], 
    beta = c(1,2))
)

data <- get_population_data(squid_data)
head(data)
```

```
##    body_mass temperature    rainfall   residual  hatchdate      tarsus
## 1 -5.1201502  -0.3219991  0.28397662 -0.5723472 -0.6874021 -1.89229718
## 2  1.8502585   0.8333574  0.08478356 -0.1766728 -0.6874021  1.13610981
## 3  3.2626839   0.9148226  0.53157905  1.1044014 -0.4279814  0.98468946
## 4 -0.8634924   0.3699352  0.06444501 -0.3601455 -1.4656641  0.37900806
## 5  0.1270666   1.6980716  0.72127504  0.6778183 -1.4656641 -0.07525299
## 6 -2.1097562  -0.5640504 -2.01404256  0.6965921  0.3502805 -1.13519543
##   squid_pop
## 1         1
## 2         1
## 3         1
## 4         1
## 5         1
## 6         1
```

```r
plot(body_mass~hatchdate,data)
```

<img src="02-sim_pop_files/figure-html/unnamed-chunk-17-1.png" width="672" />




## Non-Gaussian phenotypes {#nonGaussian}

To simulate non-Gaussian data, we can specify a link function and a family as arguments to the simulate_population function. Underneath the predictors are being simulated as multivariate normal, and then the resulting phenotype is transformed.

<!-- 
Dave: worth pointing out that the bulk of the function is the same, but just need to add an instruction about the distribution.

Anne: I would provide here also a bit more comments on the code provided, presently it’s very dry. E.g. how is the phenotype transformed?
 -->

<div class="alert alert-info">

$$
y \sim Poisson(\hat{y})
$$
$$
\hat{y} = exp( \beta_0 +  X \beta + \epsilon )
$$
$$ X \sim MVN(\mu_x,\Sigma_x)
$$
$$
\epsilon \sim N(0,\sigma^2_\epsilon)
$$
</div>



```r
squid_data <- simulate_population(
  parameters = list(
    observation = list(
      names = c("temperature","rainfall"),
      beta = c(0.2,0.1)
    ),
    residual = list(
      mean = 1.75,
      vcov = 0.2
    )
  ),
  n = 2000,
  family = "poisson", 
  link = "log"
)

data <- get_population_data(squid_data)
head(data)
```

```
##    y temperature  rainfall  residual squid_pop
## 1 12   1.2077136  1.690710 2.0165826         1
## 2  3  -1.9996630 -2.184447 0.7536421         1
## 3  4   0.4729655  1.283041 1.3955001         1
## 4  2  -0.5997843  1.602728 1.5615546         1
## 5 11   0.8575299  1.094495 2.1075483         1
## 6  9   0.9042359  2.080520 1.6623042         1
```

```r
plot(table(data$y), ylab="Frequency", xlab="z")
```

<img src="02-sim_pop_files/figure-html/non-gaussian-1.png" width="576" />

```r
glm(y ~ temperature + rainfall, data, family="poisson")
```

```
## 
## Call:  glm(formula = y ~ temperature + rainfall, family = "poisson", 
##     data = data)
## 
## Coefficients:
## (Intercept)  temperature     rainfall  
##      1.8260       0.1707       0.1048  
## 
## Degrees of Freedom: 1999 Total (i.e. Null);  1997 Residual
## Null Deviance:	    5283 
## Residual Deviance: 4779 	AIC: 11700
```





## Model equations {#modeleq}

In all the examples so far, the predictors are simulated, scaled by their respective beta value, and then added together. We may want to prevent some of this behaviour or add in additional parameters, interactions or general complexity.  In isolation, the functionality outlined here might seem a bit redundant, but it becomes useful for more complex models. 

 <!-- provide links to examples where this is used in vignette
 -IGE
 -cyclical temporal effects
  -->

To introduce this increased complexity, we can specify a model formula. This explicitly tells the simulate_population function how to put the simulated predictors together to form the response variable. We can first demonstrate this with a simple linear model.


```r
squid_data <- simulate_population(
  parameters=list(
    observation= list(
      names = c("temperature", "rainfall"),
      beta =c(0.5,0.3)  
    ),
    residual = list(
      names="residual",
      vcov=1
    )
  ),
  n=2000,
  model = "y = temperature + rainfall + residual"
)

data <- get_population_data(squid_data)

coef(lm(y ~ temperature + rainfall, data))
```

```
## (Intercept) temperature    rainfall 
##   0.0324253   0.4540031   0.3047709
```

In the formula, we write out how the variables are added up. *Everything that you want exported needs to be defined and named* (e.g. `y=...`). By default they are all scaled by their beta values before this happens. Sometimes it is useful to prevent this (i.e. multiply two traits together without them being scaled by their respective beta) and we can do this by using `I()`.


```r
squid_data <- simulate_population(
  parameters=list(
    observation= list(
      names = c("temperature", "rainfall"),
      beta =c(0.5,0.3) 
    ),
    residual = list(
      names="residual",
      vcov=1
    )
  ),
  n=2000,
  model = "y = temperature + I(rainfall) + residual"
)

data <- get_population_data(squid_data)

coef(lm(y ~ temperature + rainfall, data))
```

```
## (Intercept) temperature    rainfall 
##  0.03598997  0.46822332  0.99910684
```

We can also add extra parameters to the parameter list, which we can call from within the function. In combination with `I()` we can then customise the model formula a bit


```r
squid_data <- simulate_population(
  parameters=list(
    observation= list(
      names = c("temperature", "rainfall"),
      beta =c(0.5,0.3),
      extra_beta = 0.1  
      ),
    residual = list(
      names="residual",
      vcov=1
      )
  ),
  n=2000,
  model = "y = temperature + extra_beta*I(rainfall) + residual"
)

data <- get_population_data(squid_data)

coef(lm(y ~ temperature + rainfall, data))
```

```
## (Intercept) temperature    rainfall 
## -0.01676897  0.52834559  0.09186402
```

Finally, we can use `[]` to index the levels of the random effects within the formula...


## Simulating multiple populations {#npop}

We can use the `simulate_population()` function to generate multiple datasets (populations) form the same set of parameters (world). To do this we can specify the `n_pop` argument in `simulate_population()`. This defaults to 1.


```r
squid_data <- simulate_population(
  n=2000,
  response_name = "body_mass",
  parameters=list(
    intercept=10,
    observation=list(
      names=c("temperature","rainfall", "wind"),
      beta =c(0.5,-0.3,0.4)
    ),
    residual=list(
      vcov=0.8
    )
  ),
  n_pop=5
)
```
By default `get_population_data` returns a data.frame, where the `squid_pop` column indicates the population

```r
data <- get_population_data(squid_data)
head(data)
```

```
##   body_mass temperature   rainfall       wind    residual squid_pop
## 1  7.058729 -1.42222411  0.7042168 1.08504214 -2.45291059         1
## 2 11.033847 -0.12042549 -1.2311494 1.76814409  0.01745728         1
## 3 10.002565  0.49804292 -0.5566153 0.81361051 -0.73888514         1
## 4  9.654366  0.50218154  0.1165662 0.09622172 -0.60024362         1
## 5 12.059080  0.44748651 -0.8722499 2.37200520  0.62485992         1
## 6  9.543231  0.07489646  2.4048953 0.34836165  0.08790699         1
```

It can also be output as a list, which might be more useful for processing many iterations of a simulation. 

```r
data <- get_population_data(squid_data, list=TRUE)
```



## Parameter list summary

The parameters list contains one (or more) list for each hierarchical level that you want to simulate at. A residual list is always need, specifying variances/covariances for the residual. Additionally, the parameter list can also be provided with an intercept vector and interactions list. 

The simplest paramter list will look something like this:
```r
parameters=list(
  residual=list(
    vcov=...
  )  
)
``` 

We can add more complexity by adding an intercept (if not specified, is assumed to be 0):
```r
parameters=list(
  intercept=c(...),  
  residual=list(
    vcov=...
  )  
)
```

and then simulate variables that vary at the observation level:
```r
parameters=list(
  intercept=c(...),
  observation=list(
    beta = ...
  ),
  residual=list(
    vcov = ...
  )  
)
```

as well as variables that vary at the other levels, for example at the level of the individual:

```r
parameters=list(
  intercept=c(...),
  individual=list(
    names = c(...),
    beta = ...
  ),
  observation=list(
    names = c(...),
    beta = ...
  ),
  residual=list(
    vcov = ...
  )  
)
```

Finally we can add in interactions:
```r
parameters=list(
  intercept=c(...),
  individual=list(
    names = c(...),
    beta = ...
  ),
  observation=list(
    names = c(...),
    beta = ...
  ),
  interactions=list(
    names = c(...),
    beta = ...
  ),
  residual=list(
    vcov = ...
  )  
)
```

For each item in the parameter list (excluding intercept, interactions, and residual), the following can be specified:

- **names** Vector containing the names of predictors from this list that will be output. This doesn't not have to be specified, unless the predictors at this level are included in interactions. By default, the names will be the name of the list (e.g. 'individual' in the example above), appended with `_effect` and a sequential number if there are multiple predictors.
- **group** Character string relates the level of variation back to the data_structure. Does not have to be specified and by default is the name of the list.
- **mean** Vector of means for the predictor variables. Defaults to 0.
- **vcov** Either a vector of variances, or a variance-covariance matrix, for the predictor variables. Defaults to identity matrix.
- **vcorr** Variance-correlation matrix, can be specified instead of vcov (it is ignored if both are specified).
- **beta** Vector (or matrix with multiple responses) of effect sizes/slopes. Defaults to 1.
- **fixed** Logical, indicating whether the effects for the levels are fixed or to be simulated. If TRUE, `beta` represents the fixed effects. Defaults to FALSE.
- **covariate** Logical, indicating whether the indexes in the data structure are to be used as a continuous variable rather than simulating one. Defaults to FALSE.
- **functions** Vector - transformation to be applied to the response variable. Defaults to 'identity'.
