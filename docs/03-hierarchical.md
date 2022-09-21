# Hierarchical structure {#hierarchical}

There are two parts to simulating hierarchical data. First you need to have a hierarchical data structure and second you need parameters at each of the different hierarchical levels. The data structure is essentially a data.frame (or matrix), with all the grouping factors and their levels, as we would see in a typical dataset. Lets take the blue tit dataset we explored earlier:


```r
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
Here `animal`, `dam`, `fosternest` and `sex` make up the data structure.

In this Section, we will first demonstrate how to make a simple hierarchical structure using the `make_structure` function. `simulate_population` also allows pre-existing data structures to be incorporated into simulations. The remaining part of the section details how to simulate hierarchical data once you have a hierarchical data structure. 



## Making a hierarchical structure {#makestr}

We can use the `make_structure` function to create nested and crossed hierarchical data structures. The `make_structure` function only produces balanced data structures, but these can be made unbalanced by sampling, which is outlined in Section \@ref(sampling)



### Single Factor

Simplest structure - one grouping factor with multiple observations. Here we create a structure with 2 repeated observations of 5 individuals (small number are used here simply for illustration purposes). The `structure` contains the name of the grouping factors and their sample sizes, and `repeat_obs` is the number of repeated observations.

```r
make_structure(structure="individual(5)", repeat_obs=2)
```

```
##    individual
## 1           1
## 2           1
## 3           2
## 4           2
## 5           3
## 6           3
## 7           4
## 8           4
## 9           5
## 10          5
```


### Nested factors
If we want to have nested factors, so different hierarchical groups, where levels of one group only exist in one higher group then we can use the `/` symbol in the `structure` argument. For example, here we have 2 sexes, each with 5 individuals, with 2 repeated measurements each. 

```r
make_structure(structure="sex(2)/individual(5)", repeat_obs=2)
```

```
##    sex individual
## 1    1          1
## 2    1          1
## 3    1          2
## 4    1          2
## 5    1          3
## 6    1          3
## 7    1          4
## 8    1          4
## 9    1          5
## 10   1          5
## 11   2          6
## 12   2          6
## 13   2          7
## 14   2          7
## 15   2          8
## 16   2          8
## 17   2          9
## 18   2          9
## 19   2         10
## 20   2         10
```
<!-- In this case we have created a nested structure with 2000 observations - 2 observations for each of 1000 individuals, 500 for each of 2 sexes. We can then use this to simulate variation at different levels. Note that sample sizes are extracted from the data structure and so we do not need to specify `N` in the `simulate_population` function as above.
 -->
<img src="03-hierarchical_files/figure-html/unnamed-chunk-6-1.png" width="576" />
Note that in the nesting, the sample size for the lower group now represents the number within each level of the higher, rather than the total sample size, so overall there is 10 individuals. 

We can nest as much as we want:

```r
make_structure(structure="species(2)/population(2)/individual(2)", repeat_obs=2)
```

```
##    species population individual
## 1        1          1          1
## 2        1          1          1
## 3        1          1          2
## 4        1          1          2
## 5        1          2          3
## 6        1          2          3
## 7        1          2          4
## 8        1          2          4
## 9        2          3          5
## 10       2          3          5
## 11       2          3          6
## 12       2          3          6
## 13       2          4          7
## 14       2          4          7
## 15       2          4          8
## 16       2          4          8
```

<img src="03-hierarchical_files/figure-html/unnamed-chunk-8-1.png" width="576" />


### Crossed factors
We can create completely crossed factors - every combination of levels exists - using the `+` symbol in the `structure` argument

```r
make_structure(structure="treatment(2) + individual(5)", repeat_obs=1)
```

```
##    treatment individual
## 1          1          1
## 2          1          2
## 3          1          3
## 4          1          4
## 5          1          5
## 6          2          1
## 7          2          2
## 8          2          3
## 9          2          4
## 10         2          5
```

We can combine crossed and nested structures:

```r
make_structure(structure="treatment(2) + sex(2)/individual(5)", repeat_obs=1)
```

```
##    treatment sex individual
## 1          1   1          1
## 2          1   1          2
## 3          1   1          3
## 4          1   1          4
## 5          1   1          5
## 6          1   2          6
## 7          1   2          7
## 8          1   2          8
## 9          1   2          9
## 10         1   2         10
## 11         2   1          1
## 12         2   1          2
## 13         2   1          3
## 14         2   1          4
## 15         2   1          5
## 16         2   2          6
## 17         2   2          7
## 18         2   2          8
## 19         2   2          9
## 20         2   2         10
```

We can also output the crossed and nested using `:`

```r
make_structure(structure="treatment(2) + individual(5) + treatment:individual", repeat_obs=1)
```

```
##    treatment individual treatment_individual
## 1          1          1                    1
## 2          1          2                    2
## 3          1          3                    3
## 4          1          4                    4
## 5          1          5                    5
## 6          2          1                    6
## 7          2          2                    7
## 8          2          3                    8
## 9          2          4                    9
## 10         2          5                   10
```


### Temporal structure

```r
ds <- make_structure(structure="year(2)/month(12)/day(30)", repeat_obs=1)
head(ds)
```

```
##   year month day
## 1    1     1   1
## 2    1     1   2
## 3    1     1   3
## 4    1     1   4
## 5    1     1   5
## 6    1     1   6
```


```r
ds <- make_structure(structure="year(2) + month(12) + day(30) + year:month:day", repeat_obs=1)
head(ds)
```

```
##   year month day year_month_day
## 1    1     1   1              1
## 2    1     1   2              2
## 3    1     1   3              3
## 4    1     1   4              4
## 5    1     1   5              5
## 6    1     1   6              6
```

### Naming factor levels
Rather than just outputting 1 - N levels for the levels names of each factor, we might want to assign names. 

This can be done for all or some of the grouping factors, using the `level_names` argument. We can input a list, with an item in the list for each grouping factor we want to assign names, and then a vector of their names, which is the same length of the number of levels in that grouping factor. For example, below we just assign names to the two sexes:


```r
make_structure(structure="sex(2)/individual(5)", repeat_obs=2,  level_names=list(sex=c("female","male")))
```

```
##       sex individual
## 1  female          1
## 2  female          1
## 3  female          2
## 4  female          2
## 5  female          3
## 6  female          3
## 7  female          4
## 8  female          4
## 9  female          5
## 10 female          5
## 11   male          6
## 12   male          6
## 13   male          7
## 14   male          7
## 15   male          8
## 16   male          8
## 17   male          9
## 18   male          9
## 19   male         10
## 20   male         10
```

And then to the individuals and the sexes

```r
make_structure(structure="sex(2)/individual(5)", repeat_obs=2,  level_names=list(sex=c("female","male"),individual=paste0("ind_",1:10)))
```

```
##       sex individual
## 1  female      ind_1
## 2  female      ind_1
## 3  female      ind_2
## 4  female      ind_2
## 5  female      ind_3
## 6  female      ind_3
## 7  female      ind_4
## 8  female      ind_4
## 9  female      ind_5
## 10 female      ind_5
## 11   male      ind_6
## 12   male      ind_6
## 13   male      ind_7
## 14   male      ind_7
## 15   male      ind_8
## 16   male      ind_8
## 17   male      ind_9
## 18   male      ind_9
## 19   male     ind_10
## 20   male     ind_10
```

<!-- ```{r}
dat_str <- make_structure(structure="species5(2)/population(2)/individual(10)", repeat_obs=3)

plot_nested(dat_str)

structure="species(2)/population5(2)/individual(10)"
structure <- gsub("\\s","",structure)
gsub(".\\)(.)","\\1",structure)
strsplit((gsub("\\w+\\(\\d+\\)","",structure)),"")[[1]]



```
 -->




## Factors
In the first sections, we just simulated continuous predictors, varying at the level of the observation. However, we may want to simulate factors with known or *fixed* effects (i.e. not variables drawn randomly from a particular distribution) at different levels, such as sex or treatment effects.

The first thing we want to do is specify a simple data structure, for example 100 observations for each of two sexes: 


```r
ds <- make_structure(structure="sex(2)", repeat_obs=100,  level_names=list(sex=c("female","male")))
```

Then we feed this data structure into the `simulate_population()` function using the `data_structure` argument. Note that we no longer need to specify the sample size (`n`), as this is taken from the number of rows in the data_structure.

```r

squid_data <- simulate_population(
  data_structure = ds,
  parameters = ...
)

```

In order to tell the parameter list we have effects that vary at different hierarchical levels, we can create additional slots in the parameter list for the grouping factors, so now it will look something like:

```r

squid_data <- simulate_population(
  data_structure = ds,
  parameters = list(
    intercept = ...,
    sex = list(
      ...
    ),
    observation = list(
      ...
    ),
    residual = list(
      ...
    )
  )
)
```
The names in the parameter list that relate to the different grouping factors either need to match the name in the data structure exactly (as above) or a 'group' argument needs to be given e.g.

```r
squid_data <- simulate_population(
  data_structure = ds,
  parameters = list(
    intercept = ...,
    anything = list(
      group="sex,"
      ...
    ),
    observation = list(
      ...
    ),
    residual = list(
      ...
    )
  )
)
```

We then need to tell the parameters list that we have fixed effects for this grouping factor, in other words we know the difference in body size between the sexes is 0.5, for example. To do this we specify `fixed = TRUE`. 

```r
squid_data <- simulate_population(
  data_structure = ds,
  parameters = list(
    intercept = ...,
    anything = list(
      group="sex,"
      ...
    ),
    observation = list(
      ...
    ),
    residual = list(
      ...
    )
  )
)
```

We can then give a beta for all the different levels of that group. Note that there are two ways to specify this, as there also is in linear models in R. First, we can specify an intercept, and contrasts, equivalent to the output of `lm(body_mass~sex)`, which involves specifying the beta for the first level as 0 to make it the baseline level (or any other level that you would like to be the baseline).


```r
squid_data <- simulate_population(
  data_structure = ds,
  parameters = list(
    intercept= 10,
    sex=list(
      fixed=TRUE,
      beta=c(0,0.5)
    ),
    residual = list(
      vcov = 0.5
    )
  )
)

data <- get_population_data(squid_data)

boxplot( y ~ factor(sex), data)
```

<img src="03-hierarchical_files/figure-html/unnamed-chunk-17-1.png" width="672" />

```r
lm( y ~ factor(sex), data)
```

```
## 
## Call:
## lm(formula = y ~ factor(sex), data = data)
## 
## Coefficients:
##     (Intercept)  factor(sex)male  
##         10.0952           0.4053
```

Alternately, we can specify no intercept (which defaults to 0), and the means for the two levels as betas( equivalent to `lm(body_mass~0+sex)`):

```r
squid_data <- simulate_population(
  data_structure = ds,
  parameters = list(
    sex=list(
      fixed=TRUE,
      beta=c(10,10.5)
    ),
    residual = list(
      vcov = 0.5
    )
  )
)

data <- get_population_data(squid_data)

boxplot( y ~ factor(sex), data)
```

<img src="03-hierarchical_files/figure-html/unnamed-chunk-18-1.png" width="672" />

```r
lm( y ~ factor(sex), data)
```

```
## 
## Call:
## lm(formula = y ~ factor(sex), data = data)
## 
## Coefficients:
##     (Intercept)  factor(sex)male  
##         10.0668           0.6218
```

```r
lm( y ~ 0+factor(sex), data)
```

```
## 
## Call:
## lm(formula = y ~ 0 + factor(sex), data = data)
## 
## Coefficients:
## factor(sex)female    factor(sex)male  
##             10.07              10.69
```

We would recommend the former methods, as this makes things clearer if other factors are simulated.

Finally We can also name the levels: 
THIS SECTION NEEDS TO BE COMPLETED AS THERE IS A POTENTIAL BUG HERE


Lets take the example of sex. Factors can be thought of as a hierarchical structure. First we can create a data structure using the 


```r
squid_data <- simulate_population(
  data_structure = make_structure(structure = "sex(2)/individual(500)", repeat_obs=2),
  parameters = list(
    intercept= 10,
    sex=list(
      fixed=TRUE,
      names=c("female","male"),
      beta=c(0,0.5)
    ),
    residual = list(
      vcov = 0.5
    )
  )
)


data <- get_population_data(squid_data)

boxplot( y ~ factor(sex), data)
```

<img src="03-hierarchical_files/figure-html/unnamed-chunk-19-1.png" width="576" />

```r
lm( y ~ factor(sex), data)
```

```
## 
## Call:
## lm(formula = y ~ factor(sex), data = data)
## 
## Coefficients:
##  (Intercept)  factor(sex)2  
##       9.9966        0.5117
```


### Fixed Factor Interactions

We might want to simulate an interaction between a continuous predictor and a factor, for example the effect of the environment varying between two sexes. Specifying this using `simulate_population()` is similar to interactions between two continuous predictors that we have previously encountered (Section \@ref(interactions)). In the `interaction` part of the parameter list, we now specify the contrasts between the slopes for environment, using the names that we have assigned the different levels. In the simulation below, males are larger, and have a larger environment slope:



```r
squid_data <- simulate_population(
  data_structure = make_structure(structure = "sex(2)", repeat_obs=1000),
  parameters = list(
    intercept=10,
    sex=list(
      fixed=TRUE,
      names=c("female","male"),
      beta=c(0,0.5)
    ),
    observation= list(
      names = c("environment"),
      beta =c(0.2)
      ),
    interactions = list(
      names=c("environment:male"),
      beta = 0.4
      ),
    residual = list(
      names="residual",
      vcov = 0.1
    )
  )
)

data <- get_population_data(squid_data)
head(data)
```

```
##           y female male environment    residual environment:male sex squid_pop
## 1  9.759894      1    0  -0.8837062 -0.06336431                0   1         1
## 2 10.013148      1    0  -0.6369115  0.14053016                0   1         1
## 3 10.114682      1    0  -0.5319145  0.22106476                0   1         1
## 4 10.569754      1    0   1.4651492  0.27672402                0   1         1
## 5 10.695371      1    0  -0.1998853  0.73534792                0   1         1
## 6  9.688524      1    0  -1.1348902 -0.08449844                0   1         1
```

```r
plot(y~environment,data, pch=19, col=scales::alpha(c(1,2),0.5)[factor(data$sex)])
```

<img src="03-hierarchical_files/figure-html/unnamed-chunk-20-1.png" width="576" />

```r
lm( y ~ 0 + factor(sex)*environment, data)
```

```
## 
## Call:
## lm(formula = y ~ 0 + factor(sex) * environment, data = data)
## 
## Coefficients:
##             factor(sex)1              factor(sex)2               environment  
##                   9.9933                   10.4936                    0.2180  
## factor(sex)2:environment  
##                   0.3928
```


## Simulating predictors at different hierarchical levels
<!-- <div class="panel panel-success">
<div class="panel-heading">
**Biological example**
</div>
<div class="panel-body">
We have taken repeated measurements of adult body mass.  
</div>
</div> -->
As well as simulating  continuous predictors at the level of the observation, we can also simulate predictors at different hierarchical levels. Lets take the example of a situation where we have repeated measures of individuals. The individuals have traits that are consistently expressed, whilst the environment varies between observations. We can describe variation at these different hierarchical levels as:

<div class="alert alert-info">

$$
y_i = \beta_0 +  Z_j beta_Z +  X_i \beta_X + \epsilon_{i}
$$
$$
 X \sim MVN(\mu_X,\Sigma_X)
$$
$$
 Z \sim MVN(\mu_Z,\Sigma_Z)
$$
$$
\epsilon \sim N(0,\sigma^2_\epsilon)
$$
</div>

where $Z$ is a matrix of predictors that vary at the individual level (denoted by the subscript $j$), and $X$ is a matrix of predictors at the observation level (denoted by the index $i$).

In order to simulate from this model, we need a data structure and parameters for each of these levels. To do this, we can either specify a data structure generated using `make_structure` (outlined previously in Section \@ref(makestr)), or a pre-existing data structure, to the `simulate_population` function. We then add in a item to the parameter list, the name of which matches on of the grouping factors in the data structure, and specify the parameters for predictors that vary at that level in the same way as outlined in the previous section (\@ref(linearmod)). This is similar to the fixed factors above, but we are now assuming that the variable is drawn randomly from a distribution, rather than the effects at each level being fixed. 


Lets imagine that we simulate behaviour, that is a functions of an individual's size and physiology, and also varies in response to the environment, here temperature and rainfall:

```r
squid_data <- simulate_population(
  data_structure = make_structure(structure = "individual(500)", repeat_obs=2),
  parameters = list(
    individual = list(
      names = c("size","physiology"),
      beta = c(0.1,0.2)
    ),
    observation = list(
      names=c("temperature","rainfall"),
      beta = c(0.2,-0.1)
    ),
    residual = list(
      vcov = 0.5
    )
  ),
  response_names="behaviour"
)

data <- get_population_data(squid_data)

coef(lm(behaviour ~ size + physiology + temperature + rainfall , data))
```

```
## (Intercept)        size  physiology temperature    rainfall 
## -0.01779512  0.06565936  0.23307232  0.19809783 -0.13753134
```

Here, we have simulated 4 predictors, 'size' and 'physiology' that vary at the level of the individual, and 'temperature' and 'rainfall' that vary at the level of the observation. To keep things simple, we will simulate them all as unit normal variables (mean=0 and variance=1). Note, **the names of the different grouping factors in the parameter list needs to exactly match those in the data structure**. The order does not, however, have to be the same. There are circumstances in which we may want to simulate two sets of effects at the same hierarchical level (for example see permanent environment effects in Section \@ref(va)), in this case we can call them different things in the parameter list, but link them back to the grouping factor, by providing a `group` name. For example the following will produce the same simulation as above:


```r
squid_data <- simulate_population(
  data_structure = make_structure(structure = "individual(500)", repeat_obs=2),
  parameters = list(
    ind1 = list(
      group="individual",
      names = c("size"),
      beta = c(0.1)
    ),
    ind2 = list(
      group="individual",
      names = c("physiology"),
      beta = c(0.2)
    ),
    observation = list(
      names=c("temperature","rainfall"),
      beta = c(0.2,-0.1)
    ),
    residual = list(
      vcov = 0.5
    )
  ),
  response_names="behaviour"
)
```

It is also worth noting that predictors do not have to be simulated for every grouping factor in the data structure - in this way no variation at that level can be simulated.

### Simulating 'random' effects

In essence, random effects (random intercepts) represent an unobserved/latent predictor (or group of predictors), which varies at a given hierarchical level. In a mixed effect model, the effect at each level of the grouping factor is unknown, and estimated by the model (and assumed to come from a normal distribution). When simulating this, however, we can simply simulate an additional predictor at a particular hierarchical level ($z$) with mean 0 and a given variance ($\sigma^2_z$). 

<div class="alert alert-info">

$$
y_{i} = \beta_0 + z_j + \epsilon_{i}
$$
$$
z \sim N(0,\sigma^2_z)
$$
$$
\epsilon \sim N(0,\sigma^2_\epsilon)
$$
</div>

For example we can simulate some between-individual variation as follows:


```r
squid_data <- simulate_population(
  data_structure = make_structure(structure = "individual(500)", repeat_obs=2),
  parameters = list(
    individual = list(
      vcov = 0.5
    ),
    residual = list(
      vcov = 0.5
    )
  )
)

data <- get_population_data(squid_data)
head(data)
```

```
##             y individual_effect   residual individual squid_pop
## 1  0.59746130        -0.1668517  0.7643130          1         1
## 2  1.26488279        -0.1668517  1.4317345          1         1
## 3  0.52135595        -1.0677339  1.5890899          2         1
## 4 -0.09546876        -1.0677339  0.9722651          2         1
## 5 -0.31773464         0.6450222 -0.9627568          3         1
## 6  0.43129125         0.6450222 -0.2137309          3         1
```

```r
library(lme4)
short_summary <- function(x) print(summary(x), correlation=FALSE, show.resids=FALSE, ranef.comp = c("Variance"))

short_summary(lmer(y ~ 1 + (1|individual), data))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: y ~ 1 + (1 | individual)
##    Data: data
## 
## REML criterion at convergence: 2652.1
## 
## Random effects:
##  Groups     Name        Variance
##  individual (Intercept) 0.4516  
##  Residual               0.4910  
## Number of obs: 1000, groups:  individual, 500
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept) -0.01032    0.03734  -0.276
```

Note that here we haven't specified any variable names. In this case the simulated predictors are named by the grouping factors (e.g. individual_effects).

### Incorporating existing data structures

We could also use an existing data structure, taking the grouping factors and levels from an existing dataset and input them to `simulate_population`. To demonstrate this, we can use the [blue tit dataset](https://rdrr.io/cran/MCMCglmm/man/BTdata.html) provided with the MCMCglmm package. This is a dataset with some continuous variables (tarsus, back (coloration) and hatchdate), and some grouping factors (animal, dam, fosternest and sex), the latter providing a data structure from which to simulate.


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

```r
squid_data <- simulate_population(
  data_structure = BTdata[,c("dam","fosternest")],
  parameters = list(
    dam = list(
      vcov = 0.2
    ),
    fosternest = list(
      vcov = 0.3
    ),
    residual = list(
      vcov = 0.5
    )
  )
)

data <- get_population_data(squid_data)
data
```

```
##                y  dam_effect fosternest_effect      residual     dam fosternest
## 1   -0.577555780 -0.24379862      -0.878409512  5.446523e-01 R187557      F2102
## 2   -0.271100778 -0.11381696      -0.182272429  2.498861e-02 R187559      F1902
## 3   -1.908758311 -0.19191731      -1.007119760 -7.097212e-01 R187568       A602
## 4    0.773303291  0.11074157       0.194364795  4.681969e-01 R187518      A1302
## 5   -0.432063690  0.13123132      -0.219941080 -3.433539e-01 R187528      A2602
## 6    0.429609068  0.64559470      -0.514959327  2.989737e-01 R187945      C2302
## 7   -1.220987856 -0.03138890      -0.430645382 -7.589536e-01    Fem3      C1902
## 8    0.951413080  0.26843304       1.024413152 -3.414331e-01 R187030      C1302
## 9   -0.101886589 -0.53760055       0.351259258  8.445470e-02 R187517       C602
## 10   0.777632618  0.15557758       0.602413189  1.964185e-02 R187523      B2202
## 11  -0.315044867 -0.46941454       0.076790110  7.757957e-02 R186902      B1402
## 12   0.057466436 -0.41891386       0.364817341  1.115630e-01 R187400      B1002
## 13  -1.006087458 -0.24959309      -0.422358557 -3.341358e-01 R187932       B502
## 14  -0.333279162 -0.01126390      -1.107582959  7.855677e-01 R187582      D1202
## 15  -0.280047486  0.01666119       0.488172848 -7.848815e-01 R187545      D1002
## 16  -0.187642087 -0.10710580       0.683757102 -7.642934e-01 R187546       D902
## 17  -1.369148745  0.13954234      -1.038501969 -4.701891e-01 R187590       D202
## 18   1.914559816  0.44105306       0.443790283  1.029716e+00 R187548       E902
## 19   1.038044263  0.08368727      -0.009623114  9.639801e-01 R187594       E302
## 20   0.271372045 -0.40939224       0.050193201  6.305711e-01 R187588      F2402
## 21  -0.635058394 -0.24379862      -0.878409512  4.871497e-01 R187557      F2102
## 22  -0.579781238 -0.11381696      -0.182272429 -2.836918e-01 R187559      F1902
## 23   0.960295473  0.05790010       0.460580990  4.418144e-01 R187531      F1702
## 24  -1.288256728 -0.80854693       0.534437805 -1.014148e+00 R187592      F1102
## 25  -0.035605550  0.19087364      -0.019208663 -2.072705e-01 R187575       F902
## 26   0.700947320  0.30961413       0.217220378  1.741128e-01 R186912       F102
## 27  -0.915099439 -0.61878375       0.243359432 -5.396751e-01 R187914      G1202
## 28  -0.141530514  0.11989873       0.605015873 -8.664451e-01 R187955       G602
## 29  -1.601549060 -0.61490699      -0.054012138 -9.326299e-01 R187535       G102
## 30  -0.305516116 -0.31030764      -0.063061028  6.785255e-02 K983388      H1302
## 31   1.213311156  0.89872035      -0.854177618  1.168768e+00   Fem20      H1102
## 32  -0.562330848  0.26374015      -0.749259056 -7.681194e-02 R187086       H502
## 33  -0.380032933 -0.17501294       0.162943609 -3.679636e-01 R187539       A102
## 34   0.132418059 -0.29511313       0.414572528  1.295866e-02 R187566       A302
## 35  -1.116290757 -0.57585064      -0.510528606 -2.991151e-02 R187569       A502
## 36  -0.967021832 -0.19191731      -1.007119760  2.320152e-01 R187568       A602
## 37   1.558785653  0.49478584       0.607128541  4.568713e-01 R187537      A1002
## 38   1.857670807  0.11074157       0.194364795  1.552564e+00 R187518      A1302
## 39   0.167647206  0.31632193      -0.403132636  2.544579e-01 R187916      A1602
## 40  -1.406887724 -0.57686351      -0.105983238 -7.240410e-01 R186903     A18B02
## 41   0.446886075  0.21527245       0.220515459  1.109817e-02 R187512      A2202
## 42   0.995943182 -0.04430110       0.259573821  7.806705e-01 R187562      A2302
## 43   0.167669460  0.13123132      -0.219941080  2.563792e-01 R187528      A2602
## 44  -0.658960529  0.13347705      -0.295693256 -4.967443e-01 R187563      A2702
## 45   0.284006990  0.11487289      -0.085620580  2.547547e-01 R187571      C2602
## 46   0.579039374  0.64559470      -0.514959327  4.484040e-01 R187945      C2302
## 47   0.384659138 -0.03138890      -0.430645382  8.466934e-01    Fem3      C1902
## 48   1.320762103  0.26843304       1.024413152  2.791591e-02 R187030      C1302
## 49  -0.249875177 -0.53760055       0.351259258 -6.353389e-02 R187517       C602
## 50   0.564239005  0.15557758       0.602413189 -1.937518e-01 R187523      B2202
## 51   0.167434807 -0.46941454       0.076790110  5.600592e-01 R186902      B1402
## 52  -1.914189637 -0.86093126      -0.468761495 -5.844969e-01 R187958      B1302
## 53  -1.094604511 -0.60891453      -0.500091496  1.440151e-02 R187953       B902
## 54  -0.386420005 -0.24959309      -0.422358557  2.855316e-01 R187932       B502
## 55   0.769547110 -0.28014159       0.374247902  6.754408e-01 R187547       B202
## 56  -0.527406101 -0.61768622       0.338204428 -2.479243e-01 R187947      D1302
## 57  -1.004909600 -0.01126390      -1.107582959  1.139373e-01 R187582      D1202
## 58   0.266887704  0.01666119       0.488172848 -2.379463e-01 R187545      D1002
## 59   0.111695860 -0.10710580       0.683757102 -4.649554e-01 R187546       D902
## 60  -0.195816831 -0.26994505       0.379266590 -3.051384e-01 R187239       D802
## 61  -1.146706210  0.13954234      -1.038501969 -2.477466e-01 R187590       D202
## 62  -1.040558266 -0.70081590       0.202836199 -5.425786e-01 R187521      E2002
## 63   0.144774184 -0.03411131      -0.337655202  5.165407e-01 R187931      E1902
## 64  -0.957278409  0.10065470      -1.115732648  5.779954e-02 R187577      E1802
## 65  -1.556322688 -0.36458784      -0.831589636 -3.601452e-01 R187292      E1602
## 66  -0.641836174  0.30188604      -0.826758018 -1.169642e-01 R187516      E1402
## 67  -0.646748933  0.23203549      -0.243281383 -6.355030e-01 R187166      E1202
## 68  -0.636360641  0.48031349      -0.435990873 -6.806833e-01 R187579      E1102
## 69   1.353894517  0.44105306       0.443790283  4.690512e-01 R187548       E902
## 70  -1.931028489 -0.38894299      -1.009127529 -5.329580e-01 R187155       E702
## 71  -0.275901950  0.08368727      -0.009623114 -3.499661e-01 R187594       E302
## 72  -1.339124124 -0.40939224       0.050193201 -9.799251e-01 R187588      F2402
## 73  -1.016255880 -0.24379862      -0.878409512  1.059522e-01 R187557      F2102
## 74  -1.012420664 -0.11381696      -0.182272429 -7.163313e-01 R187559      F1902
## 75   0.054112601  0.05790010       0.460580990 -4.643685e-01 R187531      F1702
## 76  -0.614258803 -1.02464494       0.385871221  2.451492e-02 R187963      F1502
## 77  -0.803384694 -0.80854693       0.534437805 -5.292756e-01 R187592      F1102
## 78  -0.957055231  0.19087364      -0.019208663 -1.128720e+00 R187575       F902
## 79   0.200711109 -0.37015115       0.757415640 -1.865534e-01    Fem5       F202
## 80  -0.834655882 -0.17009185       0.278692241 -9.432563e-01 R187930      G2202
## 81  -1.422472246 -0.06796621      -0.253796845 -1.100709e+00 R187598      G1602
## 82  -1.206538231 -0.61878375       0.243359432 -8.311139e-01 R187914      G1202
## 83  -0.864484404 -0.15437165      -0.442530603 -2.675821e-01 R187957       G702
## 84   0.503779582  0.11989873       0.605015873 -2.211350e-01 R187955       G602
## 85   1.526012262  0.14807203       0.188674120  1.189266e+00 R187552       G502
## 86  -0.565354601 -0.61490699      -0.054012138  1.035645e-01 R187535       G102
## 87   0.041342197 -0.09166757      -0.610771586  7.437814e-01 R187527      H3602
## 88   0.625836646  0.43320321      -0.444163795  6.367972e-01 R187595      H3202
## 89  -0.632792500  0.65607024      -0.242551269 -1.046311e+00 P322402      H2802
## 90  -0.715999444 -0.31030764      -0.063061028 -3.426308e-01 K983388      H1302
## 91   0.383888605  0.89872035      -0.854177618  3.393459e-01   Fem20      H1102
## 92  -1.323499094  0.26374015      -0.749259056 -8.379802e-01 R187086       H502
## 93   1.268136238 -0.17501294       0.162943609  1.280206e+00 R187539       A102
## 94   0.101488721 -0.29511313       0.414572528 -1.797067e-02 R187566       A302
## 95  -2.042996978 -0.57585064      -0.510528606 -9.566177e-01 R187569       A502
## 96  -1.796637803 -0.19191731      -1.007119760 -5.976007e-01 R187568       A602
## 97   1.847690779  0.49478584       0.607128541  7.457764e-01 R187537      A1002
## 98   0.759913233  0.11074157       0.194364795  4.548069e-01 R187518      A1302
## 99  -0.107805266  0.31632193      -0.403132636 -2.099455e-02 R187916      A1602
## 100 -0.646843172 -0.71326501      -0.353218785  4.196406e-01 R187515      A1802
## 101 -1.516481988 -0.57686351      -0.105983238 -8.336352e-01 R186903     A18B02
## 102 -1.153664954  0.21527245       0.220515459 -1.589453e+00 R187512      A2202
## 103 -0.257536972 -0.04430110       0.259573821 -4.728097e-01 R187562      A2302
## 104  0.559733167  0.13123132      -0.219941080  6.484429e-01 R187528      A2602
## 105  0.349527133  0.13347705      -0.295693256  5.117433e-01 R187563      A2702
## 106 -0.182877120  0.11487289      -0.085620580 -2.121294e-01 R187571      C2602
## 107  0.948480352  0.12282452       0.481212154  3.444437e-01 R187553      C2402
## 108  0.107473183  0.64559470      -0.514959327 -2.316219e-02 R187945      C2302
## 109  0.104581173 -0.03138890      -0.430645382  5.666155e-01    Fem3      C1902
## 110 -0.295328784  0.26843304       1.024413152 -1.588175e+00 R187030      C1302
## 111  0.329900073 -0.53760055       0.351259258  5.162414e-01 R187517       C602
## 112  0.836813251  0.14799143       0.676234353  1.258746e-02 R046109       C402
## 113  1.482804736  0.39571541       0.616489169  4.706002e-01 R187920       C202
## 114 -2.580938880 -1.00728517      -0.433153712 -1.140500e+00 R186907       C102
## 115 -0.132103368 -0.55615875       0.133819566  2.902358e-01 R187541      B2502
## 116  0.998061629  0.15557758       0.602413189  2.400709e-01 R187523      B2202
## 117  0.187896297 -0.02932911      -0.745657461  9.628829e-01 R187927      B2102
## 118  0.078215612 -0.12247720       0.039234815  1.614580e-01 R187942      B1902
## 119 -0.542557681  0.24221815      -0.992395071  2.076192e-01 R187937      B1802
## 120  1.843893808  0.13756229       0.442565782  1.263766e+00 R187824      B1702
## 121 -0.496616838 -0.44164642      -0.871384879  8.164145e-01 R186908      B1602
## 122 -0.409295224 -0.46941454       0.076790110 -1.667079e-02 R186902      B1402
## 123 -1.169733880 -0.86093126      -0.468761495  1.599589e-01 R187958      B1302
## 124 -0.452114967  0.74984886      -0.514650209 -6.873136e-01 R187944      B1202
## 125 -0.663394768 -0.41891386       0.364817341 -6.092983e-01 R187400      B1002
## 126 -1.074292465 -0.60891453      -0.500091496  3.471356e-02 R187953       B902
## 127 -1.361129859 -0.24959309      -0.422358557 -6.891782e-01 R187932       B502
## 128 -0.910838104 -0.28014159       0.374247902 -1.004944e+00 R187547       B202
## 129  0.330274425 -0.61768622       0.338204428  6.097562e-01 R187947      D1302
## 130 -2.829576507 -0.01126390      -1.107582959 -1.710730e+00 R187582      D1202
## 131  1.573088409  0.25820240       0.165871056  1.149015e+00 R187964      D1102
## 132  1.079173637  0.01666119       0.488172848  5.743396e-01 R187545      D1002
## 133  0.741228152 -0.10710580       0.683757102  1.645768e-01 R187546       D902
## 134  0.418511328 -0.26994505       0.379266590  3.091898e-01 R187239       D802
## 135 -0.373361293 -0.17867661      -0.254119561  5.943488e-02 R187940       D402
## 136 -0.728485720  0.13954234      -1.038501969  1.704739e-01 R187590       D202
## 137 -1.416711407 -0.77722684      -0.273925437 -3.655591e-01 R187524       D102
## 138 -0.800606411 -0.70081590       0.202836199 -3.026267e-01 R187521      E2002
## 139  0.269539175 -0.03411131      -0.337655202  6.413057e-01 R187931      E1902
## 140 -0.136676709  0.10065470      -1.115732648  8.784012e-01 R187577      E1802
## 141  1.923857953  0.72827673      -0.546647438  1.742229e+00 R188000      E1702
## 142 -0.620204158 -0.36458784      -0.831589636  5.759733e-01 R187292      E1602
## 143  0.412697835  0.30188604      -0.826758018  9.375698e-01 R187516      E1402
## 144 -1.164404162 -0.20797162       0.034257387 -9.906899e-01 R187999      E1302
## 145  1.012873155  0.23203549      -0.243281383  1.024119e+00 R187166      E1202
## 146  0.795112641  0.44105306       0.443790283 -8.973070e-02 R187548       E902
## 147 -1.335380426 -0.50182336      -0.344065447 -4.894916e-01 R187961       E802
## 148 -2.327793199 -0.38894299      -1.009127529 -9.297227e-01 R187155       E702
## 149 -0.137432568 -0.45246712       0.383282049 -6.824749e-02 R187399       E402
## 150 -0.390910205  0.08368727      -0.009623114 -4.649744e-01 R187594       E302
## 151  0.245869722 -0.40939224       0.050193201  6.050688e-01 R187588      F2402
## 152 -1.472047383 -0.24379862      -0.878409512 -3.498393e-01 R187557      F2102
## 153 -0.305948582 -0.11381696      -0.182272429 -9.859192e-03 R187559      F1902
## 154 -0.743808010  0.05790010       0.460580990 -1.262289e+00 R187531      F1702
## 155 -0.296136939 -1.02464494       0.385871221  3.426368e-01 R187963      F1502
## 156  1.661354395 -0.80854693       0.534437805  1.935464e+00 R187592      F1102
## 157  0.057370104  0.19087364      -0.019208663 -1.142949e-01 R187575       F902
## 158  0.857185638 -0.37015115       0.757415640  4.699212e-01    Fem5       F202
## 159  1.667601990  0.30961413       0.217220378  1.140767e+00 R186912       F102
## 160  2.029492628 -0.17009185       0.278692241  1.920892e+00 R187930      G2202
## 161 -0.837772179 -0.41102734       0.809694342 -1.236439e+00 R187513      G1902
## 162 -0.864607072 -0.06796621      -0.253796845 -5.428440e-01 R187598      G1602
## 163  0.024557752 -0.34018223       0.352798903  1.194108e-02 R187918      G1302
## 164 -1.246253759 -0.15437165      -0.442530603 -6.493515e-01 R187957       G702
## 165  0.293853639  0.11989873       0.605015873 -4.310610e-01 R187955       G602
## 166  0.312649761  0.14807203       0.188674120 -2.409639e-02 R187552       G502
## 167  0.205437295 -0.61490699      -0.054012138  8.743564e-01 R187535       G102
## 168  0.129837377 -0.09166757      -0.610771586  8.322765e-01 R187527      H3602
## 169 -0.151802652  0.43320321      -0.444163795 -1.408421e-01 R187595      H3202
## 170 -1.489519167  0.20931909      -0.687410340 -1.011428e+00 R187925      H3102
## 171 -0.653764972 -0.11885849      -0.493818415 -4.108807e-02 R187936      H3002
## 172 -0.377806138  0.65607024      -0.242551269 -7.913251e-01 P322402      H2802
## 173  0.132214303 -0.31030764      -0.063061028  5.055830e-01 K983388      H1302
## 174 -0.522425349  0.89872035      -0.854177618 -5.669681e-01   Fem20      H1102
## 175 -1.502453030 -0.12302997      -0.702749543 -6.766735e-01    Fem2       H702
## 176 -0.830193728  0.26374015      -0.749259056 -3.446748e-01 R187086       H502
## 177 -0.482455498 -0.17501294       0.162943609 -4.703862e-01 R187539       A102
## 178 -0.954482335 -0.29511313       0.414572528 -1.073942e+00 R187566       A302
## 179 -0.596754336 -0.57585064      -0.510528606  4.896249e-01 R187569       A502
## 180 -1.335196410 -0.19191731      -1.007119760 -1.361593e-01 R187568       A602
## 181  0.907237156  0.49478584       0.607128541 -1.946772e-01 R187537      A1002
## 182  0.485073654  0.11074157       0.194364795  1.799673e-01 R187518      A1302
## 183  0.202818757  0.31632193      -0.403132636  2.896295e-01 R187916      A1602
## 184 -0.524060195 -0.71326501      -0.353218785  5.424236e-01 R187515      A1802
## 185 -1.184146043 -0.57686351      -0.105983238 -5.012993e-01 R186903     A18B02
## 186  0.026506505  0.21527245       0.220515459 -4.092814e-01 R187512      A2202
## 187  0.992870567  0.23618471       0.858158899 -1.014730e-01 R187343     A22B02
## 188 -0.199132121 -0.04430110       0.259573821 -4.144048e-01 R187562      A2302
## 189 -0.961025978 -0.13975244      -0.795921760 -2.535178e-02 R186911      A2502
## 190 -0.287480468  0.13123132      -0.219941080 -1.987707e-01 R187528      A2602
## 191 -0.779916309  0.13347705      -0.295693256 -6.177001e-01 R187563      A2702
## 192 -1.247055672  0.11487289      -0.085620580 -1.276308e+00 R187571      C2602
## 193  0.217385265  0.12282452       0.481212154 -3.866514e-01 R187553      C2402
## 194 -1.319754407  0.64559470      -0.514959327 -1.450390e+00 R187945      C2302
## 195 -0.950352531 -0.03138890      -0.430645382 -4.883182e-01    Fem3      C1902
## 196  2.122057607  0.26843304       1.024413152  8.292114e-01 R187030      C1302
## 197 -0.401655563 -0.53760055       0.351259258 -2.153143e-01 R187517       C602
## 198  0.530684749  0.14799143       0.676234353 -2.935410e-01 R046109       C402
## 199  2.403534685  0.39571541       0.616489169  1.391330e+00 R187920       C202
## 200 -1.828996337 -1.00728517      -0.433153712 -3.885575e-01 R186907       C102
## 201  0.283933263 -0.55615875       0.133819566  7.062724e-01 R187541      B2502
## 202  2.136164382  0.15557758       0.602413189  1.378174e+00 R187523      B2202
## 203 -0.353127093 -0.12247720       0.039234815 -2.698847e-01 R187942      B1902
## 204 -0.497031897  0.24221815      -0.992395071  2.531450e-01 R187937      B1802
## 205  1.433028231  0.13756229       0.442565782  8.529002e-01 R187824      B1702
## 206 -1.976603002 -0.44164642      -0.871384879 -6.635717e-01 R186908      B1602
## 207  1.294598679 -0.46941454       0.076790110  1.687223e+00 R186902      B1402
## 208  0.182472737 -0.86093126      -0.468761495  1.512165e+00 R187958      B1302
## 209 -0.655758762  0.74984886      -0.514650209 -8.909574e-01 R187944      B1202
## 210 -0.481526893 -0.50880833      -0.676838984  7.041204e-01 R187902      B1102
## 211  0.515240561 -0.41891386       0.364817341  5.693371e-01 R187400      B1002
## 212 -0.862398063 -0.60891453      -0.500091496  2.466080e-01 R187953       B902
## 213 -0.616068907 -0.24959309      -0.422358557  5.588274e-02 R187932       B502
## 214  0.936751235 -0.28014159       0.374247902  8.426449e-01 R187547       B202
## 215 -0.279346993 -0.61768622       0.338204428  1.347950e-04 R187947      D1302
## 216 -0.910345949 -0.01126390      -1.107582959  2.085009e-01 R187582      D1202
## 217  0.344630378  0.25820240       0.165871056 -7.944308e-02 R187964      D1102
## 218  0.390713876  0.01666119       0.488172848 -1.141202e-01 R187545      D1002
## 219  1.756741577 -0.10710580       0.683757102  1.180090e+00 R187546       D902
## 220 -0.880275952 -0.26994505       0.379266590 -9.895975e-01 R187239       D802
## 221 -1.086563084 -0.17867661      -0.254119561 -6.537669e-01 R187940       D402
## 222 -1.033236607  0.13954234      -1.038501969 -1.342770e-01 R187590       D202
## 223 -1.028651245 -0.77722684      -0.273925437  2.250104e-02 R187524       D102
## 224  0.164306296 -0.70081590       0.202836199  6.622860e-01 R187521      E2002
## 225 -1.092264477 -0.03411131      -0.337655202 -7.204980e-01 R187931      E1902
## 226 -0.990902295  0.10065470      -1.115732648  2.417566e-02 R187577      E1802
## 227  1.284970015  0.72827673      -0.546647438  1.103341e+00 R188000      E1702
## 228 -1.903864782 -0.36458784      -0.831589636 -7.076873e-01 R187292      E1602
## 229 -0.774961504  0.30188604      -0.826758018 -2.500895e-01 R187516      E1402
## 230 -0.059739455 -0.20797162       0.034257387  1.139748e-01 R187999      E1302
## 231 -0.264168410  0.23203549      -0.243281383 -2.529225e-01 R187166      E1202
## 232 -0.573854210  0.48031349      -0.435990873 -6.181768e-01 R187579      E1102
## 233  0.264324161  0.44105306       0.443790283 -6.205192e-01 R187548       E902
## 234 -0.438547654 -0.50182336      -0.344065447  4.073412e-01 R187961       E802
## 235 -0.525936957 -0.38894299      -1.009127529  8.721336e-01 R187155       E702
## 236  0.499161924 -0.45246712       0.383282049  5.683470e-01 R187399       E402
## 237  1.649571257  0.08368727      -0.009623114  1.575507e+00 R187594       E302
## 238  0.213390730 -0.30731744       0.492355325  2.835285e-02 R187948      F2702
## 239 -0.416285188 -0.40939224       0.050193201 -5.708615e-02 R187588      F2402
## 240 -1.025736449 -0.24379862      -0.878409512  9.647168e-02 R187557      F2102
## 241  0.393352611 -0.11381696      -0.182272429  6.894420e-01 R187559      F1902
## 242  0.595795920  0.05790010       0.460580990  7.731483e-02 R187531      F1702
## 243 -1.206714578 -1.02464494       0.385871221 -5.679409e-01 R187963      F1502
## 244 -0.625889007 -0.80854693       0.534437805 -3.517799e-01 R187592      F1102
## 245  1.017547990  0.19087364      -0.019208663  8.458830e-01 R187575       F902
## 246 -0.716453838 -0.37015115       0.757415640 -1.103718e+00    Fem5       F202
## 247  0.249385000  0.30961413       0.217220378 -2.774495e-01 R186912       F102
## 248 -0.860440996 -0.17009185       0.278692241 -9.690414e-01 R187930      G2202
## 249  0.932704787 -0.41102734       0.809694342  5.340378e-01 R187513      G1902
## 250  1.458309557  0.12343192       0.940475518  3.944021e-01 R187000      G1802
## 251  0.067250973 -0.34018223       0.352798903  5.463430e-02 R187918      G1302
## 252  0.954394790 -0.61878375       0.243359432  1.329819e+00 R187914      G1202
## 253  0.583711519 -0.15437165      -0.442530603  1.180614e+00 R187957       G702
## 254  1.961063867  0.11989873       0.605015873  1.236149e+00 R187955       G602
## 255 -1.294775737  0.14807203       0.188674120 -1.631522e+00 R187552       G502
## 256 -0.228387557 -0.61490699      -0.054012138  4.405316e-01 R187535       G102
## 257 -0.963779309 -0.09166757      -0.610771586 -2.613402e-01 R187527      H3602
## 258  0.173731222  0.43320321      -0.444163795  1.846918e-01 R187595      H3202
## 259 -0.480571196  0.20931909      -0.687410340 -2.479949e-03 R187925      H3102
## 260 -1.346980596 -0.11885849      -0.493818415 -7.343037e-01 R187936      H3002
## 261  1.022880763  0.65607024      -0.242551269  6.093618e-01 P322402      H2802
## 262  0.802186268 -0.05998455       0.617744816  2.444260e-01 R186918      H2502
## 263  0.306173501 -0.06913447       0.706717112 -3.314091e-01 R187637      H1802
## 264 -0.403568389 -0.31030764      -0.063061028 -3.019972e-02 K983388      H1302
## 265 -0.604086445  0.89872035      -0.854177618 -6.486292e-01   Fem20      H1102
## 266 -2.383743119 -0.12302997      -0.702749543 -1.557964e+00    Fem2       H702
## 267 -0.220756484  0.26374015      -0.749259056  2.647624e-01 R187086       H502
## 268  0.695045778 -0.17501294       0.162943609  7.071151e-01 R187539       A102
## 269  0.178123155 -0.29511313       0.414572528  5.866376e-02 R187566       A302
## 270  0.071101925 -0.57585064      -0.510528606  1.157481e+00 R187569       A502
## 271 -1.437187537 -0.19191731      -1.007119760 -2.381505e-01 R187568       A602
## 272  1.398908321  0.49478584       0.607128541  2.969939e-01 R187537      A1002
## 273 -0.437896576  0.11074157       0.194364795 -7.430029e-01 R187518      A1302
## 274 -1.165692946  0.31632193      -0.403132636 -1.078882e+00 R187916      A1602
## 275 -0.681829820 -0.71326501      -0.353218785  3.846540e-01 R187515      A1802
## 276 -0.025066842 -0.57686351      -0.105983238  6.577799e-01 R186903     A18B02
## 277  0.195186775  0.21527245       0.220515459 -2.406011e-01 R187512      A2202
## 278  0.925966803  0.23618471       0.858158899 -1.683768e-01 R187343     A22B02
## 279 -0.259320152 -0.04430110       0.259573821 -4.745929e-01 R187562      A2302
## 280  0.044910027 -0.13975244      -0.795921760  9.805842e-01 R186911      A2502
## 281 -0.658461194  0.13123132      -0.219941080 -5.697514e-01 R187528      A2602
## 282 -0.460075184  0.13347705      -0.295693256 -2.978590e-01 R187563      A2702
## 283 -0.455223459  0.11487289      -0.085620580 -4.844758e-01 R187571      C2602
## 284 -0.367665885  0.12282452       0.481212154 -9.717026e-01 R187553      C2402
## 285  0.995476451  0.64559470      -0.514959327  8.648411e-01 R187945      C2302
## 286  0.875678196  0.58853159       0.043739473  2.434071e-01 R187398      C2202
## 287  0.392160143 -0.03138890      -0.430645382  8.541944e-01    Fem3      C1902
## 288  1.707480996  0.26843304       1.024413152  4.146348e-01 R187030      C1302
## 289  0.262554609 -0.53760055       0.351259258  4.488959e-01 R187517       C602
## 290  1.678918848  0.14799143       0.676234353  8.546931e-01 R046109       C402
## 291 -0.558151460  0.39571541       0.616489169 -1.570356e+00 R187920       C202
## 292 -0.127343375 -0.55615875       0.133819566  2.949958e-01 R187541      B2502
## 293  1.034083662  0.15557758       0.602413189  2.760929e-01 R187523      B2202
## 294 -0.216803881 -0.02932911      -0.745657461  5.581827e-01 R187927      B2102
## 295  0.199790898 -0.12247720       0.039234815  2.830333e-01 R187942      B1902
## 296 -0.889204572  0.24221815      -0.992395071 -1.390277e-01 R187937      B1802
## 297  1.016568875  0.13756229       0.442565782  4.364408e-01 R187824      B1702
## 298 -1.415748925 -0.44164642      -0.871384879 -1.027176e-01 R186908      B1602
## 299  1.381758053 -0.46941454       0.076790110  1.774382e+00 R186902      B1402
## 300 -0.463867223 -0.86093126      -0.468761495  8.658255e-01 R187958      B1302
## 301  0.525369285  0.74984886      -0.514650209  2.901706e-01 R187944      B1202
## 302 -0.839087688 -0.50880833      -0.676838984  3.465596e-01 R187902      B1102
## 303 -1.054418741 -0.41891386       0.364817341 -1.000322e+00 R187400      B1002
## 304 -0.209266156 -0.60891453      -0.500091496  8.997399e-01 R187953       B902
## 305  0.370923057 -0.24959309      -0.422358557  1.042875e+00 R187932       B502
## 306 -0.555171196 -0.28014159       0.374247902 -6.492775e-01 R187547       B202
## 307  1.020616818 -0.61768622       0.338204428  1.300099e+00 R187947      D1302
## 308 -1.747958775 -0.01126390      -1.107582959 -6.291119e-01 R187582      D1202
## 309  0.701568645  0.25820240       0.165871056  2.774952e-01 R187964      D1102
## 310  1.088035099  0.01666119       0.488172848  5.832011e-01 R187545      D1002
## 311 -0.298826558 -0.10710580       0.683757102 -8.754779e-01 R187546       D902
## 312  0.097260805 -0.26994505       0.379266590 -1.206073e-02 R187239       D802
## 313 -1.148370609 -0.17867661      -0.254119561 -7.155744e-01 R187940       D402
## 314 -1.193150299  0.13954234      -1.038501969 -2.941907e-01 R187590       D202
## 315 -1.592210350 -0.77722684      -0.273925437 -5.410581e-01 R187524       D102
## 316 -1.540402239 -0.70081590       0.202836199 -1.042423e+00 R187521      E2002
## 317 -0.686325234 -0.03411131      -0.337655202 -3.145587e-01 R187931      E1902
## 318 -0.617900240  0.10065470      -1.115732648  3.971777e-01 R187577      E1802
## 319  0.164706387  0.72827673      -0.546647438 -1.692290e-02 R188000      E1702
## 320 -0.080314514 -0.36458784      -0.831589636  1.115863e+00 R187292      E1602
## 321 -1.086219667  0.30188604      -0.826758018 -5.613477e-01 R187516      E1402
## 322  0.974840315 -0.20797162       0.034257387  1.148555e+00 R187999      E1302
## 323 -0.944349234  0.23203549      -0.243281383 -9.331033e-01 R187166      E1202
## 324  0.967638276  0.48031349      -0.435990873  9.233157e-01 R187579      E1102
## 325  1.136115633  0.44105306       0.443790283  2.512723e-01 R187548       E902
## 326 -1.176890642 -0.50182336      -0.344065447 -3.310018e-01 R187961       E802
## 327 -0.628716575 -0.38894299      -1.009127529  7.693539e-01 R187155       E702
## 328  1.046679614  0.08368727      -0.009623114  9.726155e-01 R187594       E302
## 329 -0.199064679 -0.30731744       0.492355325 -3.841026e-01 R187948      F2702
## 330  0.463775145 -0.40939224       0.050193201  8.229742e-01 R187588      F2402
## 331 -2.169556777 -0.24379862      -0.878409512 -1.047349e+00 R187557      F2102
## 332 -1.345990984 -0.11381696      -0.182272429 -1.049902e+00 R187559      F1902
## 333  0.398401300  0.05790010       0.460580990 -1.200798e-01 R187531      F1702
## 334 -0.386985249 -1.02464494       0.385871221  2.517885e-01 R187963      F1502
## 335  0.189791298 -0.80854693       0.534437805  4.639004e-01 R187592      F1102
## 336  0.829948571  0.19087364      -0.019208663  6.582836e-01 R187575       F902
## 337  0.634644709 -0.37015115       0.757415640  2.473802e-01    Fem5       F202
## 338  1.240415419  0.30961413       0.217220378  7.135809e-01 R186912       F102
## 339 -0.756221576 -0.17009185       0.278692241 -8.648220e-01 R187930      G2202
## 340 -0.548430166 -0.41102734       0.809694342 -9.470972e-01 R187513      G1902
## 341  1.248699160  0.12343192       0.940475518  1.847917e-01 R187000      G1802
## 342 -0.662382866 -0.06796621      -0.253796845 -3.406198e-01 R187598      G1602
## 343 -0.095241218 -0.34018223       0.352798903 -1.078579e-01 R187918      G1302
## 344 -1.447416794 -0.61878375       0.243359432 -1.071992e+00 R187914      G1202
## 345 -0.910119953 -0.15437165      -0.442530603 -3.132177e-01 R187957       G702
## 346  0.180535985  0.11989873       0.605015873 -5.443786e-01 R187955       G602
## 347 -0.743029505  0.14807203       0.188674120 -1.079776e+00 R187552       G502
## 348 -1.711327246 -0.61490699      -0.054012138 -1.042408e+00 R187535       G102
## 349  0.273787311 -0.09166757      -0.610771586  9.762265e-01 R187527      H3602
## 350  0.632613850  0.43320321      -0.444163795  6.435744e-01 R187595      H3202
## 351  0.137432339  0.20931909      -0.687410340  6.155236e-01 R187925      H3102
## 352 -0.458848127  0.42981414      -0.737813082 -1.508492e-01 R187001      H2602
## 353 -0.288778896 -0.05998455       0.617744816 -8.465392e-01 R186918      H2502
## 354 -0.049473155 -0.06913447       0.706717112 -6.870558e-01 R187637      H1802
## 355 -0.026654589 -0.31030764      -0.063061028  3.467141e-01 K983388      H1302
## 356 -0.188142836  0.89872035      -0.854177618 -2.326856e-01   Fem20      H1102
## 357  1.074388765  0.26374015      -0.749259056  1.559908e+00 R187086       H502
## 358 -0.260851163  0.10711933      -0.844219842  4.762493e-01 R187009       H102
## 359  0.490584907  0.07978973      -0.515131678  9.259269e-01 R187926       G402
## 360 -0.836838650  0.07978973      -0.515131678 -4.014967e-01 R187926       G402
## 361 -0.218097515  0.07978973      -0.515131678  2.172444e-01 R187926       G402
## 362 -0.168783045  0.07978973      -0.515131678  2.665589e-01 R187926       G402
## 363 -1.631278597  0.07978973      -0.515131678 -1.195937e+00 R187926       G402
## 364 -0.395616686  0.07978973      -0.515131678  3.972526e-02 R187926       G402
## 365  1.475403957  0.83826639      -0.188367525  8.255051e-01 R187951      F2502
## 366 -1.031736779  0.07978973      -0.515131678 -5.963948e-01 R187926       G402
## 367  1.063769622  0.83826639      -0.188367525  4.138708e-01 R187951      F2502
## 368 -0.298038597  0.07978973      -0.515131678  1.373033e-01 R187926       G402
## 369 -0.708241011 -0.53760055       0.043739473 -2.143799e-01 R187517      C2202
## 370  0.427628674 -0.45246712       0.133819566  7.462762e-01 R187399      B2502
## 371 -0.440453943 -0.02932911      -0.254119561 -1.570053e-01 R187927       D402
## 372  0.830951644  0.83826639      -0.188367525  1.810528e-01 R187951      F2502
## 373 -0.521912565  0.07978973      -0.515131678 -8.657062e-02 R187926       G402
## 374 -1.550609014 -0.04430110      -1.007119760 -4.991882e-01 R187562       A602
## 375 -0.267837789 -0.24493955      -0.222557319  1.996591e-01 R186910      A1202
## 376  0.129478612 -0.06796621      -0.403132636  6.005775e-01 R187598      A1602
## 377 -0.995419266  0.11074157      -0.219941080 -8.862198e-01 R187518      A2602
## 378  0.170547006 -0.53760055       0.043739473  6.644081e-01 R187517      C2202
## 379 -0.243225204 -0.45246712       0.133819566  7.542235e-02 R187399      B2502
## 380  0.278160078 -0.26994505       0.374247902  1.738572e-01 R187239       B202
## 381 -1.172425743 -0.02932911      -0.254119561 -8.889771e-01 R187927       D402
## 382  0.755010819  0.83826639      -0.188367525  1.051120e-01 R187951      F2502
## 383  0.351451114 -0.03411131       0.278692241  1.068702e-01 R187931      G2202
## 384  0.447514542  0.07978973      -0.515131678  8.828565e-01 R187926       G402
## 385 -0.225411089  0.42981414      -0.844219842  1.889946e-01 R187001       H102
## 386 -0.287121977 -0.04430110      -1.007119760  7.642989e-01 R187562       A602
## 387 -0.381244168 -0.24493955      -0.222557319  8.625270e-02 R186910      A1202
## 388 -1.298411191 -0.06796621      -0.403132636 -8.273123e-01 R187598      A1602
## 389  1.220119067  0.11074157      -0.219941080  1.329319e+00 R187518      A2602
## 390 -0.514390887  0.14807203      -0.085620580 -5.768423e-01 R187552      C2602
## 391 -1.367727003 -0.53760055       0.043739473 -8.738659e-01 R187517      C2202
## 392  0.785864956  0.58853159       0.351259258 -1.539259e-01 R187398       C602
## 393  2.178339645 -0.11885849       0.616489169  1.680709e+00 R187936       C202
## 394  1.050325268 -0.45246712       0.133819566  1.368973e+00 R187399      B2502
## 395 -0.785525221 -0.26994505       0.374247902 -8.898281e-01 R187239       B202
## 396 -0.086452325 -0.02932911      -0.254119561  1.969963e-01 R187927       D402
## 397 -0.369052625 -0.24959309      -1.038501969  9.190424e-01 R187932       D202
## 398  0.460878823  0.30188604      -0.273925437  4.329182e-01 R187516       D102
## 399 -0.385611421 -0.30731744      -0.546647438  4.683535e-01 R187948      E1702
## 400 -0.283815865 -0.50182336       0.034257387  1.837501e-01 R187961      E1302
## 401 -0.754785228  0.23203549      -0.435990873 -5.508298e-01 R187166      E1102
## 402 -1.435534650 -0.40939224      -1.009127529 -1.701488e-02 R187588       E702
## 403  0.183986560  0.83826639      -0.188367525 -4.659123e-01 R187951      F2502
## 404 -0.291488358 -0.11381696      -0.878409512  7.007381e-01 R187559      F2102
## 405 -0.087075361  0.26843304       0.460580990 -8.160894e-01 R187030      F1702
## 406 -1.259019458 -0.80854693      -0.019208663 -4.312639e-01 R187592       F902
## 407  1.353445940  0.14106848       0.757415640  4.549618e-01 R187941       F202
## 408  0.539509178 -0.03411131       0.278692241  2.949282e-01 R187931      G2202
## 409  1.784033710 -0.46941454       0.940475518  1.312973e+00 R186902      G1802
## 410 -1.179272907 -0.61878375       0.605015873 -1.165505e+00 R187914       G602
## 411  0.005515433  0.07978973      -0.515131678  4.408574e-01 R187926       G402
## 412 -0.749685306  0.26374015      -0.054012138 -9.594133e-01 R187086       G102
## 413 -0.739314587  0.12282452      -0.444163795 -4.179753e-01 R187553      H3202
## 414 -0.162689157 -0.27011654       0.617744816 -5.103174e-01 R186917      H2502
## 415  1.516489950  0.89872035      -0.063061028  6.808306e-01   Fem20      H1302
## 416 -1.337344760  0.42981414      -0.844219842 -9.229391e-01 R187001       H102
## 417 -0.583380284 -0.29511313       0.162943609 -4.512108e-01 R187566       A102
## 418 -0.083084560 -0.17501294       0.414572528 -3.226441e-01 R187539       A302
## 419 -1.203916581 -0.04430110      -1.007119760 -1.524957e-01 R187562       A602
## 420  1.745101900  0.13347705       0.607128541  1.004496e+00 R187563      A1002
## 421  0.262967681 -0.24493955      -0.222557319  7.304646e-01 R186910      A1202
## 422 -0.888583404 -0.06796621      -0.403132636 -4.174846e-01 R187598      A1602
## 423 -0.841443102 -0.09166757      -0.353218785 -3.965567e-01 R187527      A1802
## 424  0.750165122  0.65607024       0.220515459 -1.264206e-01 P322402      A2202
## 425 -0.212891199 -0.57585064       0.858158899 -4.951995e-01 R187569     A22B02
## 426  1.048172077 -0.19191731       0.259573821  9.805156e-01 R187568      A2302
## 427  1.085469453  0.11074157      -0.219941080  1.194669e+00 R187518      A2602
## 428 -0.741948512  0.14807203      -0.085620580 -8.044000e-01 R187552      C2602
## 429  2.638167265  0.43320321       0.481212154  1.723752e+00 R187595      C2402
## 430  0.764572996  0.08368727      -0.514959327  1.195845e+00 R187594      C2302
## 431 -1.359184036 -0.53760055       0.043739473 -8.653230e-01 R187517      C2202
## 432 -0.233817415  0.05790010       1.024413152 -1.316131e+00 R187531      C1302
## 433  1.024044384  0.58853159       0.351259258  8.425353e-02 R187398       C602
## 434  1.594375440 -0.11885849       0.616489169  1.096745e+00 R187936       C202
## 435  1.622505803 -0.44164642       0.442565782  1.621586e+00 R186908      B1702
## 436  0.600431906  0.12343192       0.076790110  4.002099e-01 R187000      B1402
## 437 -0.236262499  0.30961413      -0.676838984  1.309624e-01 R186912      B1102
## 438  0.872774259 -0.03138890       0.364817341  5.393458e-01    Fem3      B1002
## 439 -0.628648675 -0.26994505       0.374247902 -7.329515e-01 R187239       B202
## 440 -0.105505849  0.01666119       0.683757102 -8.059241e-01 R187545       D902
## 441  0.751499290  0.56961413       0.145716067  3.616909e-02 R187540       D602
## 442 -1.391101303 -0.24959309      -1.038501969 -1.030062e-01 R187932       D202
## 443  0.356255854  0.30188604      -0.273925437  3.282953e-01 R187516       D102
## 444 -0.235838141 -0.30731744      -0.546647438  6.181267e-01 R187948      E1702
## 445 -1.354106834 -0.77722684      -0.826758018  2.498780e-01 R187524      E1402
## 446 -0.097244926 -0.50182336       0.034257387  3.703210e-01 R187961      E1302
## 447  0.606016501  0.48031349      -0.243281383  3.689844e-01 R187579      E1202
## 448 -0.222304974  0.23203549      -0.435990873 -1.834959e-02 R187166      E1102
## 449 -2.030913023 -1.00728517       0.159030574 -1.182658e+00 R186907      E1002
## 450  0.096407517 -0.40939224      -1.009127529  1.514927e+00 R187588       E702
## 451  0.500599267  0.64559470      -0.009623114 -1.353723e-01 R187945       E302
## 452  0.754419720  0.83826639      -0.188367525  1.045209e-01 R187951      F2502
## 453 -0.908373611 -0.38894299       0.050193201 -5.696238e-01 R187155      F2402
## 454 -0.503970876 -0.11381696      -0.878409512  4.882556e-01 R187559      F2102
## 455 -0.447394735 -0.24379862      -0.182272429 -2.132369e-02 R187557      F1902
## 456  1.520914792  0.26843304       0.460580990  7.919008e-01 R187030      F1702
## 457  0.057413512  0.25820240       0.385871221 -5.866601e-01 R187964      F1502
## 458 -0.582903928 -0.80854693      -0.019208663  2.448517e-01 R187592       F902
## 459  0.876231810  0.14106848       0.757415640 -2.225231e-02 R187941       F202
## 460 -0.124976519 -0.03411131       0.278692241 -3.695574e-01 R187931      G2202
## 461  0.606672457 -0.46941454       0.940475518  1.356115e-01 R186902      G1802
## 462  0.314652547  0.31632193      -0.253796845  2.521275e-01 R187916      G1602
## 463  0.322834673  0.11989873       0.243359432 -4.042349e-02 R187955      G1202
## 464  0.251521703 -0.61878375       0.605015873  2.652896e-01 R187914       G602
## 465 -1.339567300  0.07978973      -0.515131678 -9.042254e-01 R187926       G402
## 466  0.555512620  0.26374015      -0.054012138  3.457846e-01 R187086       G102
## 467 -0.187153780 -0.71326501      -0.610771586  1.136883e+00 R187515      H3602
## 468 -1.129181419  0.12282452      -0.444163795 -8.078421e-01 R187553      H3202
## 469  0.021276273 -0.27011654       0.617744816 -3.263520e-01 R186917      H2502
## 470  0.370368940  0.89872035      -0.063061028 -4.652904e-01   Fem20      H1302
## 471  0.120446368 -0.31030764      -0.854177618  1.284932e+00 K983388      H1102
## 472 -1.622004950 -0.61490699      -0.749259056 -2.578389e-01 R187535       H502
## 473  0.897675618  0.42981414      -0.844219842  1.312081e+00 R187001       H102
## 474 -1.084875429 -0.29511313       0.162943609 -9.527059e-01 R187566       A102
## 475  0.335805640 -0.17501294       0.414572528  9.624605e-02 R187539       A302
## 476  0.357887750  0.23618471      -0.510528606  6.322316e-01 R187343       A502
## 477 -0.538259946 -0.04430110      -1.007119760  5.131609e-01 R187562       A602
## 478  1.754623531  0.13347705       0.607128541  1.014018e+00 R187563      A1002
## 479 -0.473187421 -0.24493955      -0.222557319 -5.690551e-03 R186910      A1202
## 480 -0.653230299 -0.06796621      -0.403132636 -1.821315e-01 R187598      A1602
## 481 -0.897411527 -0.09166757      -0.353218785 -4.525252e-01 R187527      A1802
## 482  1.642915163  0.65607024       0.220515459  7.663295e-01 P322402      A2202
## 483 -0.360734315 -0.57585064       0.858158899 -6.430426e-01 R187569     A22B02
## 484  0.631503427 -0.19191731       0.259573821  5.638469e-01 R187568      A2302
## 485 -1.519418486  0.11074157      -0.219941080 -1.410219e+00 R187518      A2602
## 486  0.113207852  0.14807203      -0.085620580  5.075641e-02 R187552      C2602
## 487 -0.408435150  0.08368727      -0.514959327  2.283691e-02 R187594      C2302
## 488 -0.402545394 -0.53760055       0.043739473  9.131568e-02 R187517      C2202
## 489 -0.823920838 -0.41891386      -0.430645382  2.563840e-02 R187400      C1902
## 490  1.504538857  0.05790010       1.024413152  4.222256e-01 R187531      C1302
## 491 -0.001418513  0.58853159       0.351259258 -9.412094e-01 R187398       C602
## 492  0.055177637 -0.11885849       0.616489169 -4.424530e-01 R187936       C202
## 493 -0.863812631 -0.45246712       0.133819566 -5.451651e-01 R187399      B2502
## 494 -0.917766188 -0.70081590       0.602413189 -8.193635e-01 R187521      B2202
## 495 -0.636455910 -0.44164642       0.442565782 -6.373753e-01 R186908      B1702
## 496 -0.667666676  0.13756229      -0.871384879  6.615592e-02 R187824      B1602
## 497 -0.982792687  0.12343192       0.076790110 -1.183015e+00 R187000      B1402
## 498 -0.774411527 -0.61768622      -0.514650209  3.579249e-01 R187947      B1202
## 499 -0.379894864  0.30961413      -0.676838984 -1.267001e-02 R186912      B1102
## 500  1.426063884 -0.03138890       0.364817341  1.092635e+00    Fem3      B1002
## 501 -0.271565245 -0.86093126      -0.500091496  1.089458e+00 R187958       B902
## 502  0.111060088 -0.05998455       0.015293055  1.557516e-01 R186918       B702
## 503 -0.412080171  0.13954234      -0.422358557 -1.292640e-01 R187590       B502
## 504 -0.193920434 -0.26994505       0.374247902 -2.982233e-01 R187239       B202
## 505 -1.117482999  0.44105306      -1.107582959 -4.509531e-01 R187548      D1202
## 506  1.669622649 -0.10710580       0.488172848  1.288556e+00 R187546      D1002
## 507  0.256386584  0.01666119       0.683757102 -4.440317e-01 R187545       D902
## 508  0.178904635  0.56961413       0.145716067 -5.364256e-01 R187540       D602
## 509 -0.937435776 -0.02932911      -0.254119561 -6.539871e-01 R187927       D402
## 510 -2.301311328 -0.24959309      -1.038501969 -1.013216e+00 R187932       D202
## 511 -0.228443371  0.30188604      -0.273925437 -2.564040e-01 R187516       D102
## 512 -1.109676057 -0.30731744      -0.546647438 -2.557112e-01 R187948      E1702
## 513 -2.978496506 -0.77722684      -0.826758018 -1.374512e+00 R187524      E1402
## 514  0.428396573 -0.50182336       0.034257387  8.959625e-01 R187961      E1302
## 515  0.208488843  0.48031349      -0.243281383 -2.854326e-02 R187579      E1202
## 516  0.302488166  0.23203549      -0.435990873  5.064436e-01 R187166      E1102
## 517 -0.958680456 -1.00728517       0.159030574 -1.104259e-01 R186907      E1002
## 518  0.788051537 -0.01126390       0.443790283  3.555252e-01 R187582       E902
## 519 -0.526999986 -0.20797162      -0.344065447  2.503708e-02 R187999       E802
## 520 -1.464922063 -0.40939224      -1.009127529 -4.640229e-02 R187588       E702
## 521 -0.368239409  0.64559470      -0.009623114 -1.004211e+00 R187945       E302
## 522  1.088141574  0.83826639      -0.188367525  4.382427e-01 R187951      F2502
## 523  0.276526005 -0.38894299       0.050193201  6.152758e-01 R187155      F2402
## 524 -0.924190500 -0.11381696      -0.878409512  6.803597e-02 R187559      F2102
## 525 -0.500711291 -0.24379862      -0.182272429 -7.464025e-02 R187557      F1902
## 526  0.019054512  0.26843304       0.460580990 -7.099595e-01 R187030      F1702
## 527  0.611370469  0.25820240       0.385871221 -3.270315e-02 R187964      F1502
## 528  1.138046126  0.19087364       0.534437805  4.127347e-01 R187575      F1102
## 529 -1.218935871 -0.80854693      -0.019208663 -3.911803e-01 R187592       F902
## 530  0.675661631  0.14106848       0.757415640 -2.228225e-01 R187941       F202
## 531  0.225570859 -0.03411131       0.278692241 -1.901007e-02 R187931      G2202
## 532  1.296707368  0.28588528       0.809694342  2.011277e-01 R187573      G1902
## 533 -0.879634992  0.31632193      -0.253796845 -9.421601e-01 R187916      G1602
## 534  0.348078053  0.20931909       0.352798903 -2.140399e-01 R187925      G1302
## 535 -0.769447433  0.11989873       0.243359432 -1.132706e+00 R187955      G1202
## 536  0.315031047 -0.61878375       0.605015873  3.287989e-01 R187914       G602
## 537 -0.990911907  0.07978973      -0.515131678 -5.555700e-01 R187926       G402
## 538 -0.099216838  0.26374015      -0.054012138 -3.089448e-01 R187086       G102
## 539 -0.582434408 -0.71326501      -0.610771586  7.416022e-01 R187515      H3602
## 540 -1.409895253  0.12282452      -0.444163795 -1.088556e+00 R187553      H3202
## 541  0.235032702 -0.27011654       0.617744816 -1.125956e-01 R186917      H2502
## 542  2.190905234 -0.15437165       0.706717112  1.638560e+00 R187957      H1802
## 543  0.709776721  0.89872035      -0.063061028 -1.258826e-01   Fem20      H1302
## 544 -1.676963252 -0.31030764      -0.854177618 -5.124780e-01 K983388      H1102
## 545 -1.692318392 -0.57686351      -0.702749543 -4.127053e-01 R186903       H702
## 546 -2.869582667 -0.61490699      -0.749259056 -1.505417e+00 R187535       H502
## 547 -0.408880477 -0.13975244      -1.180998594  9.118706e-01 R186911       H302
## 548 -0.302024676  0.42981414      -0.844219842  1.123810e-01 R187001       H102
## 549 -0.239970535 -0.29511313       0.162943609 -1.078010e-01 R187566       A102
## 550  0.476595843 -0.17501294       0.414572528  2.370363e-01 R187539       A302
## 551  0.018485865  0.23618471      -0.510528606  2.928298e-01 R187343       A502
## 552 -2.246630032 -0.04430110      -1.007119760 -1.195209e+00 R187562       A602
## 553  0.187573966  0.13347705       0.607128541 -5.530316e-01 R187563      A1002
## 554  1.136253649 -0.24493955      -0.222557319  1.603751e+00 R186910      A1202
## 555  0.271867602  0.13123132       0.194364795 -5.372852e-02 R187528      A1302
## 556 -0.307483319 -0.06796621      -0.403132636  1.636155e-01 R187598      A1602
## 557 -0.470219007 -0.09166757      -0.353218785 -2.533265e-02 R187527      A1802
## 558 -1.125133217 -0.12302997      -0.105983238 -8.961200e-01    Fem2     A18B02
## 559  1.377643287  0.65607024       0.220515459  5.010576e-01 P322402      A2202
## 560  1.188851780 -0.57585064       0.858158899  9.065435e-01 R187569     A22B02
## 561 -0.140126171 -0.19191731       0.259573821 -2.077827e-01 R187568      A2302
## 562 -0.987853758  0.11074157      -0.219941080 -8.786542e-01 R187518      A2602
## 563  1.047509523  0.49478584      -0.295693256  8.484169e-01 R187537      A2702
## 564  1.283927417  0.14807203      -0.085620580  1.221476e+00 R187552      C2602
## 565  1.130987805  0.43320321       0.481212154  2.165724e-01 R187595      C2402
## 566 -0.428381020  0.08368727      -0.514959327  2.891039e-03 R187594      C2302
## 567  0.362732128 -0.53760055       0.043739473  8.565932e-01 R187517      C2202
## 568 -0.523367533 -0.41891386      -0.430645382  3.261917e-01 R187400      C1902
## 569  1.209356450  0.05790010       1.024413152  1.270432e-01 R187531      C1302
## 570 -0.121709572 -0.41818211       0.676234353 -3.797618e-01 R046108       C402
## 571  0.402529306 -0.11885849       0.616489169 -9.510137e-02 R187936       C202
## 572  0.277656344 -0.45246712       0.133819566  5.963039e-01 R187399      B2502
## 573  1.139318646 -0.70081590       0.602413189  1.237721e+00 R187521      B2202
## 574 -1.419373349 -0.17867661      -0.745657461 -4.950393e-01 R187940      B2102
## 575  0.597215850 -0.44164642       0.442565782  5.962965e-01 R186908      B1702
## 576 -0.762104357  0.13756229      -0.871384879 -2.828176e-02 R187824      B1602
## 577 -0.203526876 -0.60891453      -0.468761495  8.741491e-01 R187953      B1302
## 578 -0.178656988 -0.61768622      -0.514650209  9.536794e-01 R187947      B1202
## 579 -2.171747658  0.30961413      -0.676838984 -1.804523e+00 R186912      B1102
## 580  0.869227094 -0.03138890       0.364817341  5.357987e-01    Fem3      B1002
## 581 -1.203172302 -0.86093126      -0.500091496  1.578505e-01 R187958       B902
## 582 -0.772911474  0.13954234      -0.422358557 -4.900953e-01 R187590       B502
## 583 -0.308392094 -0.26994505       0.374247902 -4.126949e-01 R187239       B202
## 584  1.865532937  0.74984886       0.338204428  7.774797e-01 R187944      D1302
## 585 -1.275304168  0.44105306      -1.107582959 -6.087743e-01 R187548      D1202
## 586 -0.920293695 -1.02464494       0.165871056 -6.151981e-02 R187963      D1102
## 587  1.306676147 -0.10710580       0.488172848  9.256091e-01 R187546      D1002
## 588 -0.851386587  0.01666119       0.683757102 -1.551805e+00 R187545       D902
## 589 -0.127925491 -0.28014159       0.379266590 -2.270505e-01 R187547       D802
## 590  1.249000939  0.56961413       0.145716067  5.336707e-01 R187540       D602
## 591  0.453697693 -0.02932911      -0.254119561  7.371464e-01 R187927       D402
## 592 -1.216340940 -0.24959309      -1.038501969  7.175412e-02 R187932       D202
## 593 -0.312262651  0.30188604      -0.273925437 -3.402232e-01 R187516       D102
## 594  0.356805742  0.15557758       0.202836199 -1.608036e-03 R187523      E2002
## 595 -1.587013458 -0.30731744      -0.546647438 -7.330486e-01 R187948      E1702
## 596 -1.313389851  0.10065470      -0.831589636 -5.824549e-01 R187577      E1602
## 597 -2.748081168 -0.77722684      -0.826758018 -1.144096e+00 R187524      E1402
## 598 -0.551188205 -0.50182336       0.034257387 -8.362223e-02 R187961      E1302
## 599  0.494441337  0.48031349      -0.243281383  2.574092e-01 R187579      E1202
## 600  0.736314732  0.23203549      -0.435990873  9.402701e-01 R187166      E1102
## 601  0.758866318 -0.01126390       0.443790283  3.263399e-01 R187582       E902
## 602  0.646349072 -0.20797162      -0.344065447  1.198386e+00 R187999       E802
## 603 -1.418613830 -0.40939224      -1.009127529 -9.406159e-05 R187588       E702
## 604  0.504301680  0.64559470      -0.009623114 -1.316699e-01 R187945       E302
## 605  1.651691654  0.72827673       0.492355325  4.310596e-01 R188000      F2702
## 606 -0.552948884  0.83826639      -0.188367525 -1.202848e+00 R187951      F2502
## 607 -0.355067567 -0.38894299       0.050193201 -1.631778e-02 R187155      F2402
## 608 -1.693323921 -0.11381696      -0.878409512 -7.010974e-01 R187559      F2102
## 609  0.366325613 -0.24379862      -0.182272429  7.923967e-01 R187557      F1902
## 610  0.457694637  0.26843304       0.460580990 -2.713194e-01 R187030      F1702
## 611  1.868083314  0.25820240       0.385871221  1.224010e+00 R187964      F1502
## 612  0.590200117  0.19087364       0.534437805 -1.351113e-01 R187575      F1102
## 613  0.009414888 -0.80854693      -0.019208663  8.371705e-01 R187592       F902
## 614  1.359959813  0.14106848       0.757415640  4.614757e-01 R187941       F202
## 615 -0.496621505 -0.50880833       0.217220378 -2.050336e-01 R187902       F102
## 616 -0.078944907 -0.03411131       0.278692241 -3.235258e-01 R187931      G2202
## 617  2.360778071  0.28588528       0.809694342  1.265198e+00 R187573      G1902
## 618 -0.367306850 -0.46941454       0.940475518 -8.383678e-01 R186902      G1802
## 619  0.025180037  0.31632193      -0.253796845 -3.734504e-02 R187916      G1602
## 620  0.330732308  0.20931909       0.352798903 -2.313857e-01 R187925      G1302
## 621  0.441063764  0.11989873       0.243359432  7.780560e-02 R187955      G1202
## 622  0.432569028 -0.61878375       0.605015873  4.463369e-01 R187914       G602
## 623  0.318889911  0.26374015      -0.054012138  1.091619e-01 R187086       G102
## 624 -2.266363336 -0.71326501      -0.610771586 -9.423267e-01 R187515      H3602
## 625  0.476636967  0.12282452      -0.444163795  7.979762e-01 R187553      H3202
## 626 -0.404137929  0.39571541      -0.493818415 -3.060349e-01 R187920      H3002
## 627 -0.369125041  0.21527245      -0.242551269 -3.418462e-01 R187512      H2802
## 628  1.213226752 -0.27011654       0.617744816  8.655985e-01 R186917      H2502
## 629  0.544778284 -0.15437165       0.706717112 -7.567175e-03 R187957      H1802
## 630  0.856000951  0.89872035      -0.063061028  2.034163e-02   Fem20      H1302
## 631 -1.001854952 -0.31030764      -0.854177618  1.626303e-01 K983388      H1102
## 632 -0.992313249 -0.61490699      -0.749259056  3.718528e-01 R187535       H502
## 633 -1.224759027 -0.13975244      -1.180998594  9.599200e-02 R186911       H302
## 634  0.223417877  0.42981414      -0.844219842  6.378236e-01 R187001       H102
## 635 -1.969252306 -0.29511313       0.162943609 -1.837083e+00 R187566       A102
## 636  0.895940500 -0.17501294       0.414572528  6.563809e-01 R187539       A302
## 637 -0.225855470  0.23618471      -0.510528606  4.848842e-02 R187343       A502
## 638 -0.799046099 -0.04430110      -1.007119760  2.523748e-01 R187562       A602
## 639  0.604069762  0.13347705       0.607128541 -1.365358e-01 R187563      A1002
## 640  0.124750905 -0.24493955      -0.222557319  5.922478e-01 R186910      A1202
## 641  0.771833234  0.13123132       0.194364795  4.462371e-01 R187528      A1302
## 642 -0.919055190 -0.06796621      -0.403132636 -4.479563e-01 R187598      A1602
## 643  1.722151342 -0.09166757      -0.353218785  2.167038e+00 R187527      A1802
## 644 -0.101478868 -0.12302997      -0.105983238  1.275343e-01    Fem2     A18B02
## 645  2.168048857  0.65607024       0.220515459  1.291463e+00 P322402      A2202
## 646  0.444549371 -0.57585064       0.858158899  1.622411e-01 R187569     A22B02
## 647  0.129177614 -0.19191731       0.259573821  6.152110e-02 R187568      A2302
## 648  0.391996899  0.11074157      -0.219941080  5.011964e-01 R187518      A2602
## 649 -0.443895193  0.49478584      -0.295693256 -6.429878e-01 R187537      A2702
## 650 -0.540897430  0.14807203      -0.085620580 -6.033489e-01 R187552      C2602
## 651  0.828823483  0.43320321       0.481212154 -8.559188e-02 R187595      C2402
## 652 -0.166041079  0.08368727      -0.514959327  2.652310e-01 R187594      C2302
## 653 -0.343485143 -0.53760055       0.043739473  1.503759e-01 R187517      C2202
## 654 -2.169847251 -0.41891386      -0.430645382 -1.320288e+00 R187400      C1902
## 655  2.380492827  0.05790010       1.024413152  1.298180e+00 R187531      C1302
## 656  1.393627721  0.58853159       0.351259258  4.538369e-01 R187398       C602
## 657  0.335158625 -0.41818211       0.676234353  7.710638e-02 R046108       C402
## 658  0.510060200 -0.11885849       0.616489169  1.242952e-02 R187936       C202
## 659 -0.728559140 -0.45246712       0.133819566 -4.099116e-01 R187399      B2502
## 660 -1.762902378 -0.70081590       0.602413189 -1.664500e+00 R187521      B2202
## 661 -0.753440255 -0.17867661      -0.745657461  1.708938e-01 R187940      B2102
## 662 -0.204950479  0.24221815       0.039234815 -4.864034e-01 R187937      B1902
## 663 -1.457406390 -0.44164642       0.442565782 -1.458326e+00 R186908      B1702
## 664 -0.563508001  0.13756229      -0.871384879  1.703146e-01 R187824      B1602
## 665 -0.668084566  0.12343192       0.076790110 -8.683066e-01 R187000      B1402
## 666 -0.821852913 -0.60891453      -0.468761495  2.558231e-01 R187953      B1302
## 667 -1.577915269 -0.61768622      -0.514650209 -4.455788e-01 R187947      B1202
## 668 -0.831939794  0.30961413      -0.676838984 -4.647149e-01 R186912      B1102
## 669  0.667521298 -0.03138890       0.364817341  3.340929e-01    Fem3      B1002
## 670 -0.504527659 -0.86093126      -0.500091496  8.564951e-01 R187958       B902
## 671 -0.258537753  0.13954234      -0.422358557  2.427846e-02 R187590       B502
## 672 -0.082846381 -0.26994505       0.374247902 -1.871492e-01 R187239       B202
## 673  0.629590055  0.74984886       0.338204428 -4.584632e-01 R187944      D1302
## 674 -0.305172738  0.44105306      -1.107582959  3.613572e-01 R187548      D1202
## 675  0.136147871 -0.10710580       0.488172848 -2.449192e-01 R187546      D1002
## 676  1.584876017  0.01666119       0.683757102  8.844577e-01 R187545       D902
## 677  0.264430917 -0.28014159       0.379266590  1.653059e-01 R187547       D802
## 678  1.085226395  0.56961413       0.145716067  3.698962e-01 R187540       D602
## 679 -0.970525117 -0.02932911      -0.254119561 -6.870764e-01 R187927       D402
## 680 -0.897726704 -0.24959309      -1.038501969  3.903684e-01 R187932       D202
## 681  1.536602201  0.30188604      -0.273925437  1.508642e+00 R187516       D102
## 682 -0.300611787  0.15557758       0.202836199 -6.590256e-01 R187523      E2002
## 683 -0.503199638 -0.17009185      -0.337655202  4.547414e-03 R187930      E1902
## 684 -0.824648188 -0.36458784      -1.115732648  6.556723e-01 R187292      E1802
## 685 -0.044135779 -0.30731744      -0.546647438  8.098291e-01 R187948      E1702
## 686 -0.667591904  0.10065470      -0.831589636  6.334304e-02 R187577      E1602
## 687 -0.876989105 -0.77722684      -0.826758018  7.269958e-01 R187524      E1402
## 688 -1.068798815 -0.50182336       0.034257387 -6.012328e-01 R187961      E1302
## 689 -0.328553920  0.48031349      -0.243281383 -5.655860e-01 R187579      E1202
## 690  0.417197095  0.23203549      -0.435990873  6.211525e-01 R187166      E1102
## 691 -0.285439547 -0.01126390       0.443790283 -7.179659e-01 R187582       E902
## 692 -0.326741443 -0.20797162      -0.344065447  2.252956e-01 R187999       E802
## 693 -1.716048821 -0.40939224      -1.009127529 -2.975291e-01 R187588       E702
## 694  0.136630133 -0.55615875       0.383282049  3.095068e-01 R187541       E402
## 695  0.065539199  0.64559470      -0.009623114 -5.704324e-01 R187945       E302
## 696  1.137525723  0.72827673       0.492355325 -8.310633e-02 R188000      F2702
## 697  0.905584288  0.83826639      -0.188367525  2.556854e-01 R187951      F2502
## 698 -0.749531899 -0.11381696      -0.878409512  2.426946e-01 R187559      F2102
## 699  0.294146439 -0.24379862      -0.182272429  7.202175e-01 R187557      F1902
## 700  0.032349409  0.26843304       0.460580990 -6.966646e-01 R187030      F1702
## 701  0.473869810  0.25820240       0.385871221 -1.702038e-01 R187964      F1502
## 702  1.108975429  0.19087364       0.534437805  3.836640e-01 R187575      F1102
## 703 -1.642863811 -0.80854693      -0.019208663 -8.151082e-01 R187592       F902
## 704  0.022325109  0.14106848       0.757415640 -8.761590e-01 R187941       F202
## 705  0.053052703 -0.50880833       0.217220378  3.446407e-01 R187902       F102
## 706 -0.411776711 -0.03411131       0.278692241 -6.563576e-01 R187931      G2202
## 707 -0.546120752  0.28588528       0.809694342 -1.641700e+00 R187573      G1902
## 708  0.035069397 -0.46941454       0.940475518 -4.359916e-01 R186902      G1802
## 709  1.912977675  0.36856568       1.379487305  1.649247e-01 R186901      G1702
## 710  0.128454874  0.31632193      -0.253796845  6.592979e-02 R187916      G1602
## 711  1.685913228  0.20931909       0.352798903  1.123795e+00 R187925      G1302
## 712  0.167556053  0.11989873       0.243359432 -1.957021e-01 R187955      G1202
## 713 -0.381126246 -0.61878375       0.605015873 -3.673584e-01 R187914       G602
## 714 -0.649902087  0.11487289       0.188674120 -9.534491e-01 R187571       G502
## 715 -0.988072691  0.07978973      -0.515131678 -5.527307e-01 R187926       G402
## 716  0.511248510  0.26374015      -0.054012138  3.015205e-01 R187086       G102
## 717 -2.114748072 -0.71326501      -0.610771586 -7.907115e-01 R187515      H3602
## 718 -0.835240358  0.12282452      -0.444163795 -5.139011e-01 R187553      H3202
## 719 -1.722859448 -0.34018223      -0.687410340 -6.952669e-01 R187918      H3102
## 720  0.198364482  0.39571541      -0.493818415  2.964675e-01 R187920      H3002
## 721  0.245100689  0.21527245      -0.242551269  2.723795e-01 R187512      H2802
## 722  0.516198949 -0.27011654       0.617744816  1.685707e-01 R186917      H2502
## 723  0.551778962 -0.15437165       0.706717112 -5.664962e-04 R187957      H1802
## 724  2.089302724  0.89872035      -0.063061028  1.253643e+00   Fem20      H1302
## 725 -1.338392739 -0.31030764      -0.854177618 -1.739075e-01 K983388      H1102
## 726 -1.045890475 -0.57686351      -0.702749543  2.337226e-01 R186903       H702
## 727 -2.113824813 -0.61490699      -0.749259056 -7.496588e-01 R187535       H502
## 728 -0.667418143 -0.13975244      -1.180998594  6.533329e-01 R186911       H302
## 729  0.124615421  0.42981414      -0.844219842  5.390211e-01 R187001       H102
## 730  0.588003761 -0.29511313       0.162943609  7.201733e-01 R187566       A102
## 731  0.217052212 -0.17501294       0.414572528 -2.250738e-02 R187539       A302
## 732  0.495732848  0.23618471      -0.510528606  7.700767e-01 R187343       A502
## 733 -1.654434500 -0.04430110      -1.007119760 -6.030136e-01 R187562       A602
## 734 -0.278575152  0.13347705       0.607128541 -1.019181e+00 R187563      A1002
## 735 -1.251406470 -0.24493955      -0.222557319 -7.839096e-01 R186910      A1202
## 736  0.174960319  0.13123132       0.194364795 -1.506358e-01 R187528      A1302
## 737 -0.534584777 -0.06796621      -0.403132636 -6.348593e-02 R187598      A1602
## 738 -2.336745916 -0.09166757      -0.353218785 -1.891860e+00 R187527      A1802
## 739  0.502843017 -0.12302997      -0.105983238  7.318562e-01    Fem2     A18B02
## 740  1.129569707  0.65607024       0.220515459  2.529840e-01 P322402      A2202
## 741 -0.422940049 -0.57585064       0.858158899 -7.052483e-01 R187569     A22B02
## 742  0.342556325 -0.19191731       0.259573821  2.748998e-01 R187568      A2302
## 743  0.567113956  0.85868651      -0.795921760  5.043492e-01 R186909      A2502
## 744 -0.673877376  0.11074157      -0.219941080 -5.646779e-01 R187518      A2602
## 745 -0.084830971  0.49478584      -0.295693256 -2.839236e-01 R187537      A2702
## 746  0.691827355  0.14807203      -0.085620580  6.293759e-01 R187552      C2602
## 747  1.432965769  0.43320321       0.481212154  5.185504e-01 R187595      C2402
## 748  0.141463461  0.08368727      -0.514959327  5.727355e-01 R187594      C2302
## 749 -0.621776271 -0.53760055       0.043739473 -1.279152e-01 R187517      C2202
## 750 -1.080246331 -0.41891386      -0.430645382 -2.306871e-01 R187400      C1902
## 751  1.062327915  0.05790010       1.024413152 -1.998534e-02 R187531      C1302
## 752 -0.333612235 -0.41818211       0.676234353 -5.916645e-01 R046108       C402
## 753 -0.474765482 -0.45246712       0.133819566 -1.561179e-01 R187399      B2502
## 754 -0.475352222 -0.70081590       0.602413189 -3.769495e-01 R187521      B2202
## 755 -0.160372737 -0.17867661      -0.745657461  7.639613e-01 R187940      B2102
## 756 -0.652219983  0.24221815       0.039234815 -9.336730e-01 R187937      B1902
## 757  0.035530530 -0.12247720      -0.992395071  1.150403e+00 R187942      B1802
## 758  0.352526536 -0.44164642       0.442565782  3.516072e-01 R186908      B1702
## 759 -1.586276629  0.13756229      -0.871384879 -8.524540e-01 R187824      B1602
## 760  0.241951071  0.12343192       0.076790110  4.172904e-02 R187000      B1402
## 761 -2.149907643 -0.60891453      -0.468761495 -1.072232e+00 R187953      B1302
## 762 -1.996432064 -0.61768622      -0.514650209 -8.640956e-01 R187947      B1202
## 763  0.371567009 -0.03138890       0.364817341  3.813857e-02    Fem3      B1002
## 764  0.494827187 -0.86093126      -0.500091496  1.855850e+00 R187958       B902
## 765 -0.922653778  0.13954234      -0.422358557 -6.398376e-01 R187590       B502
## 766  1.191663962 -0.26994505       0.374247902  1.087361e+00 R187239       B202
## 767  0.242092135  0.74984886       0.338204428 -8.459612e-01 R187944      D1302
## 768 -1.403033950  0.44105306      -1.107582959 -7.365041e-01 R187548      D1202
## 769 -1.102680539 -1.02464494       0.165871056 -2.439067e-01 R187963      D1102
## 770  0.718619851 -0.10710580       0.488172848  3.375528e-01 R187546      D1002
## 771  0.608555733  0.01666119       0.683757102 -9.186256e-02 R187545       D902
## 772  0.507521892 -0.28014159       0.379266590  4.083969e-01 R187547       D802
## 773  0.637963729  0.56961413       0.145716067 -7.736647e-02 R187540       D602
## 774 -0.700672577 -0.02932911      -0.254119561 -4.172239e-01 R187927       D402
## 775 -0.096228049 -0.24959309      -1.038501969  1.191867e+00 R187932       D202
## 776  0.540871329  0.30188604      -0.273925437  5.129107e-01 R187516       D102
## 777  0.149888910  0.15557758       0.202836199 -2.085249e-01 R187523      E2002
## 778 -0.581382944 -0.17009185      -0.337655202 -7.363589e-02 R187930      E1902
## 779 -0.103310196 -0.36458784      -1.115732648  1.377010e+00 R187292      E1802
## 780 -0.019250906 -0.30731744      -0.546647438  8.347140e-01 R187948      E1702
## 781 -0.405885628  0.10065470      -0.831589636  3.250493e-01 R187577      E1602
## 782 -1.554218501 -0.77722684      -0.826758018  4.976636e-02 R187524      E1402
## 783  0.489815618 -0.50182336       0.034257387  9.573816e-01 R187961      E1302
## 784 -0.391340048  0.48031349      -0.243281383 -6.283722e-01 R187579      E1202
## 785 -0.765741766  0.23203549      -0.435990873 -5.617864e-01 R187166      E1102
## 786 -0.962333487 -1.00728517       0.159030574 -1.140789e-01 R186907      E1002
## 787  0.776317516 -0.01126390       0.443790283  3.437911e-01 R187582       E902
## 788 -0.481895788 -0.20797162      -0.344065447  7.014128e-02 R187999       E802
## 789 -0.536509500 -0.40939224      -1.009127529  8.820103e-01 R187588       E702
## 790  1.660047825 -0.55615875       0.383282049  1.832925e+00 R187541       E402
## 791  0.947876046  0.64559470      -0.009623114  3.119045e-01 R187945       E302
## 792 -0.005737303  0.72827673       0.492355325 -1.226369e+00 R188000      F2702
## 793  0.175497999  0.83826639      -0.188367525 -4.744009e-01 R187951      F2502
## 794  0.011627751 -0.38894299       0.050193201  3.503775e-01 R187155      F2402
## 795 -1.014404134 -0.11381696      -0.878409512 -2.217766e-02 R187559      F2102
## 796  0.804473961 -0.24379862      -0.182272429  1.230545e+00 R187557      F1902
## 797 -0.124510226  0.26843304       0.460580990 -8.535243e-01 R187030      F1702
## 798  1.529713772  0.25820240       0.385871221  8.856402e-01 R187964      F1502
## 799  0.087525842  0.19087364       0.534437805 -6.377856e-01 R187575      F1102
## 800 -0.217314704 -0.80854693      -0.019208663  6.104409e-01 R187592       F902
## 801  0.183468522  0.14106848       0.757415640 -7.150156e-01 R187941       F202
## 802 -1.683385729 -0.50880833       0.217220378 -1.391798e+00 R187902       F102
## 803  0.523725892 -0.03411131       0.278692241  2.791450e-01 R187931      G2202
## 804  1.310964540  0.28588528       0.809694342  2.153849e-01 R187573      G1902
## 805  1.994120410 -0.46941454       0.940475518  1.523059e+00 R186902      G1802
## 806  2.197771519  0.36856568       1.379487305  4.497185e-01 R186901      G1702
## 807  1.114180147  0.31632193      -0.253796845  1.051655e+00 R187916      G1602
## 808  0.496747176  0.20931909       0.352798903 -6.537082e-02 R187925      G1302
## 809 -0.546099988  0.11989873       0.243359432 -9.093581e-01 R187955      G1202
## 810 -1.529475336 -0.06913447      -0.442530603 -1.017810e+00 R187637       G702
## 811 -1.098084622 -0.61878375       0.605015873 -1.084317e+00 R187914       G602
## 812  1.020858758  0.11487289       0.188674120  7.173117e-01 R187571       G502
## 813 -1.244046179  0.07978973      -0.515131678 -8.087042e-01 R187926       G402
## 814  2.169450960  0.26374015      -0.054012138  1.959723e+00 R187086       G102
## 815 -0.304290082 -0.71326501      -0.610771586  1.019747e+00 R187515      H3602
## 816  0.829260310  0.12282452      -0.444163795  1.150600e+00 R187553      H3202
## 817 -1.216682113 -0.34018223      -0.687410340 -1.890895e-01 R187918      H3102
## 818  0.226919835  0.39571541      -0.493818415  3.250228e-01 R187920      H3002
## 819 -0.254099583  0.21527245      -0.242551269 -2.268208e-01 R187512      H2802
## 820 -1.228676113  0.10711933      -0.737813082 -5.979824e-01 R187009      H2602
## 821  0.056667179 -0.27011654       0.617744816 -2.909611e-01 R186917      H2502
## 822 -0.211268944 -0.15437165       0.706717112 -7.636144e-01 R187957      H1802
## 823  1.324811800  0.89872035      -0.063061028  4.891525e-01   Fem20      H1302
## 824 -1.973729711 -0.31030764      -0.854177618 -8.092445e-01 K983388      H1102
## 825 -2.101382379 -0.57686351      -0.702749543 -8.217693e-01 R186903       H702
## 826 -1.081569149 -0.61490699      -0.749259056  2.825969e-01 R187535       H502
## 827 -1.621680910 -0.13975244      -1.180998594 -3.009299e-01 R186911       H302
## 828 -0.301262147  0.42981414      -0.844219842  1.131436e-01 R187001       H102
##     squid_pop
## 1           1
## 2           1
## 3           1
## 4           1
## 5           1
## 6           1
## 7           1
## 8           1
## 9           1
## 10          1
## 11          1
## 12          1
## 13          1
## 14          1
## 15          1
## 16          1
## 17          1
## 18          1
## 19          1
## 20          1
## 21          1
## 22          1
## 23          1
## 24          1
## 25          1
## 26          1
## 27          1
## 28          1
## 29          1
## 30          1
## 31          1
## 32          1
## 33          1
## 34          1
## 35          1
## 36          1
## 37          1
## 38          1
## 39          1
## 40          1
## 41          1
## 42          1
## 43          1
## 44          1
## 45          1
## 46          1
## 47          1
## 48          1
## 49          1
## 50          1
## 51          1
## 52          1
## 53          1
## 54          1
## 55          1
## 56          1
## 57          1
## 58          1
## 59          1
## 60          1
## 61          1
## 62          1
## 63          1
## 64          1
## 65          1
## 66          1
## 67          1
## 68          1
## 69          1
## 70          1
## 71          1
## 72          1
## 73          1
## 74          1
## 75          1
## 76          1
## 77          1
## 78          1
## 79          1
## 80          1
## 81          1
## 82          1
## 83          1
## 84          1
## 85          1
## 86          1
## 87          1
## 88          1
## 89          1
## 90          1
## 91          1
## 92          1
## 93          1
## 94          1
## 95          1
## 96          1
## 97          1
## 98          1
## 99          1
## 100         1
## 101         1
## 102         1
## 103         1
## 104         1
## 105         1
## 106         1
## 107         1
## 108         1
## 109         1
## 110         1
## 111         1
## 112         1
## 113         1
## 114         1
## 115         1
## 116         1
## 117         1
## 118         1
## 119         1
## 120         1
## 121         1
## 122         1
## 123         1
## 124         1
## 125         1
## 126         1
## 127         1
## 128         1
## 129         1
## 130         1
## 131         1
## 132         1
## 133         1
## 134         1
## 135         1
## 136         1
## 137         1
## 138         1
## 139         1
## 140         1
## 141         1
## 142         1
## 143         1
## 144         1
## 145         1
## 146         1
## 147         1
## 148         1
## 149         1
## 150         1
## 151         1
## 152         1
## 153         1
## 154         1
## 155         1
## 156         1
## 157         1
## 158         1
## 159         1
## 160         1
## 161         1
## 162         1
## 163         1
## 164         1
## 165         1
## 166         1
## 167         1
## 168         1
## 169         1
## 170         1
## 171         1
## 172         1
## 173         1
## 174         1
## 175         1
## 176         1
## 177         1
## 178         1
## 179         1
## 180         1
## 181         1
## 182         1
## 183         1
## 184         1
## 185         1
## 186         1
## 187         1
## 188         1
## 189         1
## 190         1
## 191         1
## 192         1
## 193         1
## 194         1
## 195         1
## 196         1
## 197         1
## 198         1
## 199         1
## 200         1
## 201         1
## 202         1
## 203         1
## 204         1
## 205         1
## 206         1
## 207         1
## 208         1
## 209         1
## 210         1
## 211         1
## 212         1
## 213         1
## 214         1
## 215         1
## 216         1
## 217         1
## 218         1
## 219         1
## 220         1
## 221         1
## 222         1
## 223         1
## 224         1
## 225         1
## 226         1
## 227         1
## 228         1
## 229         1
## 230         1
## 231         1
## 232         1
## 233         1
## 234         1
## 235         1
## 236         1
## 237         1
## 238         1
## 239         1
## 240         1
## 241         1
## 242         1
## 243         1
## 244         1
## 245         1
## 246         1
## 247         1
## 248         1
## 249         1
## 250         1
## 251         1
## 252         1
## 253         1
## 254         1
## 255         1
## 256         1
## 257         1
## 258         1
## 259         1
## 260         1
## 261         1
## 262         1
## 263         1
## 264         1
## 265         1
## 266         1
## 267         1
## 268         1
## 269         1
## 270         1
## 271         1
## 272         1
## 273         1
## 274         1
## 275         1
## 276         1
## 277         1
## 278         1
## 279         1
## 280         1
## 281         1
## 282         1
## 283         1
## 284         1
## 285         1
## 286         1
## 287         1
## 288         1
## 289         1
## 290         1
## 291         1
## 292         1
## 293         1
## 294         1
## 295         1
## 296         1
## 297         1
## 298         1
## 299         1
## 300         1
## 301         1
## 302         1
## 303         1
## 304         1
## 305         1
## 306         1
## 307         1
## 308         1
## 309         1
## 310         1
## 311         1
## 312         1
## 313         1
## 314         1
## 315         1
## 316         1
## 317         1
## 318         1
## 319         1
## 320         1
## 321         1
## 322         1
## 323         1
## 324         1
## 325         1
## 326         1
## 327         1
## 328         1
## 329         1
## 330         1
## 331         1
## 332         1
## 333         1
## 334         1
## 335         1
## 336         1
## 337         1
## 338         1
## 339         1
## 340         1
## 341         1
## 342         1
## 343         1
## 344         1
## 345         1
## 346         1
## 347         1
## 348         1
## 349         1
## 350         1
## 351         1
## 352         1
## 353         1
## 354         1
## 355         1
## 356         1
## 357         1
## 358         1
## 359         1
## 360         1
## 361         1
## 362         1
## 363         1
## 364         1
## 365         1
## 366         1
## 367         1
## 368         1
## 369         1
## 370         1
## 371         1
## 372         1
## 373         1
## 374         1
## 375         1
## 376         1
## 377         1
## 378         1
## 379         1
## 380         1
## 381         1
## 382         1
## 383         1
## 384         1
## 385         1
## 386         1
## 387         1
## 388         1
## 389         1
## 390         1
## 391         1
## 392         1
## 393         1
## 394         1
## 395         1
## 396         1
## 397         1
## 398         1
## 399         1
## 400         1
## 401         1
## 402         1
## 403         1
## 404         1
## 405         1
## 406         1
## 407         1
## 408         1
## 409         1
## 410         1
## 411         1
## 412         1
## 413         1
## 414         1
## 415         1
## 416         1
## 417         1
## 418         1
## 419         1
## 420         1
## 421         1
## 422         1
## 423         1
## 424         1
## 425         1
## 426         1
## 427         1
## 428         1
## 429         1
## 430         1
## 431         1
## 432         1
## 433         1
## 434         1
## 435         1
## 436         1
## 437         1
## 438         1
## 439         1
## 440         1
## 441         1
## 442         1
## 443         1
## 444         1
## 445         1
## 446         1
## 447         1
## 448         1
## 449         1
## 450         1
## 451         1
## 452         1
## 453         1
## 454         1
## 455         1
## 456         1
## 457         1
## 458         1
## 459         1
## 460         1
## 461         1
## 462         1
## 463         1
## 464         1
## 465         1
## 466         1
## 467         1
## 468         1
## 469         1
## 470         1
## 471         1
## 472         1
## 473         1
## 474         1
## 475         1
## 476         1
## 477         1
## 478         1
## 479         1
## 480         1
## 481         1
## 482         1
## 483         1
## 484         1
## 485         1
## 486         1
## 487         1
## 488         1
## 489         1
## 490         1
## 491         1
## 492         1
## 493         1
## 494         1
## 495         1
## 496         1
## 497         1
## 498         1
## 499         1
## 500         1
## 501         1
## 502         1
## 503         1
## 504         1
## 505         1
## 506         1
## 507         1
## 508         1
## 509         1
## 510         1
## 511         1
## 512         1
## 513         1
## 514         1
## 515         1
## 516         1
## 517         1
## 518         1
## 519         1
## 520         1
## 521         1
## 522         1
## 523         1
## 524         1
## 525         1
## 526         1
## 527         1
## 528         1
## 529         1
## 530         1
## 531         1
## 532         1
## 533         1
## 534         1
## 535         1
## 536         1
## 537         1
## 538         1
## 539         1
## 540         1
## 541         1
## 542         1
## 543         1
## 544         1
## 545         1
## 546         1
## 547         1
## 548         1
## 549         1
## 550         1
## 551         1
## 552         1
## 553         1
## 554         1
## 555         1
## 556         1
## 557         1
## 558         1
## 559         1
## 560         1
## 561         1
## 562         1
## 563         1
## 564         1
## 565         1
## 566         1
## 567         1
## 568         1
## 569         1
## 570         1
## 571         1
## 572         1
## 573         1
## 574         1
## 575         1
## 576         1
## 577         1
## 578         1
## 579         1
## 580         1
## 581         1
## 582         1
## 583         1
## 584         1
## 585         1
## 586         1
## 587         1
## 588         1
## 589         1
## 590         1
## 591         1
## 592         1
## 593         1
## 594         1
## 595         1
## 596         1
## 597         1
## 598         1
## 599         1
## 600         1
## 601         1
## 602         1
## 603         1
## 604         1
## 605         1
## 606         1
## 607         1
## 608         1
## 609         1
## 610         1
## 611         1
## 612         1
## 613         1
## 614         1
## 615         1
## 616         1
## 617         1
## 618         1
## 619         1
## 620         1
## 621         1
## 622         1
## 623         1
## 624         1
## 625         1
## 626         1
## 627         1
## 628         1
## 629         1
## 630         1
## 631         1
## 632         1
## 633         1
## 634         1
## 635         1
## 636         1
## 637         1
## 638         1
## 639         1
## 640         1
## 641         1
## 642         1
## 643         1
## 644         1
## 645         1
## 646         1
## 647         1
## 648         1
## 649         1
## 650         1
## 651         1
## 652         1
## 653         1
## 654         1
## 655         1
## 656         1
## 657         1
## 658         1
## 659         1
## 660         1
## 661         1
## 662         1
## 663         1
## 664         1
## 665         1
## 666         1
## 667         1
## 668         1
## 669         1
## 670         1
## 671         1
## 672         1
## 673         1
## 674         1
## 675         1
## 676         1
## 677         1
## 678         1
## 679         1
## 680         1
## 681         1
## 682         1
## 683         1
## 684         1
## 685         1
## 686         1
## 687         1
## 688         1
## 689         1
## 690         1
## 691         1
## 692         1
## 693         1
## 694         1
## 695         1
## 696         1
## 697         1
## 698         1
## 699         1
## 700         1
## 701         1
## 702         1
## 703         1
## 704         1
## 705         1
## 706         1
## 707         1
## 708         1
## 709         1
## 710         1
## 711         1
## 712         1
## 713         1
## 714         1
## 715         1
## 716         1
## 717         1
## 718         1
## 719         1
## 720         1
## 721         1
## 722         1
## 723         1
## 724         1
## 725         1
## 726         1
## 727         1
## 728         1
## 729         1
## 730         1
## 731         1
## 732         1
## 733         1
## 734         1
## 735         1
## 736         1
## 737         1
## 738         1
## 739         1
## 740         1
## 741         1
## 742         1
## 743         1
## 744         1
## 745         1
## 746         1
## 747         1
## 748         1
## 749         1
## 750         1
## 751         1
## 752         1
## 753         1
## 754         1
## 755         1
## 756         1
## 757         1
## 758         1
## 759         1
## 760         1
## 761         1
## 762         1
## 763         1
## 764         1
## 765         1
## 766         1
## 767         1
## 768         1
## 769         1
## 770         1
## 771         1
## 772         1
## 773         1
## 774         1
## 775         1
## 776         1
## 777         1
## 778         1
## 779         1
## 780         1
## 781         1
## 782         1
## 783         1
## 784         1
## 785         1
## 786         1
## 787         1
## 788         1
## 789         1
## 790         1
## 791         1
## 792         1
## 793         1
## 794         1
## 795         1
## 796         1
## 797         1
## 798         1
## 799         1
## 800         1
## 801         1
## 802         1
## 803         1
## 804         1
## 805         1
## 806         1
## 807         1
## 808         1
## 809         1
## 810         1
## 811         1
## 812         1
## 813         1
## 814         1
## 815         1
## 816         1
## 817         1
## 818         1
## 819         1
## 820         1
## 821         1
## 822         1
## 823         1
## 824         1
## 825         1
## 826         1
## 827         1
## 828         1
```



<!-- getME(mod,"theta")
library(numDeriv)
fm1Fun <- update(mod,devFunOnly=TRUE)
fm1_thpar <- getME(mod,"theta")
h <- hessian(fm1Fun, fm1_thpar)
sqrt(diag(solve(h)))

mySumm2 <- function(.) {
    c(beta=fixef(.),sigma=sigma(.), sig01=unlist(VarCorr(.)))
}
bootMer(mod,mySumm2,nsim = 100) -->


## Random slopes {#randomslopes}

Random slopes are essentially an interaction between predictors at different levels, with the random slopes being an unobserved, latent variable.

<div class="alert alert-info">

$$
y_{i} = \beta_0 + \beta_1 x_{i} + z_{1,j} + z_{2,j}x_{i}  + \epsilon_{i}
$$
$$
x \sim MVN(0,\sigma^2_{x})
$$
$$
Z \sim MVN(0,\Sigma_{Z})
$$
$$
\epsilon \sim N(0,\sigma^2_\epsilon)
$$

</div>

We can specify random slopes by simulating a slopes variable at the individual level (`ind_slope` - $z_{2,j}$). We can specify the mean environmental effect the slope of the environmental variable ($beta_1$). $z_{2,j}$ then represents the deviations from the mean slope (this is typically how it is modelling in a linear mixed effect model).
<!-- alternatively the mean environmental effect could be specified as the mean of the slopes variable with the environment slope as 0 -->
Importantly the `beta` parameter associated with `ind_slope` is specified as 0 (there is no 'main effect' of the slopes, just the interaction), and the `beta` parameter associated with interaction is 1.



```r
squid_data <- simulate_population(
  data_structure=make_structure("individual(300)",repeat_obs=10),
  parameters = list(
    individual = list(
      names = c("ind_int","ind_slope"), 
      beta = c(1,0),
      vcov = c(1,0.5)
    ),
    observation= list(
      names = c("environment"),
      beta = c(0.2)
    ), 
    residual = list(
      vcov = c(0.5)
    ),
    interactions = list(
      names = c("ind_slope:environment"),
      beta = c(1)
    )
  )
)

data <- get_population_data(squid_data)

short_summary(lmer(y ~ environment + (1+environment|individual),data))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: y ~ environment + (1 + environment | individual)
##    Data: data
## 
## REML criterion at convergence: 7929.6
## 
## Random effects:
##  Groups     Name        Variance Corr
##  individual (Intercept) 0.9537       
##             environment 0.4442   0.04
##  Residual               0.4925       
## Number of obs: 3000, groups:  individual, 300
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  0.04645    0.05799   0.801
## environment  0.24768    0.04135   5.989
```


Here we have specified no correlation between intercepts and slopes. To simulate a correlation between intercepts and slopes, we can simply give the `vcov` argument a covariance matrix, instead of two variances:


```r
squid_data <- simulate_population(
  data_structure=make_structure("individual(300)",repeat_obs=10),
  parameters = list(
    individual = list(
      names = c("ind_int","ind_slope"), 
      beta = c(1,0),
      vcov = matrix(c(1,0.3,0.3,0.5),ncol=2,nrow=2,byrow=TRUE)
    ),
    observation= list(
      names = c("environment"),
      beta = c(0.2)
    ), 
    residual = list(
      vcov = c(0.5)
    ),
    interactions = list(
      names = c("ind_slope:environment"),
      beta = c(1)
    )
  )
)

data <- get_population_data(squid_data)

short_summary(lmer(y ~ environment + (1+environment|individual),data))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: y ~ environment + (1 + environment | individual)
##    Data: data
## 
## REML criterion at convergence: 7854.1
## 
## Random effects:
##  Groups     Name        Variance Corr
##  individual (Intercept) 0.8896       
##             environment 0.4560   0.37
##  Residual               0.4884       
## Number of obs: 3000, groups:  individual, 300
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  0.04673    0.05608   0.833
## environment  0.14736    0.04179   3.526
```



## Between- and within-group effects {#BetweenWithin}

We may want to simulate the case where a predictor variable varies both within and between groups - in other words it is repeatable at the group level. For example, if we are simulating body mass, we might expect that body mass is a function of an environmental variable, rainfall, and that differs systematically between individuals, as well as within, for example due to spatial variation in where individual lives. 

The simplest way of simulating this is as two rainfall variables, one at the level of the individual and one at the level of the observation, something like:


```r
squid_data <- simulate_population(
  data_structure=make_structure("individual(100)", repeat_obs=5),
  parameters = list(
    individual=list(
      names=c("between_rainfall"), 
      ...
    ),
    observation=list(
      names=c("within_rainfall"), 
      ...
    ),
    residual=list(
      ...
    )
  )
)  
```
We can then specify the variation in rainfall within and between individuals, for example, if the repeatability of rainfall amongst individuals is 0.5, and the total variance in rainfall is 0.8, we would make the variance in rainfall 0.4 at each level:

```r
squid_data <- simulate_population(
  data_structure=make_structure("individual(100)", repeat_obs=5),
  parameters = list(
    individual=list(
      names=c("between_rainfall"), 
      vcov=0.4,
      ...
    ),
    observation=list(
      names=c("within_rainfall"), 
      vcov=0.4,
      ...
    ),
    residual=list(
      0.8
    )
  )
)  
```

If we want the rainfall variable to have a mean that is not 0, we should specify this in only one place, as otherwise they will add up weirdly!
```r
squid_data <- simulate_population(
  data_structure=make_structure("individual(100)", repeat_obs=5),
  parameters = list(
    individual=list(
      names=c("between_rainfall"), 
      vcov=0.4,
      ...
    ),
    observation=list(
      names=c("within_rainfall"), 
      vcov=0.4,
      mean=10
    ),
    residual=list(
      0.8
    )
  )
)  
```
Now we want to add in some betas. If the effect of rainfall on body mass is causal, we would expect the beta to be the same at both levels - this results as a simple reorganisation of the model equation. We start with an effect of rainfall on body size
$$ y_i = beta_0 + beta_1 (x_{1i,j}) + \epsilon_i $$
where $x_{1ij}$ is rainfall varying at levels $i$ (observation) and $j$ (individual).

We can split rainfall up into within ($x_{1i}$) and between ($z_{1i}$) individual components:

$$ y_i = beta_0 + beta_1 (z_1_j + x_1_i) + \epsilon_i $$

$$ y_i = beta_0 + beta_{1}z_1_j + beta_{1}*x_1_i + \epsilon_i $$

you see that the coefficients should be the same.



```r
squid_data <- simulate_population(
  data_structure=make_structure("individual(100)", repeat_obs=5),
  parameters = list(
    individual=list(
      names=c("between_env", "ind_int"), 
      vcov=c(0.5,1), 
      beta=c(-2,1)
    ),
    observation=list(
      names=c("within_env"), 
      vcov=c(0.5), 
      beta=c(2)
    ),
    residual=list(
      vcov=1
    )
  )
)  

data <- get_population_data(squid_data)
data$environment <- data$between_env + data$within_env
```




