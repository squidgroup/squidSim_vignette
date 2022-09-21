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
##         10.0180           0.4019
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
##         10.0005           0.4774
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
##             10.00              10.48
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
##      10.0035        0.4666
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
## 1  9.712770      1    0  -1.0789111 -0.07144816                0   1         1
## 2  9.973229      1    0   0.3827627 -0.10332401                0   1         1
## 3 10.178768      1    0   0.2988904  0.11898991                0   1         1
## 4 10.740719      1    0   2.1762381  0.30547178                0   1         1
## 5 10.350520      1    0   1.1953571  0.11144855                0   1         1
## 6 10.358600      1    0   0.9419259  0.17021443                0   1         1
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
##                  10.0060                   10.4829                    0.1926  
## factor(sex)2:environment  
##                   0.4229
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
##   0.0139543   0.0925905   0.1980361   0.2456884  -0.1044459
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
##             y individual_effect    residual individual squid_pop
## 1 -0.39657197         0.2052116 -0.60178352          1         1
## 2  0.29928285         0.2052116  0.09407130          1         1
## 3  1.64489023         1.1739907  0.47089956          2         1
## 4 -0.06726288         1.1739907 -1.24125355          2         1
## 5  0.01857982         0.3679298 -0.34934996          3         1
## 6  0.28144965         0.3679298 -0.08648013          3         1
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
## REML criterion at convergence: 2781
## 
## Random effects:
##  Groups     Name        Variance
##  individual (Intercept) 0.6719  
##  Residual               0.4847  
## Number of obs: 1000, groups:  individual, 500
## 
## Fixed effects:
##              Estimate Std. Error t value
## (Intercept) -0.001119   0.042762  -0.026
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
##                y   dam_effect fosternest_effect      residual     dam
## 1   -0.599637837 -0.163017161     -0.0665506946 -3.700700e-01 R187557
## 2    0.605351533 -0.264026422      0.5179401614  3.514378e-01 R187559
## 3   -1.899452387 -0.473220015     -1.1106481578 -3.155842e-01 R187568
## 4   -0.710223246 -0.106400866     -0.3388679569 -2.649544e-01 R187518
## 5   -0.261536387 -0.514008086      0.7715733294 -5.191016e-01 R187528
## 6   -1.364171684  0.417816418      0.2203211850 -2.002309e+00 R187945
## 7   -0.632619211 -0.541012302     -0.0001817107 -9.142520e-02    Fem3
## 8    0.862568975 -0.444761774     -0.2393953208  1.546726e+00 R187030
## 9    0.391450082 -0.336150949      0.5047938274  2.228072e-01 R187517
## 10   2.128813044 -0.137640164      0.9336440587  1.332809e+00 R187523
## 11   0.692604218 -0.357311121      0.0996140063  9.503013e-01 R186902
## 12  -0.026018711 -0.510422765     -0.7103398880  1.194744e+00 R187400
## 13  -0.588721778 -0.321879131     -0.4489854877  1.821428e-01 R187932
## 14   0.228665670 -0.421845891      0.1852000939  4.653115e-01 R187582
## 15  -0.224984705 -0.621685378     -0.3397460945  7.364468e-01 R187545
## 16   2.813875030  1.010793240      1.2293402424  5.737415e-01 R187546
## 17  -1.597873165 -0.118518345      0.1092330270 -1.588588e+00 R187590
## 18  -0.606180178  0.343150256     -0.5486846241 -4.006458e-01 R187548
## 19  -0.745383205 -0.182867012      0.2172279676 -7.797442e-01 R187594
## 20  -0.454520901  0.149948422     -0.4860586210 -1.184107e-01 R187588
## 21   0.604775199 -0.163017161     -0.0665506946  8.343431e-01 R187557
## 22  -0.113721746 -0.264026422      0.5179401614 -3.676355e-01 R187559
## 23  -0.646010479  0.128929314     -0.3694369421 -4.055029e-01 R187531
## 24  -1.873847264 -0.497971284     -0.3716266513 -1.004249e+00 R187592
## 25  -0.406079419 -0.207915112      0.0485953394 -2.467596e-01 R187575
## 26  -1.458036456 -0.692326750      0.9233292259 -1.689039e+00 R186912
## 27   0.761289079  0.108474565      0.2820076156  3.708069e-01 R187914
## 28  -0.581218807 -0.111108353      0.4560975644 -9.262080e-01 R187955
## 29   0.386884373  0.310917487      0.1066508524 -3.068397e-02 R187535
## 30   0.579065178 -0.164335670      0.2649099669  4.784909e-01 K983388
## 31  -0.624914003  0.049599994      0.2139443351 -8.884583e-01   Fem20
## 32  -0.234437213  0.105776164     -0.2365440378 -1.036693e-01 R187086
## 33   0.444747101 -0.248288194      0.5204078520  1.726274e-01 R187539
## 34   0.295655924  0.212760356     -0.4565467688  5.394423e-01 R187566
## 35   0.467530719  0.287343801     -0.5933475065  7.735344e-01 R187569
## 36  -0.588700731 -0.473220015     -1.1106481578  9.951674e-01 R187568
## 37  -0.788995563 -0.083986784      0.4883147794 -1.193324e+00 R187537
## 38   0.264265037 -0.106400866     -0.3388679569  7.095339e-01 R187518
## 39  -0.428252454 -0.035550637      0.0872542252 -4.799560e-01 R187916
## 40  -0.473627301 -0.396178271     -0.0686755334 -8.773497e-03 R186903
## 41   1.963133276  0.017310351      0.6184896802  1.327333e+00 R187512
## 42  -1.062618696 -0.502686995     -0.3385350669 -2.213966e-01 R187562
## 43  -0.444304210 -0.514008086      0.7715733294 -7.018695e-01 R187528
## 44  -0.312923602 -0.335821824      0.5478883067 -5.249901e-01 R187563
## 45  -1.068653013 -0.393278029     -0.4418149384 -2.335600e-01 R187571
## 46   0.610820250  0.417816418      0.2203211850 -2.731735e-02 R187945
## 47  -0.345286014 -0.541012302     -0.0001817107  1.959080e-01    Fem3
## 48  -0.352613537 -0.444761774     -0.2393953208  3.315436e-01 R187030
## 49  -0.407826802 -0.336150949      0.5047938274 -5.764697e-01 R187517
## 50   0.147400745 -0.137640164      0.9336440587 -6.486031e-01 R187523
## 51   0.208414563 -0.357311121      0.0996140063  4.661117e-01 R186902
## 52  -0.547961945  0.169285742     -0.1331679790 -5.840797e-01 R187958
## 53  -1.576420258 -0.710002242     -0.2303746227 -6.360434e-01 R187953
## 54  -0.898329518 -0.321879131     -0.4489854877 -1.274649e-01 R187932
## 55   1.430532098 -0.075916280      0.9221902441  5.842581e-01 R187547
## 56  -2.173001856 -0.139669927     -1.5435048892 -4.898270e-01 R187947
## 57  -0.659148275 -0.421845891      0.1852000939 -4.225025e-01 R187582
## 58   0.650377778 -0.621685378     -0.3397460945  1.611809e+00 R187545
## 59   2.709851786  1.010793240      1.2293402424  4.697183e-01 R187546
## 60   1.455526516  0.793105370     -0.1415687127  8.039899e-01 R187239
## 61  -1.018763020 -0.118518345      0.1092330270 -1.009478e+00 R187590
## 62  -1.305352187 -0.119850923     -0.6206857675 -5.648155e-01 R187521
## 63   1.418584317  0.609554100      0.8260829262 -1.705271e-02 R187931
## 64  -0.906746248  0.040298744     -0.6479762732 -2.990687e-01 R187577
## 65   1.281439924  0.496768873      0.6753737157  1.092973e-01 R187292
## 66   0.258963251 -0.077789199      0.1100035748  2.267489e-01 R187516
## 67   0.958695485 -0.070032377      0.5911670099  4.375609e-01 R187166
## 68  -1.029835057 -0.586214959     -0.2566015242 -1.870186e-01 R187579
## 69   0.066366946  0.343150256     -0.5486846241  2.719013e-01 R187548
## 70   0.061532674 -0.124913243     -0.2223895132  4.088354e-01 R187155
## 71   0.552371357 -0.182867012      0.2172279676  5.180104e-01 R187594
## 72   0.577477764  0.149948422     -0.4860586210  9.135880e-01 R187588
## 73  -0.060426697 -0.163017161     -0.0665506946  1.691412e-01 R187557
## 74   0.049886368 -0.264026422      0.5179401614 -2.040274e-01 R187559
## 75  -0.728940337  0.128929314     -0.3694369421 -4.884327e-01 R187531
## 76  -0.978798005 -0.113329566      0.2130797673 -1.078548e+00 R187963
## 77  -0.883595325 -0.497971284     -0.3716266513 -1.399739e-02 R187592
## 78   0.417286714 -0.207915112      0.0485953394  5.766065e-01 R187575
## 79   0.598348529  0.336575748     -0.1578358156  4.196086e-01    Fem5
## 80  -0.191205412  0.074285891     -0.3132066418  4.771534e-02 R187930
## 81  -0.251743833 -0.435732821      0.2254438434 -4.145486e-02 R187598
## 82  -0.132225424  0.108474565      0.2820076156 -5.227076e-01 R187914
## 83  -0.288641541  0.601863348      0.0503159518 -9.408208e-01 R187957
## 84   0.066551893 -0.111108353      0.4560975644 -2.784373e-01 R187955
## 85   0.351107195  0.493320742     -0.4726758719  3.304623e-01 R187552
## 86  -0.659962069  0.310917487      0.1066508524 -1.077530e+00 R187535
## 87  -0.123968449 -0.041896884     -0.2866655166  2.045940e-01 R187527
## 88   0.800829789  0.043832264      0.4302394334  3.267581e-01 R187595
## 89   1.541303896  0.335295749      0.7535746524  4.524335e-01 P322402
## 90  -0.283567720 -0.164335670      0.2649099669 -3.841420e-01 K983388
## 91  -0.795880652  0.049599994      0.2139443351 -1.059425e+00   Fem20
## 92   0.999176882  0.105776164     -0.2365440378  1.129945e+00 R187086
## 93  -0.211254958 -0.248288194      0.5204078520 -4.833746e-01 R187539
## 94  -1.351152327  0.212760356     -0.4565467688 -1.107366e+00 R187566
## 95  -1.120001252  0.287343801     -0.5933475065 -8.139975e-01 R187569
## 96  -3.068939078 -0.473220015     -1.1106481578 -1.485071e+00 R187568
## 97  -0.260426037 -0.083986784      0.4883147794 -6.647540e-01 R187537
## 98  -0.664771573 -0.106400866     -0.3388679569 -2.195028e-01 R187518
## 99  -0.733134078 -0.035550637      0.0872542252 -7.848377e-01 R187916
## 100  0.962969664 -0.295128879      0.0138170032  1.244282e+00 R187515
## 101 -1.738030045 -0.396178271     -0.0686755334 -1.273176e+00 R186903
## 102  0.862107627  0.017310351      0.6184896802  2.263076e-01 R187512
## 103 -0.419433165 -0.502686995     -0.3385350669  4.217889e-01 R187562
## 104  0.211885775 -0.514008086      0.7715733294 -4.567947e-02 R187528
## 105 -1.021879405 -0.335821824      0.5478883067 -1.233946e+00 R187563
## 106 -0.027956871 -0.393278029     -0.4418149384  8.071361e-01 R187571
## 107 -0.230424615 -0.593703220      0.0428102208  3.204684e-01 R187553
## 108  1.566082333  0.417816418      0.2203211850  9.279447e-01 R187945
## 109 -0.679131634 -0.541012302     -0.0001817107 -1.379376e-01    Fem3
## 110  0.640503713 -0.444761774     -0.2393953208  1.324661e+00 R187030
## 111 -0.316317102 -0.336150949      0.5047938274 -4.849600e-01 R187517
## 112 -0.712400085 -0.434652239      0.0997371078 -3.774850e-01 R046109
## 113 -0.006425425  0.227745624      0.2719944416 -5.061655e-01 R187920
## 114  1.344291773  0.395765728      0.9486077910 -8.174574e-05 R186907
## 115  0.311258915  0.156030971      0.2164029348 -6.117499e-02 R187541
## 116  1.642900886 -0.137640164      0.9336440587  8.468970e-01 R187523
## 117  0.098034561  0.113219894      0.1112327346 -1.264181e-01 R187927
## 118 -2.868738715 -1.001253606     -1.1116736132 -7.558115e-01 R187942
## 119  2.258485833  0.537510497      0.6643183101  1.056657e+00 R187937
## 120 -1.985705540 -0.684522175     -0.8433136269 -4.578697e-01 R187824
## 121 -0.578864913 -0.222929754      0.5038999872 -8.598351e-01 R186908
## 122 -0.580236522 -0.357311121      0.0996140063 -3.225394e-01 R186902
## 123  0.584604858  0.169285742     -0.1331679790  5.484871e-01 R187958
## 124  1.421974754  0.223412678      0.0608813533  1.137681e+00 R187944
## 125 -1.234720139 -0.510422765     -0.7103398880 -1.395749e-02 R187400
## 126 -1.377152395 -0.710002242     -0.2303746227 -4.367755e-01 R187953
## 127 -1.774153838 -0.321879131     -0.4489854877 -1.003289e+00 R187932
## 128  0.947449133 -0.075916280      0.9221902441  1.011752e-01 R187547
## 129 -1.442500131 -0.139669927     -1.5435048892  2.406747e-01 R187947
## 130  0.010142815 -0.421845891      0.1852000939  2.467886e-01 R187582
## 131 -0.977031742 -0.204957214     -0.1853466137 -5.867279e-01 R187964
## 132 -1.176408077 -0.621685378     -0.3397460945 -2.149766e-01 R187545
## 133  2.709407944  1.010793240      1.2293402424  4.692745e-01 R187546
## 134  1.410141227  0.793105370     -0.1415687127  7.586046e-01 R187239
## 135  0.253296473  0.006571449      0.5823362771 -3.356113e-01 R187940
## 136  0.127437566 -0.118518345      0.1092330270  1.367229e-01 R187590
## 137  0.596725601  0.242642367      0.6177094439 -2.636262e-01 R187524
## 138 -1.054707700 -0.119850923     -0.6206857675 -3.141710e-01 R187521
## 139  1.263305944  0.609554100      0.8260829262 -1.723311e-01 R187931
## 140 -0.219463602  0.040298744     -0.6479762732  3.882139e-01 R187577
## 141  0.157695005  1.211541972     -0.0469942069 -1.006853e+00 R188000
## 142  0.337331641  0.496768873      0.6753737157 -8.348109e-01 R187292
## 143  0.556140411 -0.077789199      0.1100035748  5.239260e-01 R187516
## 144  2.076230026  0.487128643     -0.2262148874  1.815316e+00 R187999
## 145  0.476559359 -0.070032377      0.5911670099 -4.457527e-02 R187166
## 146  0.774530723  0.343150256     -0.5486846241  9.800651e-01 R187548
## 147  2.054069290  0.430896974      0.0641166434  1.559056e+00 R187961
## 148  0.232420826 -0.124913243     -0.2223895132  5.797236e-01 R187155
## 149  0.919051272  0.441362485     -0.5055094021  9.831982e-01 R187399
## 150 -0.779739061 -0.182867012      0.2172279676 -8.141000e-01 R187594
## 151 -0.890964659  0.149948422     -0.4860586210 -5.548545e-01 R187588
## 152 -0.034432696 -0.163017161     -0.0665506946  1.951352e-01 R187557
## 153 -0.537584764 -0.264026422      0.5179401614 -7.914985e-01 R187559
## 154 -0.541834606  0.128929314     -0.3694369421 -3.013270e-01 R187531
## 155  0.125408973 -0.113329566      0.2130797673  2.565877e-02 R187963
## 156 -0.858501606 -0.497971284     -0.3716266513  1.109633e-02 R187592
## 157 -0.874281642 -0.207915112      0.0485953394 -7.149619e-01 R187575
## 158  0.991632835  0.336575748     -0.1578358156  8.128929e-01    Fem5
## 159 -0.113573472 -0.692326750      0.9233292259 -3.445759e-01 R186912
## 160  1.158885944  0.074285891     -0.3132066418  1.397807e+00 R187930
## 161 -1.033079281  0.317042901     -0.5780155224 -7.721067e-01 R187513
## 162 -0.448685987 -0.435732821      0.2254438434 -2.383970e-01 R187598
## 163  1.034319772  0.256576191      0.3204356724  4.573079e-01 R187918
## 164  1.783550511  0.601863348      0.0503159518  1.131371e+00 R187957
## 165 -0.320002702 -0.111108353      0.4560975644 -6.649919e-01 R187955
## 166 -1.486796294  0.493320742     -0.4726758719 -1.507441e+00 R187552
## 167  0.087168301  0.310917487      0.1066508524 -3.304000e-01 R187535
## 168  0.185835334 -0.041896884     -0.2866655166  5.143977e-01 R187527
## 169  1.339015005  0.043832264      0.4302394334  8.649433e-01 R187595
## 170 -0.484208623 -0.028121739     -0.7358062033  2.797193e-01 R187925
## 171  0.999132566  0.747380550      0.0171228734  2.346291e-01 R187936
## 172  1.036616671  0.335295749      0.7535746524 -5.225373e-02 P322402
## 173  1.111554967 -0.164335670      0.2649099669  1.010981e+00 K983388
## 174 -0.175218014  0.049599994      0.2139443351 -4.387623e-01   Fem20
## 175 -0.816579628 -0.435569352      0.4865132854 -8.675236e-01    Fem2
## 176 -0.980290762  0.105776164     -0.2365440378 -8.495229e-01 R187086
## 177  1.468327810 -0.248288194      0.5204078520  1.196208e+00 R187539
## 178  0.324341925  0.212760356     -0.4565467688  5.681283e-01 R187566
## 179 -0.838742026  0.287343801     -0.5933475065 -5.327383e-01 R187569
## 180 -1.908204636 -0.473220015     -1.1106481578 -3.243365e-01 R187568
## 181 -0.224206428 -0.083986784      0.4883147794 -6.285344e-01 R187537
## 182 -0.867801260 -0.106400866     -0.3388679569 -4.225324e-01 R187518
## 183  0.482786383 -0.035550637      0.0872542252  4.310828e-01 R187916
## 184  0.116524509 -0.295128879      0.0138170032  3.978364e-01 R187515
## 185 -1.506001785 -0.396178271     -0.0686755334 -1.041148e+00 R186903
## 186  2.153989132  0.017310351      0.6184896802  1.518189e+00 R187512
## 187 -1.616163506 -0.003460548     -1.3161538186 -2.965491e-01 R187343
## 188 -1.265031168 -0.502686995     -0.3385350669 -4.238091e-01 R187562
## 189  0.715494150  0.004618578      0.5371418026  1.737338e-01 R186911
## 190  0.231450476 -0.514008086      0.7715733294 -2.611477e-02 R187528
## 191 -0.701339388 -0.335821824      0.5478883067 -9.134059e-01 R187563
## 192 -0.079173953 -0.393278029     -0.4418149384  7.559190e-01 R187571
## 193 -0.287207299 -0.593703220      0.0428102208  2.636857e-01 R187553
## 194  0.256789235  0.417816418      0.2203211850 -3.813484e-01 R187945
## 195 -0.244705562 -0.541012302     -0.0001817107  2.964885e-01    Fem3
## 196 -0.090541594 -0.444761774     -0.2393953208  5.936155e-01 R187030
## 197  1.043884681 -0.336150949      0.5047938274  8.752418e-01 R187517
## 198 -0.426037982 -0.434652239      0.0997371078 -9.112285e-02 R046109
## 199  0.810167677  0.227745624      0.2719944416  3.104276e-01 R187920
## 200  0.918506952  0.395765728      0.9486077910 -4.258666e-01 R186907
## 201  1.293463984  0.156030971      0.2164029348  9.210301e-01 R187541
## 202  0.163302953 -0.137640164      0.9336440587 -6.327009e-01 R187523
## 203 -2.740735004 -1.001253606     -1.1116736132 -6.278078e-01 R187942
## 204  2.010446857  0.537510497      0.6643183101  8.086181e-01 R187937
## 205 -1.861802810 -0.684522175     -0.8433136269 -3.339670e-01 R187824
## 206 -0.376299129 -0.222929754      0.5038999872 -6.572694e-01 R186908
## 207 -0.829731933 -0.357311121      0.0996140063 -5.720348e-01 R186902
## 208  0.404640479  0.169285742     -0.1331679790  3.685227e-01 R187958
## 209  1.268250605  0.223412678      0.0608813533  9.839566e-01 R187944
## 210 -0.797364503 -0.175968644     -0.2089918958 -4.124040e-01 R187902
## 211 -0.035787871 -0.510422765     -0.7103398880  1.184975e+00 R187400
## 212 -1.892855833 -0.710002242     -0.2303746227 -9.524790e-01 R187953
## 213 -1.408324090 -0.321879131     -0.4489854877 -6.374595e-01 R187932
## 214  1.242295711 -0.075916280      0.9221902441  3.960217e-01 R187547
## 215 -1.657499424 -0.139669927     -1.5435048892  2.567539e-02 R187947
## 216  1.126175869 -0.421845891      0.1852000939  1.362822e+00 R187582
## 217 -0.833985870 -0.204957214     -0.1853466137 -4.436820e-01 R187964
## 218 -0.750552544 -0.621685378     -0.3397460945  2.108789e-01 R187545
## 219  2.409844328  1.010793240      1.2293402424  1.697108e-01 R187546
## 220  0.778092046  0.793105370     -0.1415687127  1.265554e-01 R187239
## 221  0.339426664  0.006571449      0.5823362771 -2.494811e-01 R187940
## 222  0.620664016 -0.118518345      0.1092330270  6.299493e-01 R187590
## 223  1.587880914  0.242642367      0.6177094439  7.275291e-01 R187524
## 224 -0.533474969 -0.119850923     -0.6206857675  2.070617e-01 R187521
## 225  0.555793227  0.609554100      0.8260829262 -8.798438e-01 R187931
## 226 -0.562100834  0.040298744     -0.6479762732  4.557670e-02 R187577
## 227  2.131222167  1.211541972     -0.0469942069  9.666744e-01 R188000
## 228  1.135511529  0.496768873      0.6753737157 -3.663106e-02 R187292
## 229  0.540222439 -0.077789199      0.1100035748  5.080081e-01 R187516
## 230  0.575658795  0.487128643     -0.2262148874  3.147450e-01 R187999
## 231  0.501005793 -0.070032377      0.5911670099 -2.012884e-02 R187166
## 232 -1.005899248 -0.586214959     -0.2566015242 -1.630828e-01 R187579
## 233  0.177122735  0.343150256     -0.5486846241  3.826571e-01 R187548
## 234 -1.002226927  0.430896974      0.0641166434 -1.497241e+00 R187961
## 235 -0.673658677 -0.124913243     -0.2223895132 -3.263559e-01 R187155
## 236 -1.276410099  0.441362485     -0.5055094021 -1.212263e+00 R187399
## 237  0.545041769 -0.182867012      0.2172279676  5.106808e-01 R187594
## 238  0.922292393  0.679561322      0.7697028054 -5.269717e-01 R187948
## 239 -1.359117216  0.149948422     -0.4860586210 -1.023007e+00 R187588
## 240  0.314920086 -0.163017161     -0.0665506946  5.444879e-01 R187557
## 241  0.879673352 -0.264026422      0.5179401614  6.257596e-01 R187559
## 242 -0.831767077  0.128929314     -0.3694369421 -5.912594e-01 R187531
## 243  0.824924168 -0.113329566      0.2130797673  7.251740e-01 R187963
## 244 -0.101298887 -0.497971284     -0.3716266513  7.682990e-01 R187592
## 245  0.856969853 -0.207915112      0.0485953394  1.016290e+00 R187575
## 246 -0.959204513  0.336575748     -0.1578358156 -1.137944e+00    Fem5
## 247 -0.032133170 -0.692326750      0.9233292259 -2.631356e-01 R186912
## 248  0.311533744  0.074285891     -0.3132066418  5.504545e-01 R187930
## 249  0.662195815  0.317042901     -0.5780155224  9.231684e-01 R187513
## 250  0.227181213  0.006140183      0.1072109833  1.138300e-01 R187000
## 251  2.343366313  0.256576191      0.3204356724  1.766354e+00 R187918
## 252 -0.382110325  0.108474565      0.2820076156 -7.725925e-01 R187914
## 253  0.606938864  0.601863348      0.0503159518 -4.524044e-02 R187957
## 254  0.655436969 -0.111108353      0.4560975644  3.104478e-01 R187955
## 255  0.369253873  0.493320742     -0.4726758719  3.486090e-01 R187552
## 256  0.210095386  0.310917487      0.1066508524 -2.074730e-01 R187535
## 257 -1.060288705 -0.041896884     -0.2866655166 -7.317263e-01 R187527
## 258  0.065852161  0.043832264      0.4302394334 -4.082195e-01 R187595
## 259 -0.659485432 -0.028121739     -0.7358062033  1.044425e-01 R187925
## 260  0.457062739  0.747380550      0.0171228734 -3.074407e-01 R187936
## 261  0.498309039  0.335295749      0.7535746524 -5.905614e-01 P322402
## 262 -0.014638444 -0.190720306     -0.5735198159  7.496017e-01 R186918
## 263 -0.986680904  0.276996765     -0.4993565477 -7.643211e-01 R187637
## 264 -0.287719216 -0.164335670      0.2649099669 -3.882935e-01 K983388
## 265 -0.803722032  0.049599994      0.2139443351 -1.067266e+00   Fem20
## 266  0.081572036 -0.435569352      0.4865132854  3.062810e-02    Fem2
## 267  0.868339819  0.105776164     -0.2365440378  9.991077e-01 R187086
## 268 -0.322902951 -0.248288194      0.5204078520 -5.950226e-01 R187539
## 269  0.369762295  0.212760356     -0.4565467688  6.135487e-01 R187566
## 270  0.137616183  0.287343801     -0.5933475065  4.436199e-01 R187569
## 271 -0.060515343 -0.473220015     -1.1106481578  1.523353e+00 R187568
## 272 -0.003125192 -0.083986784      0.4883147794 -4.074532e-01 R187537
## 273 -0.509385980 -0.106400866     -0.3388679569 -6.411716e-02 R187518
## 274  0.689165625 -0.035550637      0.0872542252  6.374620e-01 R187916
## 275 -0.430693240 -0.295128879      0.0138170032 -1.493814e-01 R187515
## 276  0.552357218 -0.396178271     -0.0686755334  1.017211e+00 R186903
## 277 -0.326906442  0.017310351      0.6184896802 -9.627065e-01 R187512
## 278 -0.978371995 -0.003460548     -1.3161538186  3.412424e-01 R187343
## 279 -0.212983503 -0.502686995     -0.3385350669  6.282386e-01 R187562
## 280  0.463841462  0.004618578      0.5371418026 -7.791892e-02 R186911
## 281  0.225332102 -0.514008086      0.7715733294 -3.223314e-02 R187528
## 282 -0.987625086 -0.335821824      0.5478883067 -1.199692e+00 R187563
## 283 -0.041048126 -0.393278029     -0.4418149384  7.940448e-01 R187571
## 284 -1.298007085 -0.593703220      0.0428102208 -7.471141e-01 R187553
## 285  0.514510081  0.417816418      0.2203211850 -1.236275e-01 R187945
## 286  2.240209716  0.485597021      0.0108027388  1.743810e+00 R187398
## 287 -1.248728200 -0.541012302     -0.0001817107 -7.075342e-01    Fem3
## 288 -0.538565729 -0.444761774     -0.2393953208  1.455914e-01 R187030
## 289  0.053254084 -0.336150949      0.5047938274 -1.153888e-01 R187517
## 290  0.895571387 -0.434652239      0.0997371078  1.230487e+00 R046109
## 291  1.173775577  0.227745624      0.2719944416  6.740355e-01 R187920
## 292  0.369273614  0.156030971      0.2164029348 -3.160292e-03 R187541
## 293  0.662677018 -0.137640164      0.9336440587 -1.333269e-01 R187523
## 294  1.320833389  0.113219894      0.1112327346  1.096381e+00 R187927
## 295 -3.240005039 -1.001253606     -1.1116736132 -1.127078e+00 R187942
## 296  0.748585479  0.537510497      0.6643183101 -4.532433e-01 R187937
## 297 -1.264000696 -0.684522175     -0.8433136269  2.638351e-01 R187824
## 298  0.195245511 -0.222929754      0.5038999872 -8.572472e-02 R186908
## 299 -1.147908494 -0.357311121      0.0996140063 -8.902114e-01 R186902
## 300  0.781670321  0.169285742     -0.1331679790  7.455526e-01 R187958
## 301  0.559421497  0.223412678      0.0608813533  2.751275e-01 R187944
## 302 -0.626822412 -0.175968644     -0.2089918958 -2.418619e-01 R187902
## 303 -0.396627202 -0.510422765     -0.7103398880  8.241355e-01 R187400
## 304 -0.519383122 -0.710002242     -0.2303746227  4.209937e-01 R187953
## 305 -1.188921359 -0.321879131     -0.4489854877 -4.180567e-01 R187932
## 306  0.971191961 -0.075916280      0.9221902441  1.249180e-01 R187547
## 307 -1.662665245 -0.139669927     -1.5435048892  2.050957e-02 R187947
## 308 -1.364002048 -0.421845891      0.1852000939 -1.127356e+00 R187582
## 309 -0.569183863 -0.204957214     -0.1853466137 -1.788800e-01 R187964
## 310 -1.561911254 -0.621685378     -0.3397460945 -6.004798e-01 R187545
## 311  2.068387347  1.010793240      1.2293402424 -1.717461e-01 R187546
## 312 -0.209558604  0.793105370     -0.1415687127 -8.610953e-01 R187239
## 313  0.157311000  0.006571449      0.5823362771 -4.315967e-01 R187940
## 314  0.849398610 -0.118518345      0.1092330270  8.586839e-01 R187590
## 315  0.729511586  0.242642367      0.6177094439 -1.308402e-01 R187524
## 316 -0.509843603 -0.119850923     -0.6206857675  2.306931e-01 R187521
## 317  0.598770815  0.609554100      0.8260829262 -8.368662e-01 R187931
## 318  0.313317448  0.040298744     -0.6479762732  9.209950e-01 R187577
## 319  1.572605777  1.211541972     -0.0469942069  4.080580e-01 R188000
## 320  1.670083464  0.496768873      0.6753737157  4.979409e-01 R187292
## 321  0.225194895 -0.077789199      0.1100035748  1.929805e-01 R187516
## 322 -1.197767236  0.487128643     -0.2262148874 -1.458681e+00 R187999
## 323  1.132609781 -0.070032377      0.5911670099  6.114751e-01 R187166
## 324 -1.828841949 -0.586214959     -0.2566015242 -9.860255e-01 R187579
## 325  0.914618888  0.343150256     -0.5486846241  1.120153e+00 R187548
## 326  0.128414218  0.430896974      0.0641166434 -3.665994e-01 R187961
## 327  1.043493249 -0.124913243     -0.2223895132  1.390796e+00 R187155
## 328  0.206650262 -0.182867012      0.2172279676  1.722893e-01 R187594
## 329  0.578742500  0.679561322      0.7697028054 -8.705216e-01 R187948
## 330 -0.681872102  0.149948422     -0.4860586210 -3.457619e-01 R187588
## 331 -0.305038436 -0.163017161     -0.0665506946 -7.547058e-02 R187557
## 332  0.887284382 -0.264026422      0.5179401614  6.333706e-01 R187559
## 333  0.872186796  0.128929314     -0.3694369421  1.112694e+00 R187531
## 334 -0.301050316 -0.113329566      0.2130797673 -4.008005e-01 R187963
## 335 -1.304966409 -0.497971284     -0.3716266513 -4.353685e-01 R187592
## 336  0.334088358 -0.207915112      0.0485953394  4.934081e-01 R187575
## 337  0.641222273  0.336575748     -0.1578358156  4.624823e-01    Fem5
## 338 -0.018941183 -0.692326750      0.9233292259 -2.499437e-01 R186912
## 339 -0.781238905  0.074285891     -0.3132066418 -5.423182e-01 R187930
## 340 -1.015837757  0.317042901     -0.5780155224 -7.548651e-01 R187513
## 341  0.540009283  0.006140183      0.1072109833  4.266581e-01 R187000
## 342 -0.300150572 -0.435732821      0.2254438434 -8.986159e-02 R187598
## 343  0.394549326  0.256576191      0.3204356724 -1.824625e-01 R187918
## 344  0.409622813  0.108474565      0.2820076156  1.914063e-02 R187914
## 345  1.430666246  0.601863348      0.0503159518  7.784869e-01 R187957
## 346  0.117347258 -0.111108353      0.4560975644 -2.276420e-01 R187955
## 347  0.168277497  0.493320742     -0.4726758719  1.476326e-01 R187552
## 348  0.893245940  0.310917487      0.1066508524  4.756776e-01 R187535
## 349 -0.725915256 -0.041896884     -0.2866655166 -3.973529e-01 R187527
## 350  0.410726707  0.043832264      0.4302394334 -6.334499e-02 R187595
## 351 -0.946959701 -0.028121739     -0.7358062033 -1.830318e-01 R187925
## 352  0.657997299  0.480818170      0.2984368305 -1.212577e-01 R187001
## 353 -0.037286207 -0.190720306     -0.5735198159  7.269539e-01 R186918
## 354 -1.306496568  0.276996765     -0.4993565477 -1.084137e+00 R187637
## 355 -0.105335012 -0.164335670      0.2649099669 -2.059093e-01 K983388
## 356  1.044851614  0.049599994      0.2139443351  7.813073e-01   Fem20
## 357 -0.840063473  0.105776164     -0.2365440378 -7.092956e-01 R187086
## 358  0.347610659  0.683871820     -0.2252768614 -1.109843e-01 R187009
## 359 -1.026518024  0.054115544     -0.3584837537 -7.221498e-01 R187926
## 360  0.525215946  0.054115544     -0.3584837537  8.295842e-01 R187926
## 361 -0.246363677  0.054115544     -0.3584837537  5.800453e-02 R187926
## 362 -0.753857196  0.054115544     -0.3584837537 -4.494890e-01 R187926
## 363 -0.679253068  0.054115544     -0.3584837537 -3.748849e-01 R187926
## 364 -1.106445231  0.054115544     -0.3584837537 -8.020770e-01 R187926
## 365 -1.096929471  0.151482272     -0.3683572296 -8.800545e-01 R187951
## 366 -0.346415561  0.054115544     -0.3584837537 -4.204735e-02 R187926
## 367 -0.409260604  0.151482272     -0.3683572296 -1.923856e-01 R187951
## 368 -0.362545297  0.054115544     -0.3584837537 -5.817709e-02 R187926
## 369 -0.390261610 -0.336150949      0.0108027388 -6.491340e-02 R187517
## 370  1.316909112  0.441362485      0.2164029348  6.591437e-01 R187399
## 371  2.133181764  0.113219894      0.5823362771  1.437626e+00 R187927
## 372  0.307730566  0.151482272     -0.3683572296  5.246055e-01 R187951
## 373 -0.015965790  0.054115544     -0.3584837537  2.884024e-01 R187926
## 374 -1.636220815 -0.502686995     -1.1106481578 -2.288566e-02 R187562
## 375 -0.255647883 -0.633770546      0.1128523135  2.652703e-01 R186910
## 376  1.314101248 -0.435732821      0.0872542252  1.662580e+00 R187598
## 377 -0.489249615 -0.106400866      0.7715733294 -1.154422e+00 R187518
## 378 -0.788226470 -0.336150949      0.0108027388 -4.628783e-01 R187517
## 379  1.016814430  0.441362485      0.2164029348  3.590490e-01 R187399
## 380  1.766101541  0.793105370      0.9221902441  5.080593e-02 R187239
## 381  1.389575735  0.113219894      0.5823362771  6.940196e-01 R187927
## 382 -0.436838071  0.151482272     -0.3683572296 -2.199631e-01 R187951
## 383 -0.818772952  0.609554100     -0.3132066418 -1.115120e+00 R187931
## 384 -0.562378601  0.054115544     -0.3584837537 -2.580104e-01 R187926
## 385 -0.984835896  0.480818170     -0.2252768614 -1.240377e+00 R187001
## 386 -2.044337255 -0.502686995     -1.1106481578 -4.310021e-01 R187562
## 387 -0.295130778 -0.633770546      0.1128523135  2.257875e-01 R186910
## 388 -1.158720774 -0.435732821      0.0872542252 -8.102422e-01 R187598
## 389  0.283921622 -0.106400866      0.7715733294 -3.812508e-01 R187518
## 390 -0.141761497  0.493320742     -0.4418149384 -1.932673e-01 R187552
## 391  0.386650893 -0.336150949      0.0108027388  7.119991e-01 R187517
## 392 -0.314781426  0.485597021      0.5047938274 -1.305172e+00 R187398
## 393 -0.306235603  0.747380550      0.2719944416 -1.325611e+00 R187936
## 394  0.473170315  0.441362485      0.2164029348 -1.845951e-01 R187399
## 395  1.544413436  0.793105370      0.9221902441 -1.708822e-01 R187239
## 396  0.241412626  0.113219894      0.5823362771 -4.541435e-01 R187927
## 397 -0.123557033 -0.321879131      0.1092330270  8.908907e-02 R187932
## 398  0.841169168 -0.077789199      0.6177094439  3.012489e-01 R187516
## 399  0.094985723  0.679561322     -0.0469942069 -5.375814e-01 R187948
## 400  0.463005163  0.430896974     -0.2262148874  2.583231e-01 R187961
## 401  1.474641259 -0.070032377     -0.2566015242  1.801275e+00 R187166
## 402  0.004605527  0.149948422     -0.2223895132  7.704662e-02 R187588
## 403 -1.389516066  0.151482272     -0.3683572296 -1.172641e+00 R187951
## 404 -2.030981545 -0.264026422     -0.0665506946 -1.700404e+00 R187559
## 405 -0.775459472 -0.444761774     -0.3694369421  3.873924e-02 R187030
## 406 -0.520894623 -0.497971284      0.0485953394 -7.151868e-02 R187592
## 407  0.675088883  0.609901882     -0.1578358156  2.230228e-01 R187941
## 408  0.904239614  0.609554100     -0.3132066418  6.078922e-01 R187931
## 409 -0.242393346 -0.357311121      0.1072109833  7.706792e-03 R186902
## 410 -0.479954112  0.108474565      0.4560975644 -1.044526e+00 R187914
## 411 -1.308087318  0.054115544     -0.3584837537 -1.003719e+00 R187926
## 412  0.140814498  0.105776164      0.1066508524 -7.161252e-02 R187086
## 413 -1.165362844 -0.593703220      0.4302394334 -1.001899e+00 R187553
## 414 -0.760649535 -0.311299829     -0.5735198159  1.241701e-01 R186917
## 415  1.388387442  0.049599994      0.2649099669  1.073877e+00   Fem20
## 416  0.558151942  0.480818170     -0.2252768614  3.026106e-01 R187001
## 417  0.990452099  0.212760356      0.5204078520  2.572839e-01 R187566
## 418 -1.807926898 -0.248288194     -0.4565467688 -1.103092e+00 R187539
## 419 -2.710022364 -0.502686995     -1.1106481578 -1.096687e+00 R187562
## 420  1.518335130 -0.335821824      0.4883147794  1.365842e+00 R187563
## 421 -0.515021926 -0.633770546      0.1128523135  5.896307e-03 R186910
## 422 -0.576136750 -0.435732821      0.0872542252 -2.276582e-01 R187598
## 423 -0.434445561 -0.041896884      0.0138170032 -4.063657e-01 R187527
## 424  0.224083828  0.335295749      0.6184896802 -7.297016e-01 P322402
## 425 -1.118271428  0.287343801     -1.3161538186 -8.946141e-02 R187569
## 426 -0.524854053 -0.473220015     -0.3385350669  2.869010e-01 R187568
## 427  0.222961888 -0.106400866      0.7715733294 -4.422106e-01 R187518
## 428  0.565167525  0.493320742     -0.4418149384  5.136617e-01 R187552
## 429  0.585730727  0.043832264      0.0428102208  4.990882e-01 R187595
## 430 -1.547080775 -0.182867012      0.2203211850 -1.584535e+00 R187594
## 431 -0.795649388 -0.336150949      0.0108027388 -4.703012e-01 R187517
## 432 -0.207675780  0.128929314     -0.2393953208 -9.720977e-02 R187531
## 433  1.706270167  0.485597021      0.5047938274  7.158793e-01 R187398
## 434  0.148535252  0.747380550      0.2719944416 -8.708397e-01 R187936
## 435  0.388562691 -0.222929754     -0.8433136269  1.454806e+00 R186908
## 436 -0.577403032  0.006140183      0.0996140063 -6.831572e-01 R187000
## 437 -0.805579313 -0.692326750     -0.2089918958  9.573933e-02 R186912
## 438 -2.773812063 -0.541012302     -0.7103398880 -1.522460e+00    Fem3
## 439  2.391005471  0.793105370      0.9221902441  6.757099e-01 R187239
## 440  1.123932737 -0.621685378      1.2293402424  5.162779e-01 R187545
## 441  0.107061299  0.630323203     -0.6228078994  9.954599e-02 R187540
## 442  0.777818757 -0.321879131      0.1092330270  9.904649e-01 R187932
## 443  1.292870246 -0.077789199      0.6177094439  7.529500e-01 R187516
## 444  1.417402974  0.679561322     -0.0469942069  7.848359e-01 R187948
## 445  1.330374878  0.242642367      0.1100035748  9.777289e-01 R187524
## 446  0.088609033  0.430896974     -0.2262148874 -1.160731e-01 R187961
## 447 -0.547387008 -0.586214959      0.5911670099 -5.523391e-01 R187579
## 448 -0.041986456 -0.070032377     -0.2566015242  2.846474e-01 R187166
## 449 -0.376689144  0.395765728      0.1498231459 -9.222780e-01 R186907
## 450  0.042837866  0.149948422     -0.2223895132  1.152790e-01 R187588
## 451  1.411200071  0.417816418      0.2172279676  7.761557e-01 R187945
## 452  0.511414923  0.151482272     -0.3683572296  7.282899e-01 R187951
## 453 -0.182601830 -0.124913243     -0.4860586210  4.283700e-01 R187155
## 454 -0.262412580 -0.264026422     -0.0665506946  6.816454e-02 R187559
## 455  0.370120830 -0.163017161      0.5179401614  1.519783e-02 R187557
## 456 -0.977325688 -0.444761774     -0.3694369421 -1.631270e-01 R187030
## 457  0.908803539 -0.204957214      0.2130797673  9.006810e-01 R187964
## 458  0.008205534 -0.497971284      0.0485953394  4.575815e-01 R187592
## 459 -1.390455657  0.609901882     -0.1578358156 -1.842522e+00 R187941
## 460  0.372741787  0.609554100     -0.3132066418  7.639433e-02 R187931
## 461 -0.809826126 -0.357311121      0.1072109833 -5.597260e-01 R186902
## 462  2.192023059 -0.035550637      0.2254438434  2.002130e+00 R187916
## 463  0.568263568 -0.111108353      0.2820076156  3.973643e-01 R187955
## 464  0.489729535  0.108474565      0.4560975644 -7.484259e-02 R187914
## 465 -0.901536904  0.054115544     -0.3584837537 -5.971687e-01 R187926
## 466  0.312959815  0.105776164      0.1066508524  1.005328e-01 R187086
## 467 -1.324823945 -0.295128879     -0.2866655166 -7.430295e-01 R187515
## 468  0.925966881 -0.593703220      0.4302394334  1.089431e+00 R187553
## 469 -1.026430277 -0.311299829     -0.5735198159 -1.416106e-01 R186917
## 470  0.046980413  0.049599994      0.2649099669 -2.675295e-01   Fem20
## 471  0.534409287 -0.164335670      0.2139443351  4.848006e-01 K983388
## 472 -0.042783917  0.310917487     -0.2365440378 -1.171574e-01 R187535
## 473 -0.042204992  0.480818170     -0.2252768614 -2.977463e-01 R187001
## 474  1.301471802  0.212760356      0.5204078520  5.683036e-01 R187566
## 475 -0.790866169 -0.248288194     -0.4565467688 -8.603121e-02 R187539
## 476 -0.983572713 -0.003460548     -0.5933475065 -3.867647e-01 R187343
## 477 -1.200624453 -0.502686995     -1.1106481578  4.127107e-01 R187562
## 478  0.650795529 -0.335821824      0.4883147794  4.983026e-01 R187563
## 479  0.563588538 -0.633770546      0.1128523135  1.084507e+00 R186910
## 480 -1.006709295 -0.435732821      0.0872542252 -6.582307e-01 R187598
## 481 -0.012263579 -0.041896884      0.0138170032  1.581630e-02 R187527
## 482  1.721342800  0.335295749      0.6184896802  7.675574e-01 P322402
## 483 -1.489828748  0.287343801     -1.3161538186 -4.610187e-01 R187569
## 484 -2.405564588 -0.473220015     -0.3385350669 -1.593810e+00 R187568
## 485  0.655850924 -0.106400866      0.7715733294 -9.321539e-03 R187518
## 486  0.755591677  0.493320742     -0.4418149384  7.040859e-01 R187552
## 487 -0.195919303 -0.182867012      0.2203211850 -2.333735e-01 R187594
## 488  0.571839479 -0.336150949      0.0108027388  8.971877e-01 R187517
## 489  1.244269093 -0.510422765     -0.0001817107  1.754874e+00 R187400
## 490 -1.489607934  0.128929314     -0.2393953208 -1.379142e+00 R187531
## 491  0.713040435  0.485597021      0.5047938274 -2.773504e-01 R187398
## 492  1.114906843  0.747380550      0.2719944416  9.553185e-02 R187936
## 493  1.267696129  0.441362485      0.2164029348  6.099307e-01 R187399
## 494  0.445591885 -0.119850923      0.9336440587 -3.682013e-01 R187521
## 495 -2.001933796 -0.222929754     -0.8433136269 -9.356904e-01 R186908
## 496  0.327744908 -0.684522175      0.5038999872  5.083671e-01 R187824
## 497 -0.096529690  0.006140183      0.0996140063 -2.022839e-01 R187000
## 498  1.105991774 -0.139669927      0.0608813533  1.184780e+00 R187947
## 499 -0.361065243 -0.692326750     -0.2089918958  5.402534e-01 R186912
## 500 -1.999369841 -0.541012302     -0.7103398880 -7.480177e-01    Fem3
## 501 -0.480834717  0.169285742     -0.2303746227 -4.197458e-01 R187958
## 502  0.718885097 -0.190720306      0.9023067998  7.298604e-03 R186918
## 503 -0.912412112 -0.118518345     -0.4489854877 -3.449083e-01 R187590
## 504  0.705495539  0.793105370      0.9221902441 -1.009800e+00 R187239
## 505 -0.063927448  0.343150256      0.1852000939 -5.922778e-01 R187548
## 506  0.907076687  1.010793240     -0.3397460945  2.360295e-01 R187546
## 507  0.254489805 -0.621685378      1.2293402424 -3.531651e-01 R187545
## 508 -0.116442973  0.630323203     -0.6228078994 -1.239583e-01 R187540
## 509 -0.326485885  0.113219894      0.5823362771 -1.022042e+00 R187927
## 510 -0.254942362 -0.321879131      0.1092330270 -4.229626e-02 R187932
## 511  1.536800064 -0.077789199      0.6177094439  9.968798e-01 R187516
## 512 -0.740279796  0.679561322     -0.0469942069 -1.372847e+00 R187948
## 513  0.249975778  0.242642367      0.1100035748 -1.026702e-01 R187524
## 514 -0.038441326  0.430896974     -0.2262148874 -2.431234e-01 R187961
## 515  0.056419573 -0.586214959      0.5911670099  5.146752e-02 R187579
## 516 -0.182566399 -0.070032377     -0.2566015242  1.440675e-01 R187166
## 517  0.916967019  0.395765728      0.1498231459  3.713781e-01 R186907
## 518 -1.626688188 -0.421845891     -0.5486846241 -6.561577e-01 R187582
## 519 -0.845386907  0.487128643      0.0641166434 -1.396632e+00 R187999
## 520 -0.418353868  0.149948422     -0.2223895132 -3.459128e-01 R187588
## 521  0.283554007  0.417816418      0.2172279676 -3.514904e-01 R187945
## 522 -0.452355283  0.151482272     -0.3683572296 -2.354803e-01 R187951
## 523  1.008324757 -0.124913243     -0.4860586210  1.619297e+00 R187155
## 524  1.140952891 -0.264026422     -0.0665506946  1.471530e+00 R187559
## 525  1.361638193 -0.163017161      0.5179401614  1.006715e+00 R187557
## 526 -1.599446615 -0.444761774     -0.3694369421 -7.852479e-01 R187030
## 527  0.605138887 -0.204957214      0.2130797673  5.970163e-01 R187964
## 528  0.065412624 -0.207915112     -0.3716266513  6.449544e-01 R187575
## 529  0.416337755 -0.497971284      0.0485953394  8.657137e-01 R187592
## 530  1.297824385  0.609901882     -0.1578358156  8.457583e-01 R187941
## 531 -0.721717292  0.609554100     -0.3132066418 -1.018065e+00 R187931
## 532 -0.735634049 -0.106850748     -0.5780155224 -5.076778e-02 R187573
## 533 -0.007916950 -0.035550637      0.2254438434 -1.978102e-01 R187916
## 534 -0.253658949 -0.028121739      0.3204356724 -5.459729e-01 R187925
## 535  0.785349839 -0.111108353      0.2820076156  6.144506e-01 R187955
## 536  1.305590601  0.108474565      0.4560975644  7.410185e-01 R187914
## 537 -0.248361773  0.054115544     -0.3584837537  5.600644e-02 R187926
## 538 -0.295570451  0.105776164      0.1066508524 -5.079975e-01 R187086
## 539 -1.003994219 -0.295128879     -0.2866655166 -4.221998e-01 R187515
## 540 -0.951544252 -0.593703220      0.4302394334 -7.880805e-01 R187553
## 541 -2.256861417 -0.311299829     -0.5735198159 -1.372042e+00 R186917
## 542  0.290387874  0.601863348     -0.4993565477  1.878811e-01 R187957
## 543 -0.581924668  0.049599994      0.2649099669 -8.964346e-01   Fem20
## 544  1.443348258 -0.164335670      0.2139443351  1.393740e+00 K983388
## 545 -1.386873354 -0.396178271      0.4865132854 -1.477208e+00 R186903
## 546 -0.613515269  0.310917487     -0.2365440378 -6.878887e-01 R187535
## 547  0.946589251  0.004618578      0.4079750381  5.339956e-01 R186911
## 548  0.066220904  0.480818170     -0.2252768614 -1.893204e-01 R187001
## 549  1.245966947  0.212760356      0.5204078520  5.127987e-01 R187566
## 550 -0.394981338 -0.248288194     -0.4565467688  3.098536e-01 R187539
## 551 -0.771916996 -0.003460548     -0.5933475065 -1.751089e-01 R187343
## 552 -1.498703753 -0.502686995     -1.1106481578  1.146314e-01 R187562
## 553 -0.569111497 -0.335821824      0.4883147794 -7.216045e-01 R187563
## 554 -0.433572300 -0.633770546      0.1128523135  8.734593e-02 R186910
## 555 -2.317637457 -0.514008086     -0.3388679569 -1.464761e+00 R187528
## 556  0.827843990 -0.435732821      0.0872542252  1.176323e+00 R187598
## 557  0.288906975 -0.041896884      0.0138170032  3.169869e-01 R187527
## 558 -1.377512239 -0.435569352     -0.0686755334 -8.732674e-01    Fem2
## 559  1.707395049  0.335295749      0.6184896802  7.536096e-01 P322402
## 560 -1.388935187  0.287343801     -1.3161538186 -3.601252e-01 R187569
## 561  0.004268109 -0.473220015     -0.3385350669  8.160232e-01 R187568
## 562  1.486119062 -0.106400866      0.7715733294  8.209466e-01 R187518
## 563  0.927704608 -0.083986784      0.5478883067  4.638031e-01 R187537
## 564  0.238868873  0.493320742     -0.4418149384  1.873631e-01 R187552
## 565  0.665200386  0.043832264      0.0428102208  5.785579e-01 R187595
## 566 -0.796144848 -0.182867012      0.2203211850 -8.335990e-01 R187594
## 567 -0.346076099 -0.336150949      0.0108027388 -2.072789e-02 R187517
## 568 -1.002493905 -0.510422765     -0.0001817107 -4.918894e-01 R187400
## 569  0.278472011  0.128929314     -0.2393953208  3.889380e-01 R187531
## 570 -0.785244705  0.202824652      0.0997371078 -1.087806e+00 R046108
## 571  0.486114461  0.747380550      0.2719944416 -5.332605e-01 R187936
## 572 -0.525314983  0.441362485      0.2164029348 -1.183080e+00 R187399
## 573  1.172974746 -0.119850923      0.9336440587  3.591816e-01 R187521
## 574 -1.122947133  0.006571449      0.1112327346 -1.240751e+00 R187940
## 575 -0.176657495 -0.222929754     -0.8433136269  8.895859e-01 R186908
## 576 -0.399998213 -0.684522175      0.5038999872 -2.193760e-01 R187824
## 577  0.127648765 -0.710002242     -0.1331679790  9.708190e-01 R187953
## 578  0.974891164 -0.139669927      0.0608813533  1.053680e+00 R187947
## 579 -0.580034187 -0.692326750     -0.2089918958  3.212845e-01 R186912
## 580 -1.236629741 -0.541012302     -0.7103398880  1.472245e-02    Fem3
## 581  1.353975234  0.169285742     -0.2303746227  1.415064e+00 R187958
## 582  0.689101838 -0.118518345     -0.4489854877  1.256606e+00 R187590
## 583  2.748689790  0.793105370      0.9221902441  1.033394e+00 R187239
## 584 -1.116418416  0.223412678     -1.5435048892  2.036738e-01 R187944
## 585  1.240660711  0.343150256      0.1852000939  7.123104e-01 R187548
## 586 -1.464482849 -0.113329566     -0.1853466137 -1.165807e+00 R187963
## 587 -0.433832739  1.010793240     -0.3397460945 -1.104880e+00 R187546
## 588  0.836463250 -0.621685378      1.2293402424  2.288084e-01 R187545
## 589  0.486436149 -0.075916280     -0.1415687127  7.039211e-01 R187547
## 590 -0.571517065  0.630323203     -0.6228078994 -5.790324e-01 R187540
## 591  2.245582808  0.113219894      0.5823362771  1.550027e+00 R187927
## 592  0.208238607 -0.321879131      0.1092330270  4.208847e-01 R187932
## 593  1.164725765 -0.077789199      0.6177094439  6.248055e-01 R187516
## 594  0.409457273 -0.137640164     -0.6206857675  1.167783e+00 R187523
## 595  0.608641130  0.679561322     -0.0469942069 -2.392599e-02 R187948
## 596  2.364037820  0.040298744      0.6753737157  1.648365e+00 R187577
## 597 -1.188691753  0.242642367      0.1100035748 -1.541338e+00 R187524
## 598 -1.219713856  0.430896974     -0.2262148874 -1.424396e+00 R187961
## 599 -0.974241067 -0.586214959      0.5911670099 -9.791931e-01 R187579
## 600 -0.536174420 -0.070032377     -0.2566015242 -2.095405e-01 R187166
## 601 -0.228234585 -0.421845891     -0.5486846241  7.422959e-01 R187582
## 602  0.990852274  0.487128643      0.0641166434  4.396070e-01 R187999
## 603 -0.946154905  0.149948422     -0.2223895132 -8.737138e-01 R187588
## 604  0.867217898  0.417816418      0.2172279676  2.321735e-01 R187945
## 605  1.319079225  1.211541972      0.7697028054 -6.621656e-01 R188000
## 606  0.613571112  0.151482272     -0.3683572296  8.304461e-01 R187951
## 607 -0.088528958 -0.124913243     -0.4860586210  5.224429e-01 R187155
## 608 -0.637258426 -0.264026422     -0.0665506946 -3.066813e-01 R187559
## 609  0.492623842 -0.163017161      0.5179401614  1.377008e-01 R187557
## 610  0.035300450 -0.444761774     -0.3694369421  8.494992e-01 R187030
## 611  0.335256568 -0.204957214      0.2130797673  3.271340e-01 R187964
## 612 -0.738032738 -0.207915112     -0.3716266513 -1.584910e-01 R187575
## 613 -1.404268007 -0.497971284      0.0485953394 -9.548921e-01 R187592
## 614  0.122716186  0.609901882     -0.1578358156 -3.293499e-01 R187941
## 615  1.456491703 -0.175968644      0.9233292259  7.091311e-01 R187902
## 616  0.819504258  0.609554100     -0.3132066418  5.231568e-01 R187931
## 617 -0.618848243 -0.106850748     -0.5780155224  6.601803e-02 R187573
## 618 -0.344666749 -0.357311121      0.1072109833 -9.456661e-02 R186902
## 619  0.148454809 -0.035550637      0.2254438434 -4.143840e-02 R187916
## 620 -0.158760511 -0.028121739      0.3204356724 -4.510744e-01 R187925
## 621  0.960967954 -0.111108353      0.2820076156  7.900687e-01 R187955
## 622  0.439927072  0.108474565      0.4560975644 -1.246451e-01 R187914
## 623  0.980171288  0.105776164      0.1066508524  7.677443e-01 R187086
## 624 -0.199874747 -0.295128879     -0.2866655166  3.819196e-01 R187515
## 625  0.019425351 -0.593703220      0.4302394334  1.828891e-01 R187553
## 626 -0.528308204  0.227745624      0.0171228734 -7.731767e-01 R187920
## 627 -0.678580586  0.017310351      0.7535746524 -1.449466e+00 R187512
## 628 -1.068587163 -0.311299829     -0.5735198159 -1.837675e-01 R186917
## 629  0.295261249  0.601863348     -0.4993565477  1.927544e-01 R187957
## 630  0.427969786  0.049599994      0.2649099669  1.134598e-01   Fem20
## 631 -0.451665657 -0.164335670      0.2139443351 -5.012743e-01 K983388
## 632  0.672464544  0.310917487     -0.2365440378  5.980911e-01 R187535
## 633  1.185499926  0.004618578      0.4079750381  7.729063e-01 R186911
## 634  0.355425481  0.480818170     -0.2252768614  9.988417e-02 R187001
## 635  1.499826164  0.212760356      0.5204078520  7.666580e-01 R187566
## 636 -0.666011323 -0.248288194     -0.4565467688  3.882364e-02 R187539
## 637 -0.126368322 -0.003460548     -0.5933475065  4.704397e-01 R187343
## 638 -1.727781371 -0.502686995     -1.1106481578 -1.144462e-01 R187562
## 639  0.082104336 -0.335821824      0.4883147794 -7.038862e-02 R187563
## 640 -0.115584595 -0.633770546      0.1128523135  4.053336e-01 R186910
## 641 -0.234642712 -0.514008086     -0.3388679569  6.182333e-01 R187528
## 642 -0.206189296 -0.435732821      0.0872542252  1.422893e-01 R187598
## 643 -0.126021981 -0.041896884      0.0138170032 -9.794210e-02 R187527
## 644 -0.254505920 -0.435569352     -0.0686755334  2.497390e-01    Fem2
## 645  1.041122515  0.335295749      0.6184896802  8.733709e-02 P322402
## 646 -0.825123392  0.287343801     -1.3161538186  2.036866e-01 R187569
## 647 -1.914791053 -0.473220015     -0.3385350669 -1.103036e+00 R187568
## 648  1.081935700 -0.106400866      0.7715733294  4.167632e-01 R187518
## 649 -0.191754746 -0.083986784      0.5478883067 -6.556563e-01 R187537
## 650  1.237191064  0.493320742     -0.4418149384  1.185685e+00 R187552
## 651  0.960579967  0.043832264      0.0428102208  8.739375e-01 R187595
## 652 -1.715688541 -0.182867012      0.2203211850 -1.753143e+00 R187594
## 653  0.390866049 -0.336150949      0.0108027388  7.162143e-01 R187517
## 654 -1.334749985 -0.510422765     -0.0001817107 -8.241455e-01 R187400
## 655 -0.246687503  0.128929314     -0.2393953208 -1.362215e-01 R187531
## 656  2.056571450  0.485597021      0.5047938274  1.066181e+00 R187398
## 657 -0.167914769  0.202824652      0.0997371078 -4.704765e-01 R046108
## 658  1.376961091  0.747380550      0.2719944416  3.575861e-01 R187936
## 659  0.123441100  0.441362485      0.2164029348 -5.343243e-01 R187399
## 660  0.041020685 -0.119850923      0.9336440587 -7.727725e-01 R187521
## 661  0.889224633  0.006571449      0.1112327346  7.714204e-01 R187940
## 662 -1.134622336  0.537510497     -1.1116736132 -5.604592e-01 R187937
## 663  0.003318098 -0.222929754     -0.8433136269  1.069561e+00 R186908
## 664 -1.023148270 -0.684522175      0.5038999872 -8.425261e-01 R187824
## 665  0.054512203  0.006140183      0.0996140063 -5.124199e-02 R187000
## 666 -1.063515470 -0.710002242     -0.1331679790 -2.203452e-01 R187953
## 667  1.006522674 -0.139669927      0.0608813533  1.085311e+00 R187947
## 668 -1.217095577 -0.692326750     -0.2089918958 -3.157769e-01 R186912
## 669 -1.012018877 -0.541012302     -0.7103398880  2.393333e-01    Fem3
## 670  0.923278279  0.169285742     -0.2303746227  9.843672e-01 R187958
## 671  0.108551136 -0.118518345     -0.4489854877  6.760550e-01 R187590
## 672  1.220545358  0.793105370      0.9221902441 -4.947503e-01 R187239
## 673 -2.651783641  0.223412678     -1.5435048892 -1.331691e+00 R187944
## 674  2.292904065  0.343150256      0.1852000939  1.764554e+00 R187548
## 675  0.184302006  1.010793240     -0.3397460945 -4.867451e-01 R187546
## 676  0.796608992 -0.621685378      1.2293402424  1.889541e-01 R187545
## 677  0.085374203 -0.075916280     -0.1415687127  3.028592e-01 R187547
## 678  0.708735792  0.630323203     -0.6228078994  7.012205e-01 R187540
## 679  2.289472029  0.113219894      0.5823362771  1.593916e+00 R187927
## 680  0.659863256 -0.321879131      0.1092330270  8.725094e-01 R187932
## 681  1.068405749 -0.077789199      0.6177094439  5.284855e-01 R187516
## 682 -0.095163381 -0.137640164     -0.6206857675  6.631626e-01 R187523
## 683  1.699267003  0.074285891      0.8260829262  7.988982e-01 R187930
## 684 -0.590868446  0.496768873     -0.6479762732 -4.396610e-01 R187292
## 685  1.962454853  0.679561322     -0.0469942069  1.329888e+00 R187948
## 686 -0.184175436  0.040298744      0.6753737157 -8.998479e-01 R187577
## 687  0.669225307  0.242642367      0.1100035748  3.165794e-01 R187524
## 688 -1.257420384  0.430896974     -0.2262148874 -1.462102e+00 R187961
## 689  1.054844345 -0.586214959      0.5911670099  1.049892e+00 R187579
## 690 -0.961989826 -0.070032377     -0.2566015242 -6.353559e-01 R187166
## 691 -1.599285602 -0.421845891     -0.5486846241 -6.287551e-01 R187582
## 692  0.774735398  0.487128643      0.0641166434  2.234901e-01 R187999
## 693  0.314122836  0.149948422     -0.2223895132  3.865639e-01 R187588
## 694  0.552406381  0.156030971     -0.5055094021  9.018848e-01 R187541
## 695  1.061631561  0.417816418      0.2172279676  4.265872e-01 R187945
## 696  2.583311692  1.211541972      0.7697028054  6.020669e-01 R188000
## 697  0.801082636  0.151482272     -0.3683572296  1.017958e+00 R187951
## 698  0.112713801 -0.264026422     -0.0665506946  4.432909e-01 R187559
## 699  1.286659149 -0.163017161      0.5179401614  9.317361e-01 R187557
## 700 -0.452537560 -0.444761774     -0.3694369421  3.616612e-01 R187030
## 701  0.555245489 -0.204957214      0.2130797673  5.471229e-01 R187964
## 702  0.261528200 -0.207915112     -0.3716266513  8.410700e-01 R187575
## 703 -0.776023404 -0.497971284      0.0485953394 -3.266475e-01 R187592
## 704  0.808969097  0.609901882     -0.1578358156  3.569030e-01 R187941
## 705  0.702997892 -0.175968644      0.9233292259 -4.436269e-02 R187902
## 706 -0.586212154  0.609554100     -0.3132066418 -8.825596e-01 R187931
## 707 -1.294409544 -0.106850748     -0.5780155224 -6.095433e-01 R187573
## 708 -1.322335751 -0.357311121      0.1072109833 -1.072236e+00 R186902
## 709 -0.642034779  0.110597803      0.6100616829 -1.362694e+00 R186901
## 710  1.558147881 -0.035550637      0.2254438434  1.368255e+00 R187916
## 711  0.373021504 -0.028121739      0.3204356724  8.070757e-02 R187925
## 712  0.865090577 -0.111108353      0.2820076156  6.941913e-01 R187955
## 713  0.737221501  0.108474565      0.4560975644  1.726494e-01 R187914
## 714 -1.549476764 -0.393278029     -0.4726758719 -6.835229e-01 R187571
## 715  0.069106953  0.054115544     -0.3584837537  3.734752e-01 R187926
## 716  0.989778663  0.105776164      0.1066508524  7.773516e-01 R187086
## 717 -0.767865082 -0.295128879     -0.2866655166 -1.860707e-01 R187515
## 718  0.854380254 -0.593703220      0.4302394334  1.017844e+00 R187553
## 719 -0.211823824  0.256576191     -0.7358062033  2.674062e-01 R187918
## 720  1.222875461  0.227745624      0.0171228734  9.780070e-01 R187920
## 721  0.981266947  0.017310351      0.7535746524  2.103819e-01 R187512
## 722  0.278125725 -0.311299829     -0.5735198159  1.162945e+00 R186917
## 723  0.207799044  0.601863348     -0.4993565477  1.052922e-01 R187957
## 724  1.081001140  0.049599994      0.2649099669  7.664912e-01   Fem20
## 725  0.354395289 -0.164335670      0.2139443351  3.047866e-01 K983388
## 726 -1.018200195 -0.396178271      0.4865132854 -1.108535e+00 R186903
## 727  0.531237240  0.310917487     -0.2365440378  4.568638e-01 R187535
## 728 -0.040995376  0.004618578      0.4079750381 -4.535890e-01 R186911
## 729  0.987825497  0.480818170     -0.2252768614  7.322842e-01 R187001
## 730  0.223884517  0.212760356      0.5204078520 -5.092837e-01 R187566
## 731 -0.962768582 -0.248288194     -0.4565467688 -2.579336e-01 R187539
## 732  0.351084084 -0.003460548     -0.5933475065  9.478921e-01 R187343
## 733 -1.848957190 -0.502686995     -1.1106481578 -2.356220e-01 R187562
## 734  0.418017457 -0.335821824      0.4883147794  2.655245e-01 R187563
## 735 -1.967385428 -0.633770546      0.1128523135 -1.446467e+00 R186910
## 736 -0.570971864 -0.514008086     -0.3388679569  2.819042e-01 R187528
## 737 -0.709113947 -0.435732821      0.0872542252 -3.606354e-01 R187598
## 738  0.680261359 -0.041896884      0.0138170032  7.083412e-01 R187527
## 739 -0.339979493 -0.435569352     -0.0686755334  1.642654e-01    Fem2
## 740  1.372550036  0.335295749      0.6184896802  4.187646e-01 P322402
## 741 -1.354057460  0.287343801     -1.3161538186 -3.252474e-01 R187569
## 742 -0.254750096 -0.473220015     -0.3385350669  5.570050e-01 R187568
## 743 -0.508785720 -0.343755967      0.5371418026 -7.021716e-01 R186909
## 744  1.036721895 -0.106400866      0.7715733294  3.715494e-01 R187518
## 745  0.906698039 -0.083986784      0.5478883067  4.427965e-01 R187537
## 746 -0.093113753  0.493320742     -0.4418149384 -1.446196e-01 R187552
## 747 -0.178252176  0.043832264      0.0428102208 -2.648947e-01 R187595
## 748  0.428905618 -0.182867012      0.2203211850  3.914514e-01 R187594
## 749 -0.682614284 -0.336150949      0.0108027388 -3.572661e-01 R187517
## 750 -0.873653539 -0.510422765     -0.0001817107 -3.630491e-01 R187400
## 751  1.113916701  0.128929314     -0.2393953208  1.224383e+00 R187531
## 752 -0.717924727  0.202824652      0.0997371078 -1.020486e+00 R046108
## 753  0.891119200  0.441362485      0.2164029348  2.333538e-01 R187399
## 754 -0.059744183 -0.119850923      0.9336440587 -8.735373e-01 R187521
## 755 -1.377783141  0.006571449      0.1112327346 -1.495587e+00 R187940
## 756  0.286115196  0.537510497     -1.1116736132  8.602783e-01 R187937
## 757  0.073924642 -1.001253606      0.6643183101  4.108599e-01 R187942
## 758 -0.687046391 -0.222929754     -0.8433136269  3.791970e-01 R186908
## 759  0.099781327 -0.684522175      0.5038999872  2.804035e-01 R187824
## 760 -0.300525017  0.006140183      0.0996140063 -4.062792e-01 R187000
## 761 -1.677071742 -0.710002242     -0.1331679790 -8.339015e-01 R187953
## 762  0.460288615 -0.139669927      0.0608813533  5.390772e-01 R187947
## 763 -0.412079602 -0.541012302     -0.7103398880  8.392726e-01    Fem3
## 764  0.406352839  0.169285742     -0.2303746227  4.674417e-01 R187958
## 765 -2.406963879 -0.118518345     -0.4489854877 -1.839460e+00 R187590
## 766  1.372221433  0.793105370      0.9221902441 -3.430742e-01 R187239
## 767 -0.928579623  0.223412678     -1.5435048892  3.915126e-01 R187944
## 768 -0.741093705  0.343150256      0.1852000939 -1.269444e+00 R187548
## 769 -0.125771650 -0.113329566     -0.1853466137  1.729045e-01 R187963
## 770  0.847663022  1.010793240     -0.3397460945  1.766159e-01 R187546
## 771  0.729669128 -0.621685378      1.2293402424  1.220143e-01 R187545
## 772  0.718403312 -0.075916280     -0.1415687127  9.358883e-01 R187547
## 773 -0.043301550  0.630323203     -0.6228078994 -5.081685e-02 R187540
## 774  0.820964915  0.113219894      0.5823362771  1.254087e-01 R187927
## 775  0.748698308 -0.321879131      0.1092330270  9.613444e-01 R187932
## 776  0.848769642 -0.077789199      0.6177094439  3.088494e-01 R187516
## 777 -0.224418307 -0.137640164     -0.6206857675  5.339076e-01 R187523
## 778  1.673781466  0.074285891      0.8260829262  7.734126e-01 R187930
## 779 -1.265042024  0.496768873     -0.6479762732 -1.113835e+00 R187292
## 780  1.321344141  0.679561322     -0.0469942069  6.887770e-01 R187948
## 781  1.209062805  0.040298744      0.6753737157  4.933903e-01 R187577
## 782  0.697767063  0.242642367      0.1100035748  3.451211e-01 R187524
## 783 -0.231857773  0.430896974     -0.2262148874 -4.365399e-01 R187961
## 784  1.037620468 -0.586214959      0.5911670099  1.032668e+00 R187579
## 785 -1.075793383 -0.070032377     -0.2566015242 -7.491595e-01 R187166
## 786  0.127537448  0.395765728      0.1498231459 -4.180514e-01 R186907
## 787 -1.368594903 -0.421845891     -0.5486846241 -3.980644e-01 R187582
## 788 -0.104508653  0.487128643      0.0641166434 -6.557539e-01 R187999
## 789 -0.467163611  0.149948422     -0.2223895132 -3.947225e-01 R187588
## 790 -0.708603217  0.156030971     -0.5055094021 -3.591248e-01 R187541
## 791  1.123962479  0.417816418      0.2172279676  4.889181e-01 R187945
## 792  1.392198805  1.211541972      0.7697028054 -5.890460e-01 R188000
## 793  0.340873657  0.151482272     -0.3683572296  5.577486e-01 R187951
## 794  0.013842806 -0.124913243     -0.4860586210  6.248147e-01 R187155
## 795 -0.802290814 -0.264026422     -0.0665506946 -4.717137e-01 R187559
## 796  0.835105040 -0.163017161      0.5179401614  4.801820e-01 R187557
## 797 -2.638405833 -0.444761774     -0.3694369421 -1.824207e+00 R187030
## 798  0.184635277 -0.204957214      0.2130797673  1.765127e-01 R187964
## 799 -0.822007014 -0.207915112     -0.3716266513 -2.424653e-01 R187575
## 800 -0.774900171 -0.497971284      0.0485953394 -3.255242e-01 R187592
## 801  1.193767937  0.609901882     -0.1578358156  7.417019e-01 R187941
## 802  0.805638924 -0.175968644      0.9233292259  5.827834e-02 R187902
## 803 -0.610665139  0.609554100     -0.3132066418 -9.070126e-01 R187931
## 804 -0.394830477 -0.106850748     -0.5780155224  2.900358e-01 R187573
## 805  0.722788041 -0.357311121      0.1072109833  9.728882e-01 R186902
## 806  2.739481213  0.110597803      0.6100616829  2.018822e+00 R186901
## 807  0.322090092 -0.035550637      0.2254438434  1.321969e-01 R187916
## 808 -0.620741946 -0.028121739      0.3204356724 -9.130559e-01 R187925
## 809  0.094587664 -0.111108353      0.2820076156 -7.631160e-02 R187955
## 810  2.376661136  0.276996765      0.0503159518  2.049348e+00 R187637
## 811  0.408723597  0.108474565      0.4560975644 -1.558485e-01 R187914
## 812 -0.846986506 -0.393278029     -0.4726758719  1.896739e-02 R187571
## 813  0.479179767  0.054115544     -0.3584837537  7.835480e-01 R187926
## 814  0.751273204  0.105776164      0.1066508524  5.388462e-01 R187086
## 815 -0.451357984 -0.295128879     -0.2866655166  1.304364e-01 R187515
## 816 -0.351101128 -0.593703220      0.4302394334 -1.876373e-01 R187553
## 817  0.044187418  0.256576191     -0.7358062033  5.234174e-01 R187918
## 818  1.993569534  0.227745624      0.0171228734  1.748701e+00 R187920
## 819 -0.199037984  0.017310351      0.7535746524 -9.699230e-01 R187512
## 820  1.422466812  0.683871820      0.2984368305  4.401582e-01 R187009
## 821 -2.470897446 -0.311299829     -0.5735198159 -1.586078e+00 R186917
## 822 -1.194356618  0.601863348     -0.4993565477 -1.296863e+00 R187957
## 823  1.741036157  0.049599994      0.2649099669  1.426526e+00   Fem20
## 824  1.572945228 -0.164335670      0.2139443351  1.523337e+00 K983388
## 825 -0.472633025 -0.396178271      0.4865132854 -5.629680e-01 R186903
## 826 -1.044653212  0.310917487     -0.2365440378 -1.119027e+00 R187535
## 827  1.220986630  0.004618578      0.4079750381  8.083930e-01 R186911
## 828  0.002580542  0.480818170     -0.2252768614 -2.529608e-01 R187001
##     fosternest squid_pop
## 1        F2102         1
## 2        F1902         1
## 3         A602         1
## 4        A1302         1
## 5        A2602         1
## 6        C2302         1
## 7        C1902         1
## 8        C1302         1
## 9         C602         1
## 10       B2202         1
## 11       B1402         1
## 12       B1002         1
## 13        B502         1
## 14       D1202         1
## 15       D1002         1
## 16        D902         1
## 17        D202         1
## 18        E902         1
## 19        E302         1
## 20       F2402         1
## 21       F2102         1
## 22       F1902         1
## 23       F1702         1
## 24       F1102         1
## 25        F902         1
## 26        F102         1
## 27       G1202         1
## 28        G602         1
## 29        G102         1
## 30       H1302         1
## 31       H1102         1
## 32        H502         1
## 33        A102         1
## 34        A302         1
## 35        A502         1
## 36        A602         1
## 37       A1002         1
## 38       A1302         1
## 39       A1602         1
## 40      A18B02         1
## 41       A2202         1
## 42       A2302         1
## 43       A2602         1
## 44       A2702         1
## 45       C2602         1
## 46       C2302         1
## 47       C1902         1
## 48       C1302         1
## 49        C602         1
## 50       B2202         1
## 51       B1402         1
## 52       B1302         1
## 53        B902         1
## 54        B502         1
## 55        B202         1
## 56       D1302         1
## 57       D1202         1
## 58       D1002         1
## 59        D902         1
## 60        D802         1
## 61        D202         1
## 62       E2002         1
## 63       E1902         1
## 64       E1802         1
## 65       E1602         1
## 66       E1402         1
## 67       E1202         1
## 68       E1102         1
## 69        E902         1
## 70        E702         1
## 71        E302         1
## 72       F2402         1
## 73       F2102         1
## 74       F1902         1
## 75       F1702         1
## 76       F1502         1
## 77       F1102         1
## 78        F902         1
## 79        F202         1
## 80       G2202         1
## 81       G1602         1
## 82       G1202         1
## 83        G702         1
## 84        G602         1
## 85        G502         1
## 86        G102         1
## 87       H3602         1
## 88       H3202         1
## 89       H2802         1
## 90       H1302         1
## 91       H1102         1
## 92        H502         1
## 93        A102         1
## 94        A302         1
## 95        A502         1
## 96        A602         1
## 97       A1002         1
## 98       A1302         1
## 99       A1602         1
## 100      A1802         1
## 101     A18B02         1
## 102      A2202         1
## 103      A2302         1
## 104      A2602         1
## 105      A2702         1
## 106      C2602         1
## 107      C2402         1
## 108      C2302         1
## 109      C1902         1
## 110      C1302         1
## 111       C602         1
## 112       C402         1
## 113       C202         1
## 114       C102         1
## 115      B2502         1
## 116      B2202         1
## 117      B2102         1
## 118      B1902         1
## 119      B1802         1
## 120      B1702         1
## 121      B1602         1
## 122      B1402         1
## 123      B1302         1
## 124      B1202         1
## 125      B1002         1
## 126       B902         1
## 127       B502         1
## 128       B202         1
## 129      D1302         1
## 130      D1202         1
## 131      D1102         1
## 132      D1002         1
## 133       D902         1
## 134       D802         1
## 135       D402         1
## 136       D202         1
## 137       D102         1
## 138      E2002         1
## 139      E1902         1
## 140      E1802         1
## 141      E1702         1
## 142      E1602         1
## 143      E1402         1
## 144      E1302         1
## 145      E1202         1
## 146       E902         1
## 147       E802         1
## 148       E702         1
## 149       E402         1
## 150       E302         1
## 151      F2402         1
## 152      F2102         1
## 153      F1902         1
## 154      F1702         1
## 155      F1502         1
## 156      F1102         1
## 157       F902         1
## 158       F202         1
## 159       F102         1
## 160      G2202         1
## 161      G1902         1
## 162      G1602         1
## 163      G1302         1
## 164       G702         1
## 165       G602         1
## 166       G502         1
## 167       G102         1
## 168      H3602         1
## 169      H3202         1
## 170      H3102         1
## 171      H3002         1
## 172      H2802         1
## 173      H1302         1
## 174      H1102         1
## 175       H702         1
## 176       H502         1
## 177       A102         1
## 178       A302         1
## 179       A502         1
## 180       A602         1
## 181      A1002         1
## 182      A1302         1
## 183      A1602         1
## 184      A1802         1
## 185     A18B02         1
## 186      A2202         1
## 187     A22B02         1
## 188      A2302         1
## 189      A2502         1
## 190      A2602         1
## 191      A2702         1
## 192      C2602         1
## 193      C2402         1
## 194      C2302         1
## 195      C1902         1
## 196      C1302         1
## 197       C602         1
## 198       C402         1
## 199       C202         1
## 200       C102         1
## 201      B2502         1
## 202      B2202         1
## 203      B1902         1
## 204      B1802         1
## 205      B1702         1
## 206      B1602         1
## 207      B1402         1
## 208      B1302         1
## 209      B1202         1
## 210      B1102         1
## 211      B1002         1
## 212       B902         1
## 213       B502         1
## 214       B202         1
## 215      D1302         1
## 216      D1202         1
## 217      D1102         1
## 218      D1002         1
## 219       D902         1
## 220       D802         1
## 221       D402         1
## 222       D202         1
## 223       D102         1
## 224      E2002         1
## 225      E1902         1
## 226      E1802         1
## 227      E1702         1
## 228      E1602         1
## 229      E1402         1
## 230      E1302         1
## 231      E1202         1
## 232      E1102         1
## 233       E902         1
## 234       E802         1
## 235       E702         1
## 236       E402         1
## 237       E302         1
## 238      F2702         1
## 239      F2402         1
## 240      F2102         1
## 241      F1902         1
## 242      F1702         1
## 243      F1502         1
## 244      F1102         1
## 245       F902         1
## 246       F202         1
## 247       F102         1
## 248      G2202         1
## 249      G1902         1
## 250      G1802         1
## 251      G1302         1
## 252      G1202         1
## 253       G702         1
## 254       G602         1
## 255       G502         1
## 256       G102         1
## 257      H3602         1
## 258      H3202         1
## 259      H3102         1
## 260      H3002         1
## 261      H2802         1
## 262      H2502         1
## 263      H1802         1
## 264      H1302         1
## 265      H1102         1
## 266       H702         1
## 267       H502         1
## 268       A102         1
## 269       A302         1
## 270       A502         1
## 271       A602         1
## 272      A1002         1
## 273      A1302         1
## 274      A1602         1
## 275      A1802         1
## 276     A18B02         1
## 277      A2202         1
## 278     A22B02         1
## 279      A2302         1
## 280      A2502         1
## 281      A2602         1
## 282      A2702         1
## 283      C2602         1
## 284      C2402         1
## 285      C2302         1
## 286      C2202         1
## 287      C1902         1
## 288      C1302         1
## 289       C602         1
## 290       C402         1
## 291       C202         1
## 292      B2502         1
## 293      B2202         1
## 294      B2102         1
## 295      B1902         1
## 296      B1802         1
## 297      B1702         1
## 298      B1602         1
## 299      B1402         1
## 300      B1302         1
## 301      B1202         1
## 302      B1102         1
## 303      B1002         1
## 304       B902         1
## 305       B502         1
## 306       B202         1
## 307      D1302         1
## 308      D1202         1
## 309      D1102         1
## 310      D1002         1
## 311       D902         1
## 312       D802         1
## 313       D402         1
## 314       D202         1
## 315       D102         1
## 316      E2002         1
## 317      E1902         1
## 318      E1802         1
## 319      E1702         1
## 320      E1602         1
## 321      E1402         1
## 322      E1302         1
## 323      E1202         1
## 324      E1102         1
## 325       E902         1
## 326       E802         1
## 327       E702         1
## 328       E302         1
## 329      F2702         1
## 330      F2402         1
## 331      F2102         1
## 332      F1902         1
## 333      F1702         1
## 334      F1502         1
## 335      F1102         1
## 336       F902         1
## 337       F202         1
## 338       F102         1
## 339      G2202         1
## 340      G1902         1
## 341      G1802         1
## 342      G1602         1
## 343      G1302         1
## 344      G1202         1
## 345       G702         1
## 346       G602         1
## 347       G502         1
## 348       G102         1
## 349      H3602         1
## 350      H3202         1
## 351      H3102         1
## 352      H2602         1
## 353      H2502         1
## 354      H1802         1
## 355      H1302         1
## 356      H1102         1
## 357       H502         1
## 358       H102         1
## 359       G402         1
## 360       G402         1
## 361       G402         1
## 362       G402         1
## 363       G402         1
## 364       G402         1
## 365      F2502         1
## 366       G402         1
## 367      F2502         1
## 368       G402         1
## 369      C2202         1
## 370      B2502         1
## 371       D402         1
## 372      F2502         1
## 373       G402         1
## 374       A602         1
## 375      A1202         1
## 376      A1602         1
## 377      A2602         1
## 378      C2202         1
## 379      B2502         1
## 380       B202         1
## 381       D402         1
## 382      F2502         1
## 383      G2202         1
## 384       G402         1
## 385       H102         1
## 386       A602         1
## 387      A1202         1
## 388      A1602         1
## 389      A2602         1
## 390      C2602         1
## 391      C2202         1
## 392       C602         1
## 393       C202         1
## 394      B2502         1
## 395       B202         1
## 396       D402         1
## 397       D202         1
## 398       D102         1
## 399      E1702         1
## 400      E1302         1
## 401      E1102         1
## 402       E702         1
## 403      F2502         1
## 404      F2102         1
## 405      F1702         1
## 406       F902         1
## 407       F202         1
## 408      G2202         1
## 409      G1802         1
## 410       G602         1
## 411       G402         1
## 412       G102         1
## 413      H3202         1
## 414      H2502         1
## 415      H1302         1
## 416       H102         1
## 417       A102         1
## 418       A302         1
## 419       A602         1
## 420      A1002         1
## 421      A1202         1
## 422      A1602         1
## 423      A1802         1
## 424      A2202         1
## 425     A22B02         1
## 426      A2302         1
## 427      A2602         1
## 428      C2602         1
## 429      C2402         1
## 430      C2302         1
## 431      C2202         1
## 432      C1302         1
## 433       C602         1
## 434       C202         1
## 435      B1702         1
## 436      B1402         1
## 437      B1102         1
## 438      B1002         1
## 439       B202         1
## 440       D902         1
## 441       D602         1
## 442       D202         1
## 443       D102         1
## 444      E1702         1
## 445      E1402         1
## 446      E1302         1
## 447      E1202         1
## 448      E1102         1
## 449      E1002         1
## 450       E702         1
## 451       E302         1
## 452      F2502         1
## 453      F2402         1
## 454      F2102         1
## 455      F1902         1
## 456      F1702         1
## 457      F1502         1
## 458       F902         1
## 459       F202         1
## 460      G2202         1
## 461      G1802         1
## 462      G1602         1
## 463      G1202         1
## 464       G602         1
## 465       G402         1
## 466       G102         1
## 467      H3602         1
## 468      H3202         1
## 469      H2502         1
## 470      H1302         1
## 471      H1102         1
## 472       H502         1
## 473       H102         1
## 474       A102         1
## 475       A302         1
## 476       A502         1
## 477       A602         1
## 478      A1002         1
## 479      A1202         1
## 480      A1602         1
## 481      A1802         1
## 482      A2202         1
## 483     A22B02         1
## 484      A2302         1
## 485      A2602         1
## 486      C2602         1
## 487      C2302         1
## 488      C2202         1
## 489      C1902         1
## 490      C1302         1
## 491       C602         1
## 492       C202         1
## 493      B2502         1
## 494      B2202         1
## 495      B1702         1
## 496      B1602         1
## 497      B1402         1
## 498      B1202         1
## 499      B1102         1
## 500      B1002         1
## 501       B902         1
## 502       B702         1
## 503       B502         1
## 504       B202         1
## 505      D1202         1
## 506      D1002         1
## 507       D902         1
## 508       D602         1
## 509       D402         1
## 510       D202         1
## 511       D102         1
## 512      E1702         1
## 513      E1402         1
## 514      E1302         1
## 515      E1202         1
## 516      E1102         1
## 517      E1002         1
## 518       E902         1
## 519       E802         1
## 520       E702         1
## 521       E302         1
## 522      F2502         1
## 523      F2402         1
## 524      F2102         1
## 525      F1902         1
## 526      F1702         1
## 527      F1502         1
## 528      F1102         1
## 529       F902         1
## 530       F202         1
## 531      G2202         1
## 532      G1902         1
## 533      G1602         1
## 534      G1302         1
## 535      G1202         1
## 536       G602         1
## 537       G402         1
## 538       G102         1
## 539      H3602         1
## 540      H3202         1
## 541      H2502         1
## 542      H1802         1
## 543      H1302         1
## 544      H1102         1
## 545       H702         1
## 546       H502         1
## 547       H302         1
## 548       H102         1
## 549       A102         1
## 550       A302         1
## 551       A502         1
## 552       A602         1
## 553      A1002         1
## 554      A1202         1
## 555      A1302         1
## 556      A1602         1
## 557      A1802         1
## 558     A18B02         1
## 559      A2202         1
## 560     A22B02         1
## 561      A2302         1
## 562      A2602         1
## 563      A2702         1
## 564      C2602         1
## 565      C2402         1
## 566      C2302         1
## 567      C2202         1
## 568      C1902         1
## 569      C1302         1
## 570       C402         1
## 571       C202         1
## 572      B2502         1
## 573      B2202         1
## 574      B2102         1
## 575      B1702         1
## 576      B1602         1
## 577      B1302         1
## 578      B1202         1
## 579      B1102         1
## 580      B1002         1
## 581       B902         1
## 582       B502         1
## 583       B202         1
## 584      D1302         1
## 585      D1202         1
## 586      D1102         1
## 587      D1002         1
## 588       D902         1
## 589       D802         1
## 590       D602         1
## 591       D402         1
## 592       D202         1
## 593       D102         1
## 594      E2002         1
## 595      E1702         1
## 596      E1602         1
## 597      E1402         1
## 598      E1302         1
## 599      E1202         1
## 600      E1102         1
## 601       E902         1
## 602       E802         1
## 603       E702         1
## 604       E302         1
## 605      F2702         1
## 606      F2502         1
## 607      F2402         1
## 608      F2102         1
## 609      F1902         1
## 610      F1702         1
## 611      F1502         1
## 612      F1102         1
## 613       F902         1
## 614       F202         1
## 615       F102         1
## 616      G2202         1
## 617      G1902         1
## 618      G1802         1
## 619      G1602         1
## 620      G1302         1
## 621      G1202         1
## 622       G602         1
## 623       G102         1
## 624      H3602         1
## 625      H3202         1
## 626      H3002         1
## 627      H2802         1
## 628      H2502         1
## 629      H1802         1
## 630      H1302         1
## 631      H1102         1
## 632       H502         1
## 633       H302         1
## 634       H102         1
## 635       A102         1
## 636       A302         1
## 637       A502         1
## 638       A602         1
## 639      A1002         1
## 640      A1202         1
## 641      A1302         1
## 642      A1602         1
## 643      A1802         1
## 644     A18B02         1
## 645      A2202         1
## 646     A22B02         1
## 647      A2302         1
## 648      A2602         1
## 649      A2702         1
## 650      C2602         1
## 651      C2402         1
## 652      C2302         1
## 653      C2202         1
## 654      C1902         1
## 655      C1302         1
## 656       C602         1
## 657       C402         1
## 658       C202         1
## 659      B2502         1
## 660      B2202         1
## 661      B2102         1
## 662      B1902         1
## 663      B1702         1
## 664      B1602         1
## 665      B1402         1
## 666      B1302         1
## 667      B1202         1
## 668      B1102         1
## 669      B1002         1
## 670       B902         1
## 671       B502         1
## 672       B202         1
## 673      D1302         1
## 674      D1202         1
## 675      D1002         1
## 676       D902         1
## 677       D802         1
## 678       D602         1
## 679       D402         1
## 680       D202         1
## 681       D102         1
## 682      E2002         1
## 683      E1902         1
## 684      E1802         1
## 685      E1702         1
## 686      E1602         1
## 687      E1402         1
## 688      E1302         1
## 689      E1202         1
## 690      E1102         1
## 691       E902         1
## 692       E802         1
## 693       E702         1
## 694       E402         1
## 695       E302         1
## 696      F2702         1
## 697      F2502         1
## 698      F2102         1
## 699      F1902         1
## 700      F1702         1
## 701      F1502         1
## 702      F1102         1
## 703       F902         1
## 704       F202         1
## 705       F102         1
## 706      G2202         1
## 707      G1902         1
## 708      G1802         1
## 709      G1702         1
## 710      G1602         1
## 711      G1302         1
## 712      G1202         1
## 713       G602         1
## 714       G502         1
## 715       G402         1
## 716       G102         1
## 717      H3602         1
## 718      H3202         1
## 719      H3102         1
## 720      H3002         1
## 721      H2802         1
## 722      H2502         1
## 723      H1802         1
## 724      H1302         1
## 725      H1102         1
## 726       H702         1
## 727       H502         1
## 728       H302         1
## 729       H102         1
## 730       A102         1
## 731       A302         1
## 732       A502         1
## 733       A602         1
## 734      A1002         1
## 735      A1202         1
## 736      A1302         1
## 737      A1602         1
## 738      A1802         1
## 739     A18B02         1
## 740      A2202         1
## 741     A22B02         1
## 742      A2302         1
## 743      A2502         1
## 744      A2602         1
## 745      A2702         1
## 746      C2602         1
## 747      C2402         1
## 748      C2302         1
## 749      C2202         1
## 750      C1902         1
## 751      C1302         1
## 752       C402         1
## 753      B2502         1
## 754      B2202         1
## 755      B2102         1
## 756      B1902         1
## 757      B1802         1
## 758      B1702         1
## 759      B1602         1
## 760      B1402         1
## 761      B1302         1
## 762      B1202         1
## 763      B1002         1
## 764       B902         1
## 765       B502         1
## 766       B202         1
## 767      D1302         1
## 768      D1202         1
## 769      D1102         1
## 770      D1002         1
## 771       D902         1
## 772       D802         1
## 773       D602         1
## 774       D402         1
## 775       D202         1
## 776       D102         1
## 777      E2002         1
## 778      E1902         1
## 779      E1802         1
## 780      E1702         1
## 781      E1602         1
## 782      E1402         1
## 783      E1302         1
## 784      E1202         1
## 785      E1102         1
## 786      E1002         1
## 787       E902         1
## 788       E802         1
## 789       E702         1
## 790       E402         1
## 791       E302         1
## 792      F2702         1
## 793      F2502         1
## 794      F2402         1
## 795      F2102         1
## 796      F1902         1
## 797      F1702         1
## 798      F1502         1
## 799      F1102         1
## 800       F902         1
## 801       F202         1
## 802       F102         1
## 803      G2202         1
## 804      G1902         1
## 805      G1802         1
## 806      G1702         1
## 807      G1602         1
## 808      G1302         1
## 809      G1202         1
## 810       G702         1
## 811       G602         1
## 812       G502         1
## 813       G402         1
## 814       G102         1
## 815      H3602         1
## 816      H3202         1
## 817      H3102         1
## 818      H3002         1
## 819      H2802         1
## 820      H2602         1
## 821      H2502         1
## 822      H1802         1
## 823      H1302         1
## 824      H1102         1
## 825       H702         1
## 826       H502         1
## 827       H302         1
## 828       H102         1
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
## REML criterion at convergence: 8158.9
## 
## Random effects:
##  Groups     Name        Variance Corr
##  individual (Intercept) 1.0441       
##             environment 0.5863   0.05
##  Residual               0.5183       
## Number of obs: 3000, groups:  individual, 300
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept) -0.02146    0.06061  -0.354
## environment  0.26719    0.04685   5.703
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
## REML criterion at convergence: 7920.1
## 
## Random effects:
##  Groups     Name        Variance Corr
##  individual (Intercept) 1.0083       
##             environment 0.5075   0.50
##  Residual               0.4925       
## Number of obs: 3000, groups:  individual, 300
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept) -0.07204    0.05952  -1.210
## environment  0.15551    0.04371   3.558
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




