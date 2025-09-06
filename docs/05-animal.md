# Genetic effects {#animal}

This vignette assumes that you are generally happy with how the `sim_population()` function works.

## Additive genetics effects {#va}
In order to simulate breeding values (additive genetic effects), we can provide the `simulate_population()` function with the relatedness structure in the population. The simplest way to do this is providing a pedigree using the the `pedigree` argument (a genetic relatedness matrix could also be given to the `cov_str` argument). The input to this argument needs to be a list, and the name of the pedigree in the list links it with the item in the parameter list.

**NOTE** the `simulate_population` function has very little error checking of pedigree structure at the moment 

When simulating breeding values, **all** individuals in pedigree need to be in the data_structure and *vice versa*. Having unsampled individuals (for example the base population) can be achieved in the sampling stage (not implemented yet). 

Lets start by importing a pedigree

``` r
library(MCMCglmm)
data(BTped)
head(BTped)
```

```
##    animal  dam sire
## 1 R187557 <NA> <NA>
## 2 R187559 <NA> <NA>
## 3 R187568 <NA> <NA>
## 4 R187518 <NA> <NA>
## 5 R187528 <NA> <NA>
## 6 R187945 <NA> <NA>
```

We can use this pedigree as a data_structure

```r
squid_data <- simulate_population(
  data_structure = BTped, 
 
  pedigree = list(animal=BTped),
  
  parameters =list(
    animal = list(
      vcov = 0.2
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
##                    y animal_effect   residual  animal  dam sire squid_pop
## R187557  0.001049242     0.2717576 -0.2707083 R187557 <NA> <NA>         1
## R187559  0.337915749     0.6864674 -0.3485517 R187559 <NA> <NA>         1
## R187568 -0.733484258    -0.1971984 -0.5362859 R187568 <NA> <NA>         1
## R187518  0.780509757     0.2115614  0.5689484 R187518 <NA> <NA>         1
## R187528  0.198435519     0.3475518 -0.1491163 R187528 <NA> <NA>         1
## R187945 -0.854447851    -0.2441706 -0.6102772 R187945 <NA> <NA>         1
```

```r
# Ainv<-inverseA(BTped)$Ainv
# mod <- MCMCglmm(y~1, random=~ animal,data=data,ginverse=list(animal=Ainv),verbose=FALSE)
# summary(mod)
```

We might want to simulate repeated measurements to allow estimation of permanent environment effects. The simplest way to do this is to create a duplicated column in the data structure of the individual IDs. Permanent environment effects that are not linked to the pedigree can then be simulated.

<!-- 
We might want to simulate repeated measurements to allow estimation of permanent environment effects. This is where being able to have something in the parameter list with a different name to the grouping factor is useful. In this way permanent environmental and additive genetic effects can be simulated in different parts of the parameter list, and linked to the same part of the data_structure.
 -->

:::: {.blackbox data-latex=""}
::: {.center data-latex=""}
**NOTICE!**
:::
The instructions given for simulating permanent environment effects using squidSim were incorrect in the vignette prior to version 0.2.0 (updated in September 2025). 
::::



``` r
## make data structure with two observations per individual, with ID duplicated in two columns, animal and individual, and link the animal column in the data structure to the pedigree
ds <- data.frame(animal=rep(BTped[,1], 2),individual=rep(BTped[,1], 2))

squid_data <- simulate_population(
  data_structure = ds, 
  pedigree=list(animal=BTped),
  parameters = list(
    individual = list(
      vcov = 0.3
    ),
    animal = list(
      vcov = 0.2
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
##            y individual_effect animal_effect    residual  animal individual
## 1 -0.1819649       -0.33421503   -0.19202089  0.34427104 R187557    R187557
## 2  0.2310721        0.59969478   -0.07796869 -0.29065395 R187559    R187559
## 3 -0.3916378        0.10109372   -0.13579773 -0.35693382 R187568    R187568
## 4  0.1983046        0.59248696   -0.38724498 -0.00693742 R187518    R187518
## 5 -0.2105813        0.23900807   -0.67656193  0.22697260 R187528    R187528
## 6  0.2974375       -0.09572865    0.07892767  0.31423850 R187945    R187945
##   squid_pop
## 1         1
## 2         1
## 3         1
## 4         1
## 5         1
## 6         1
```

``` r
# Ainv<-inverseA(BTped)$Ainv
# data$animal_id <- data$individual
# mod <- MCMCglmm(y~1, random=~ individual + animal_id,data=data,ginverse=list(animal_id=Ainv),verbose=FALSE)
# summary(mod)
```


## Multivariate genetic effects

We can simulate genetic effects affecting multiple phenotypes and the covariance between them, by specifying the number of response variables, and a covariance matrix, instead of only a variance.

```r
squid_data <- simulate_population(
  data_structure = BTped,
  pedigree = list(animal = BTped),
  n_response=2,
  parameters = list(
    animal = list(
      vcov = diag(2)

    ),
    residual = list(
      vcov = diag(2)
    )
  )
)

data <- get_population_data(squid_data)
head(data)
```

```
##                 y1         y2 animal_effect1 animal_effect2  residual1
## R187557  0.2270163  0.7349977    -0.14996162     -0.9763782  0.3769779
## R187559  0.8121651  2.6809126    -0.20470701      1.8215682  1.0168721
## R187568 -2.3218758 -1.3770079    -1.83531039      0.2799440 -0.4865654
## R187518 -1.5891070 -1.5192159    -0.23712091     -0.2315827 -1.3519861
## R187528  0.6954574 -2.1002711     0.05902919     -1.5265765  0.6364282
## R187945 -2.9368649 -0.3194446    -1.38312923      0.9299490 -1.5537356
##          residual2  animal  dam sire squid_pop
## R187557  1.7113759 R187557 <NA> <NA>         1
## R187559  0.8593444 R187559 <NA> <NA>         1
## R187568 -1.6569519 R187568 <NA> <NA>         1
## R187518 -1.2876332 R187518 <NA> <NA>         1
## R187528 -0.5736946 R187528 <NA> <NA>         1
## R187945 -1.2493935 R187945 <NA> <NA>         1
```

```r
# Ainv<-inverseA(BTped)$Ainv
# mod <- MCMCglmm(cbind(y1,y2)~1,random=~us(trait):animal, rcov=~us(trait):units,data=data,family=rep("gaussian",2),verbose=FALSE,ginverse=list(animal=Ainv))
# summary(mod)
```

<br>




## Sex specific genetic variance and inter-sexual genetic correlations


``` r
ds <- data.frame(animal=BTped[,"animal"],sex=sample(c("Female","Male"),nrow(BTped), replace=TRUE))

squid_data <- simulate_population(
  parameters = list(
    sex=list(
      fixed=TRUE,
      names=c("Female","Male"),
      beta=c(-0.5,0.5)
    ),
    animal= list(
      names = c("G_female","G_male"),
      vcov =matrix(c(0.1,-0.1,-0.1,0.4), nrow=2, ncol=2 ,byrow=TRUE)
      ),
    residual = list(
      names="residual",
      vcov = 0.1
    )
  ),
  data_structure = ds,
  pedigree = list(animal=BTped),
  model = "y = Female + Male + I(Female)*G_female + I(Male)*G_male + residual"
)

data <- get_population_data(squid_data)
head(data)
```

```
##            y Female Male    G_female      G_male    residual  animal    sex
## 1 -0.2529123      1    0 -0.03100451 -0.04841182  0.27809218 R187557 Female
## 2 -0.3205604      0    1 -0.33966106 -0.66849369 -0.15206674 R187559   Male
## 3  0.1583027      0    1  0.23986200 -0.26773808 -0.07395921 R187568   Male
## 4  0.9643963      0    1 -0.10503600  0.41074154  0.05365477 R187518   Male
## 5  0.5121345      0    1  0.31946969 -0.11387147  0.12600596 R187528   Male
## 6  1.2310101      0    1  0.11330433  0.47106512  0.25994496 R187945   Male
##   squid_pop
## 1         1
## 2         1
## 3         1
## 4         1
## 5         1
## 6         1
```

``` r
par(mfrow=c(1,2))
boxplot(y~factor(sex),data)
plot(G_female~G_male,data)
```

<img src="05-animal_files/figure-html/unnamed-chunk-6-1.png" width="960" />
<br>

## Indirect Genetic Effects {#IGE}
Indirect genetic effects are a bit more difficult to code. Lets take the example of maternal genetic effects. The maternal genetic effect that affects an individual's phenotype, is that of its mother, not itself. Here we can use `[]` to index the levels of the random effects within the formula. We need to be careful here as internally in `simulate_population()` the indexing of the factors in the data structure is done independently. We therefore need to generate a index for the mothers that links to the individual. We can do this using the `index_link` argument - in the code below we create a new factor to index with, called `dam_link`, that is the dam factor in our data structure, that has been indexed to match the animal factor.

Using this indexing trick, we can simulate the direct genetic and maternal genetic effects that an individual has (and the covariance between them), as well as generating an individual's phenotype from its own direct genetic effects, and its mother's maternal genetic effect.


``` r
squid_data <- simulate_population(
  parameters=list(
    animal = list(
      names=c("direct","maternal"),
      vcov = matrix(c(1,0.3,0.3,0.5),2,2)
    ),
    residual = list(
      names="residual",
      vcov = 0.5
    )
  ),
  data_structure=BTped,
  pedigree=list(animal=BTped),
  index_link=list(dam_link="dam-animal"),
  model = "y = direct + maternal[dam_link] + residual"
)
```

```
## Warning: Not all levels are of dam are present in animal meaning that there
## will be NAs in the new grouping factor
```

``` r
data <- get_population_data(squid_data)

head(data)
```

```
##    y     direct   maternal    residual  animal  dam sire squid_pop
## 1 NA  1.4774524  0.6386346 -1.40513270 R187557 <NA> <NA>         1
## 2 NA  0.4613623  0.7334137  0.39278836 R187559 <NA> <NA>         1
## 3 NA -1.7555338 -0.6943673 -1.11469969 R187568 <NA> <NA>         1
## 4 NA  0.2308076 -0.1172376 -1.22054447 R187518 <NA> <NA>         1
## 5 NA -0.6111666 -0.5457180  0.03596706 R187528 <NA> <NA>         1
## 6 NA  0.3943155  0.2788443  0.29346050 R187945 <NA> <NA>         1
```



## GxE
Coming soon...

<!-- 
  I dont know why this doesnt work

  
squid_data <- simulate_population(
  parameters = list(
    animal = list(
      names = c("G_int","G_slope"),
      mean = c(0,0), 
      vcov = matrix(c(1,0.3,0.3,0.5),ncol=2,nrow=2,byrow=TRUE),
      beta = c(1,0)
    ),
    observation= list(
      names = c("environment"),
      vcov = c(0.2)
    ), 
    residual = list(
      names = c("residual"),
      vcov = c(0.5)
    ),
    interactions=list(
      names = "G_slope:environment",
      beta = 1
    )
  ),
  data_structure=rbind(BTped,BTped,BTped,BTped,BTped),
  pedigree = list(animal=BTped)
)

data <- get_population_data(squid_data)
library(lme4)
short_summary <- function(x) print(summary(x), correlation=FALSE, show.resids=FALSE, ranef.comp = c("Variance"))

short_summary(lmer(y ~ environment + (1+environment|animal),data))

 -->


## Dominance
Coming soon...
<!--
Here we can make use of the dominance relatedness matrices that can be generated in the `nadiv` package

NOTE: not working fully yet!!! 
 
 -->

## Inbreeding depression
Coming soon...


## Genetic Groups
Coming soon...

