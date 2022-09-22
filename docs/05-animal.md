# Genetic effects {#animal}

This vignette assumes that you are generally happy with how the `sim_population()` function works.

## Additive genetics effects {#va}
In order to simulate breeding values (additive genetic effects), we can provide the `simulate_population()` function with the relatedness structure in the population. The simplest way to do this is providing a pedigree using the the `pedigree` argument (a genetic relatedness matrix could also be given to the `cov_str` argument). The input to this argument needs to be a list, and the name of the pedigree in the list links it with the item in the parameter list.

**NOTE** the `simulate_population` function has very little error checking of pedigree structure at the moment 

When simulating breeding values, **all** individuals in pedigree need to be in the data_structure and *vice versa*. Having unsampled individuals (for example the base population) can be achieved in the sampling stage (not implemented yet). 

Lets start by importing a pedigree

```r
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



We might want to simulate repeated measurements to allow estimation of permanent environment effects. This is where being able to have something in the parameter list with a different name to the grouping factor is useful. In this way permanent environmental and additive genetic effects can be simulated in different parts of the parameter list, and linked to the same part of the data_structure.


```r
## make data structure with two observations per individual
ds <- data.frame(individual=rep(BTped[,1], 2))

squid_data <- simulate_population(
  data_structure = ds, 
  pedigree=list(animal=BTped),
  parameters = list(
    individual = list(
      vcov = 0.3
    ),
    animal = list(
      group="individual",
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
##                  y individual_effect animal_effect   residual individual
## R187888 -0.7747080       -0.48545266    0.34386743 -0.6331228    R187557
## R187646  1.2637980        1.19344724    0.23622685 -0.1658761    R187559
## R187330  1.2315559        0.03483088    0.58394160  0.6127834    R187568
## R187374 -0.4667102       -0.64830239    0.03600587  0.1455863    R187518
## R187225  0.5837659        0.69669125    0.14473226 -0.2576576    R187528
## R187133 -0.1058035       -0.54704434    0.06288580  0.3783550    R187945
##         squid_pop
## R187888         1
## R187646         1
## R187330         1
## R187374         1
## R187225         1
## R187133         1
```

```r
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


```r
ds <- data.frame(animal=BTped[,"animal"],sex=sample(c("Female","Male"),nrow(BTped), replace=TRUE))

squid_data <- simulate_population(
  parameters = list(
    sex=list(
      fixed=TRUE,
      names=c("female","male"),
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
  model = "y = female + male + I(female)*G_female + I(male)*G_male + residual"
)

data <- get_population_data(squid_data)
head(data)
```

```
##             y female male   G_female       G_male    residual  animal    sex
## 1 -0.07664521      1    0  0.2153092 -0.277927530  0.20804561 R187557 Female
## 2  0.32705982      0    1  0.2120106 -0.195909978  0.02296980 R187559   Male
## 3 -0.32068808      0    1  0.2096736 -0.378169745 -0.44251833 R187568   Male
## 4  0.89896430      0    1  0.1734671 -0.157022020  0.55598632 R187518   Male
## 5  0.46138651      0    1 -0.5514875 -0.075445373  0.03683188 R187528   Male
## 6  0.51619286      0    1  0.4333079 -0.004309735  0.02050260 R187945   Male
##   squid_pop
## 1         1
## 2         1
## 3         1
## 4         1
## 5         1
## 6         1
```

```r
par(mfrow=c(1,2))
boxplot(y~factor(sex),data)
plot(G_female~G_male,data)
```

<img src="05-animal_files/figure-html/unnamed-chunk-6-1.png" width="960" />
<br>

## GxE

```r
squid_data <- simulate_population(
  parameters = list(
    animal = list(
      names = c("G_int","G_slope"),
      mean =c(0,0.2), 
      vcov =matrix(c(1,0.3,0.3,0.5),ncol=2,nrow=2,byrow=TRUE)
    ),
    observation= list(
      names = c("environment"),
      vcov =c(1)
    ), 
    residual = list(
      names = c("residual"),
      vcov =c(0.5)
    )
  ),
  data_structure=rbind(BTped,BTped,BTped,BTped,BTped),
  pedigree = list(animal=BTped),
  model="y = G_int + G_slope * environment + residual"
)

data <- get_population_data(squid_data)
library(lme4)
short_summary <- function(x) print(summary(x), correlation=FALSE, show.resids=FALSE, ranef.comp = c("Variance"))

short_summary(lmer(y ~ environment + (1+environment|animal),data))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: y ~ environment + (1 + environment | animal)
##    Data: data
## 
## REML criterion at convergence: 15028.3
## 
## Random effects:
##  Groups   Name        Variance Corr
##  animal   (Intercept) 0.9701       
##           environment 0.4891   0.42
##  Residual             0.4960       
## Number of obs: 5200, groups:  animal, 1040
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept) -0.03107    0.03243  -0.958
## environment  0.36240    0.02504  14.473
```

## Indirect Genetic Effects
Indirect genetic effects are a bit more difficult to code. Lets take the example of maternal genetic effects. The maternal genetic effect that affects an individual's phenotype, is that of its mother, not itself. Here we can use `[]` to index the levels of the random effects within the formula. This means that we can simulate the direct genetic and maternal genetic effects that an individual has (and the covariance between them), as well as generating an individual's phenotype from its own direct genetic effects, and its mother's maternal genetic effect.


```r
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
  model = "y = direct + maternal[dam] + residual"
)

data <- get_population_data(squid_data)

head(data)
```

```
##          y     direct    maternal   residual  animal  dam sire squid_pop
## R187557 NA  1.0057121  0.47221848 -0.8189544 R187557 <NA> <NA>         1
## R187559 NA -0.8595959 -1.01711203 -0.1827230 R187559 <NA> <NA>         1
## R187568 NA  1.1343718  0.00253274  1.3185354 R187568 <NA> <NA>         1
## R187518 NA -1.3268493 -0.93606626 -0.7956225 R187518 <NA> <NA>         1
## R187528 NA -0.1990065 -0.38247668  0.6363527 R187528 <NA> <NA>         1
## R187945 NA  0.8554560  0.97020718 -0.1085491 R187945 <NA> <NA>         1
```




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

