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
##                  y animal_effect   residual  animal  dam sire squid_pop
## R187557 -0.7104031    -0.1749195 -0.5354836 R187557 <NA> <NA>         1
## R187559  1.2892857     0.6665992  0.6226864 R187559 <NA> <NA>         1
## R187568  1.6393384     0.6900526  0.9492857 R187568 <NA> <NA>         1
## R187518  1.2545103     0.3999815  0.8545287 R187518 <NA> <NA>         1
## R187528 -0.4945722    -0.3827869 -0.1117853 R187528 <NA> <NA>         1
## R187945  0.1780773    -0.3703026  0.5483799 R187945 <NA> <NA>         1
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
##                   y individual_effect animal_effect   residual individual
## R187888 -0.29800108        -0.3030617    -0.2605827  0.2656433    R187557
## R187646  0.17464214         0.3344371    -0.5309132  0.3711182    R187559
## R187330  1.17443051         0.7872808     0.9885985 -0.6014488    R187568
## R187374 -0.88713844        -0.4727921    -0.1421482 -0.2721982    R187518
## R187225 -0.06469935        -0.2399442     0.5130975 -0.3378527    R187528
## R187133  1.72913172         0.5764696     0.7417825  0.4108797    R187945
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
## R187557 -0.8385548  0.1841058      0.6786441     -0.3935136 -1.5171990
## R187559 -0.4701207  1.0400757     -1.4113157      1.0666325  0.9411949
## R187568 -0.1846425  0.3584195      1.0712568      0.3008260 -1.2558993
## R187518 -0.9746306  1.1945939     -2.3299460      0.7226137  1.3553154
## R187528 -0.8318896  0.2290793      0.3754430      0.3256557 -1.2073326
## R187945  1.8103155 -1.2127095      2.1203028     -1.2159118 -0.3099873
##            residual2  animal  dam sire squid_pop
## R187557  0.577619380 R187557 <NA> <NA>         1
## R187559 -0.026556865 R187559 <NA> <NA>         1
## R187568  0.057593543 R187568 <NA> <NA>         1
## R187518  0.471980260 R187518 <NA> <NA>         1
## R187528 -0.096576413 R187528 <NA> <NA>         1
## R187945  0.003202301 R187945 <NA> <NA>         1
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
##            y female male    G_female       G_male   residual  animal    sex
## 1  0.2299693      0    1  0.37038919  0.002373921 -0.2724046 R187557   Male
## 2 -0.2823380      0    1  0.36135697 -0.883784973  0.1014469 R187559   Male
## 3 -0.9727263      1    0 -0.21717329  0.638474950 -0.2555530 R187568 Female
## 4 -0.8084307      1    0  0.07171387 -0.239679105 -0.3801446 R187518 Female
## 5 -0.2248901      0    1  0.35292528 -0.471803420 -0.2530867 R187528   Male
## 6 -0.5270464      1    0  0.16055916 -0.166979686 -0.1876056 R187945 Female
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
## REML criterion at convergence: 15088.6
## 
## Random effects:
##  Groups   Name        Variance Corr
##  animal   (Intercept) 1.0188       
##           environment 0.4827   0.45
##  Residual             0.5084       
## Number of obs: 5200, groups:  animal, 1040
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept) -0.00276    0.03317  -0.083
## environment  0.26450    0.02517  10.510
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
##          y     direct    maternal    residual  animal  dam sire squid_pop
## R187557 NA  0.4438700  1.15175094 -0.44495274 R187557 <NA> <NA>         1
## R187559 NA  2.2970019 -0.39471324 -0.18850357 R187559 <NA> <NA>         1
## R187568 NA  2.1728927  0.54220433 -0.40754005 R187568 <NA> <NA>         1
## R187518 NA  0.3898664 -0.05419618  0.56079375 R187518 <NA> <NA>         1
## R187528 NA -0.4593114 -0.48486427  0.09109256 R187528 <NA> <NA>         1
## R187945 NA -1.0554649 -1.35908176 -0.56942704 R187945 <NA> <NA>         1
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

