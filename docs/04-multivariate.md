
# Multi-response Models {#multivariate}

We can simulate multiple response variables, that covary at different hierarchical levels. In the case of a simple random effects model, we can have a covariance matrix at each level, 

<!-- Anne: perhaps add a short sentence of introduction here such as “We simulate here no longer one phenotype y varying across I individuals, but a number of phenotypes that (co)vary across individuals..”
 -->

<!-- 
$$
\begin{bmatrix} 
  y^{(1)} \\ 
  y^{(2)} 
\end{bmatrix}
=   \beta_0 +  Z_j + \epsilon_{i,j}
$$ 
-->
<div class="alert alert-info">

$$
\boldsymbol{y}_{ij} = \boldsymbol{\beta}_0 + \boldsymbol{u}_j + \boldsymbol{\epsilon}_{ij}
$$
$$
\boldsymbol{u}_i \sim \mathcal{N}(0, \Sigma_u)
$$
$$
\boldsymbol{\epsilon}_{i} \sim \mathcal{N}(0, \Sigma_{\epsilon})
$$

</div>

We can indicate that there are multiple phenotypes within the parameter list in two ways. First, we can use `n_response` in the parameter list, and specifying the covariance matrix (vcov) at each level. In this way we can simulate covariance at each level. 

<!-- Anne: Should the blue background model not come after the first paragraph?
  We probably just need an intro paragraph
 -->


```r
squid_data <- simulate_population(
  data_structure=make_structure(structure = "individual(100)",repeat_obs=10),
  n_response = 2,
  parameters=list(
    individual = list(
      vcov = matrix(c(1,0.5,0.5,1),nrow=2,ncol=2,byrow=TRUE)
    ),
    residual = list(
      vcov = matrix(c(1,0.5,0.5,1),nrow = 2,ncol = 2,byrow=TRUE)
    )
  )  
)

data <- get_population_data(squid_data)
head(data)
```

```
##          y1         y2 individual_effect1 individual_effect2  residual1
## 1 1.7154548 -0.3400219           1.917971          0.7785752 -0.2025162
## 2 0.5628202  0.3404365           1.917971          0.7785752 -1.3551508
## 3 2.2598837  0.1039745           1.917971          0.7785752  0.3419127
## 4 1.2400569  1.7172877           1.917971          0.7785752 -0.6779141
## 5 0.8401039  0.2945199           1.917971          0.7785752 -1.0778671
## 6 2.6972366  0.0814206           1.917971          0.7785752  0.7792656
##    residual2 individual squid_pop
## 1 -1.1185971          1         1
## 2 -0.4381387          1         1
## 3 -0.6746007          1         1
## 4  0.9387125          1         1
## 5 -0.4840553          1         1
## 6 -0.6971546          1         1
```


The formulation above (just random effects), can be simulated in a similar way with `beta` as an identity matrix (i.e. a predictor for each trait).
<!-- I got lost in the next section starting with: “The formulation above (just random effects), can be simulated in a similar way with beta as an identity matrix (i.e. a predictor for each trait).” might need more explanation.
 -->

```r
squid_data <- simulate_population(
  data_structure= make_structure(structure = "individual(100)",repeat_obs=10),
  n_response=2,
  parameters=list(
    individual = list(
      vcov =matrix(c(1,0.5,0.5,1),nrow=2,ncol=2,byrow=TRUE),
      beta= diag(2)
    ),
    residual = list(
      vcov =matrix(c(1,0.5,0.5,1),nrow=2,ncol=2,byrow=TRUE),
      beta= diag(2)
    )
  )
)

data <- get_population_data(squid_data)
head(data)
```

```
##           y1         y2 individual_effect1 individual_effect2  residual1
## 1 -1.7790297 -2.1313181         -0.2306292          0.2132262 -1.5484005
## 2  0.9989910  1.5914308         -0.2306292          0.2132262  1.2296201
## 3  0.5519600  0.4977197         -0.2306292          0.2132262  0.7825892
## 4  0.8959494  0.9204513         -0.2306292          0.2132262  1.1265785
## 5  0.5997486  0.8608984         -0.2306292          0.2132262  0.8303778
## 6 -1.4619544 -0.8689692         -0.2306292          0.2132262 -1.2313253
##    residual2 individual squid_pop
## 1 -2.3445443          1         1
## 2  1.3782046          1         1
## 3  0.2844935          1         1
## 4  0.7072251          1         1
## 5  0.6476722          1         1
## 6 -1.0821955          1         1
```

```r
# library(MCMCglmm)
# mod <- MCMCglmm(cbind(y1,y2)~1,random=~us(trait):individual, rcov=~us(trait):units,data=data,family=rep("gaussian",2),verbose=FALSE)
# summary(mod)
```


<!-- 
https://stackoverflow.com/questions/63007496/how-to-create-an-editable-matrix-in-shiny-app
make little shiny app that allows you to enter diagonal and 

 -->
Second, we can build up predictors at each level that drive this covariance. Here we make `beta` into a matrix ($B$), with predictors as rows, and responses as columns. 

<div class="alert alert-info">


$$\boldsymbol{y}_{ij} = \boldsymbol{\beta}_0 + \boldsymbol{x}_{i} B_x + \boldsymbol{u}_j + \boldsymbol{\epsilon}_{ij}$$

$$\boldsymbol{x}_i \sim \mathcal{N}(\boldsymbol{\mu}_x, \Sigma_x)$$

$$\boldsymbol{u}_i \sim \mathcal{N}(0, \Sigma_u)$$

$$\boldsymbol{\epsilon}_{i} \sim \mathcal{N}(0, \Sigma_{\epsilon})$$

</div>

Alternatively, you could also create multivariate phenotypes being affected by the same predictors. Here we have two phenotypes, affected by three predictors, and so we can create a 3x2 matrix of betas


```r
beta <- matrix(c(
  0.5, 0.1,
  0.2, 0.2,
  0.3, 0.1
  ),nrow=3,ncol=2,byrow=TRUE)
beta
```

```
##      [,1] [,2]
## [1,]  0.5  0.1
## [2,]  0.2  0.2
## [3,]  0.3  0.1
```


```r
squid_data <- simulate_population(
  data_structure= make_structure(structure = "individual(100)",repeat_obs=20),
  n_response=2,
  parameters= list(
    individual = list(
      vcov =matrix(c(1,0.5,0.5,1),nrow=2,ncol=2,byrow=TRUE)
    ),
    observation = list(
      names = c("temperature", "rainfall", "wind"),
      beta= beta
    ),
    residual = list(
      vcov= matrix(c(1,0.5,0.5,1),nrow=2,ncol=2,byrow=TRUE)
    )
  )
)

data <- get_population_data(squid_data)
head(data)
```

```
##           y1         y2 individual_effect1 individual_effect2 temperature
## 1 1.22310660  0.5923479          0.3440492           0.579633   1.5166067
## 2 1.41370650 -0.1014570          0.3440492           0.579633   1.4510438
## 3 0.61015279 -0.4051245          0.3440492           0.579633   0.6377108
## 4 0.52717455  1.4791758          0.3440492           0.579633  -0.4579670
## 5 0.36549918  0.3292956          0.3440492           0.579633   0.3432748
## 6 0.06027212 -0.4377444          0.3440492           0.579633   0.1036752
##     rainfall        wind    residual1  residual2 individual squid_pop
## 1  0.6364945 -0.00366005 -0.005446858 -0.2658786          1         1
## 2  0.2034993  0.19014050  0.246393344 -0.8859082          1         1
## 3 -2.3866566  0.05766300  0.407280601 -0.5769636          1         1
## 4  0.1370347  1.41738837 -0.040514635  0.7761938          1         1
## 5  0.5636609 -0.85833490 -0.005419163 -0.3115636          1         1
## 6 -0.4653576  1.49089522 -0.689811760 -1.0837629          1         1
```

```r
# library(MCMCglmm)
# mod <- MCMCglmm(cbind(y1,y2)~1,random=~us(trait):individual, rcov=~us(trait):units,data=data,family=rep("gaussian",2),verbose=FALSE)
# summary(mod)
```




```r
squid_data <- simulate_population(
  data_structure= make_structure(structure = "individual(100)",repeat_obs=20),
  n_response=2,
  
  parameters= list(
    individual = list(
      vcov =matrix(c(1,0.5,0.5,1),nrow=2,ncol=2,byrow=TRUE)
    ),
    observation = list(
      names = c("temperature", "rainfall", "wind"),
      beta= beta
    ),
    interactions = list(
      names = c("temperature:rainfall"),
      beta=matrix(c(0.1,-0.3),ncol=2,byrow=TRUE)
    ),
    residual = list(
      vcov= matrix(c(1,0.5,0.5,1),nrow=2,ncol=2,byrow=TRUE)
    )
  )
)

data <- get_population_data(squid_data)
head(data)
```

```
##            y1           y2 individual_effect1 individual_effect2 temperature
## 1  2.21050134  1.294281304          0.3738621          0.3130563   0.2474442
## 2  0.30671985  0.767288962          0.3738621          0.3130563   0.6267361
## 3 -0.08741854 -0.675319438          0.3738621          0.3130563   1.4255186
## 4  0.98283306 -0.002284301          0.3738621          0.3130563   0.8091150
## 5  2.78888855  1.180712862          0.3738621          0.3130563   0.1566717
## 6  1.59693774  0.290589001          0.3738621          0.3130563   1.0819382
##      rainfall        wind  residual1  residual2 temperature:rainfall individual
## 1  0.07576161  1.40470231  1.2744794  0.8064820           0.01874677          1
## 2  0.49027116 -0.02155445 -0.5028253  0.3878414           0.30727064          1
## 3 -0.68348827 -0.56325151 -0.7709343 -1.2302024          -0.97432525          1
## 4  0.92104163  0.76022751 -0.2823860 -0.4329146           0.74522861          1
## 5 -1.25575415  1.41897367  2.1818234  0.9022205          -0.19674116          1
## 6  0.95321639  2.08038728 -0.2357851 -0.2199468           1.03132125          1
##   squid_pop
## 1         1
## 2         1
## 3         1
## 4         1
## 5         1
## 6         1
```

```r
# library(MCMCglmm)
# mod <- MCMCglmm(cbind(y1,y2)~1,random=~us(trait):individual, rcov=~us(trait):units,data=data,family=rep("gaussian",2),verbose=FALSE)
# summary(mod)
```



## One response repeatedly measured, the other not
set the beta values for the trait that is unmeasured at a particular level to 0

The other way to do this is through sampling (link and example needed)


```r
individual <- list(
  vcov = matrix(c(
    1,0.5,
    0.5,1
    ),nrow=2,ncol=2,byrow=TRUE)
)

residual <- list(
  vcov = matrix(c(
    1,0.5,
    0.5,1
    ),nrow = 2,ncol = 2,byrow=TRUE),
  beta = matrix(c(
    1,0,
    0,0
    ),nrow = 2,ncol = 2,byrow=TRUE)
)

squid_data <- simulate_population(
  data_structure= make_structure(structure = "individual(100)",repeat_obs=20),
  n_response = 2,
  parameters=list(individual = individual, residual = residual)
)

data <- get_population_data(squid_data)
```


## Different distributions


```r
individual <- list(
  vcov = matrix(c(
    1,0.5,
    0.5,1
    ),nrow=2,ncol=2,byrow=TRUE)
)

residual <- list(
  vcov = matrix(c(
    1,0.5,
    0.5,1
    ),nrow = 2,ncol = 2,byrow=TRUE),
  beta = matrix(c(
    1,0,
    0,0
    ),nrow = 2,ncol = 2,byrow=TRUE)
)


squid_data <- simulate_population(
  data_structure= make_structure(structure = "individual(100)",repeat_obs=20),
  n_response = 2,
  parameters=list(individual = individual, residual = residual),
  family=c("gaussian","binomial"), link=c("identity","logit")
)

data <- get_population_data(squid_data)
head(data,20)
```

```
##            y1 y2 individual_effect1 individual_effect2  residual1   residual2
## 1  -1.6706436  0          -1.085769          -1.809539 -0.5848745 -1.10098411
## 2  -1.4666490  0          -1.085769          -1.809539 -0.3808800 -1.10359510
## 3  -0.7798335  0          -1.085769          -1.809539  0.3059355  0.89906527
## 4  -0.6755392  0          -1.085769          -1.809539  0.4102298  1.27144736
## 5  -1.4314630  0          -1.085769          -1.809539 -0.3456940  1.47579179
## 6  -2.0038855  0          -1.085769          -1.809539 -0.9181164 -1.71020015
## 7   0.3282533  0          -1.085769          -1.809539  1.4140223 -1.38338556
## 8  -0.8040910  0          -1.085769          -1.809539  0.2816781  1.12610799
## 9  -2.4921780  0          -1.085769          -1.809539 -1.4064089 -0.83539112
## 10  0.5833605  0          -1.085769          -1.809539  1.6691295  2.18371514
## 11 -1.6705681  0          -1.085769          -1.809539 -0.5847991  0.44656648
## 12 -2.0109248  0          -1.085769          -1.809539 -0.9251558 -1.22703322
## 13  0.1975017  0          -1.085769          -1.809539  1.2832708  1.63331381
## 14 -4.1406047  0          -1.085769          -1.809539 -3.0548357 -1.46211542
## 15 -0.7559031  0          -1.085769          -1.809539  0.3298660 -1.24415539
## 16 -0.1440253  0          -1.085769          -1.809539  0.9417437 -1.04997775
## 17 -0.1195880  0          -1.085769          -1.809539  0.9661811  1.52189122
## 18 -1.6872446  0          -1.085769          -1.809539 -0.6014755 -0.86684664
## 19 -0.7503544  0          -1.085769          -1.809539  0.3354147  0.02700792
## 20 -0.4113180  0          -1.085769          -1.809539  0.6744510  0.15798303
##    individual squid_pop
## 1           1         1
## 2           1         1
## 3           1         1
## 4           1         1
## 5           1         1
## 6           1         1
## 7           1         1
## 8           1         1
## 9           1         1
## 10          1         1
## 11          1         1
## 12          1         1
## 13          1         1
## 14          1         1
## 15          1         1
## 16          1         1
## 17          1         1
## 18          1         1
## 19          1         1
## 20          1         1
```

```r
data <- get_population_data(squid_data)
```


## Multivariate Random Slopes
Before reading this it is worth checking out how to simulate univariate random slopes in Section \@ref(randomslopes).

Here we have to think about the beta matrix. As we saw in an example above, in multivariate models beta can be thought of as switching on and off predictor variables for the response variables. We we can simulate 4 variables, an intercept and slope for each variable, and then use the beta matrix to tell `simulate_population` which response variable they link to 


```r
individual <- list(
  names = c("ind_int1","ind_slope1","ind_int2","ind_slope2"),
  vcov = matrix(c(
     1, 0.5, 0,  0, 
    0.5, 1,  0,  0,
     0,  0,  1, 0.2,
     0,  0, 0.2, 1
    ),nrow=4,ncol=4, byrow=TRUE),
  beta = matrix(c(
    1, 0,
    0, 0,
    0, 1,
    0, 0
    ),nrow = 4,ncol = 2, byrow=TRUE)

)

observation <- list(
  names="environment",
  beta=matrix(c(0.5,-0.3), ncol=2,byrow=TRUE)
)

residual <- list(
  vcov = matrix(c(
    1,0.5,
    0.5,1
    ),nrow = 2,ncol = 2,byrow=TRUE)
)

interactions <- list(
  names=c("ind_slope1:environment","ind_slope2:environment"),
  beta= matrix(c(
    1,0,
    0,1
    ), ncol=2,byrow=TRUE)
)

squid_data <- simulate_population(
  data_structure = make_structure(structure = "individual(100)",repeat_obs=20),
  n_response = 2,
  parameters=list(
    individual = individual, 
    observation = observation,
    residual = residual, 
    interactions = interactions
  )
)

data <- get_population_data(squid_data)
head(data,20)
```

```
##            y1          y2  ind_int1 ind_slope1  ind_int2 ind_slope2 environment
## 1  -2.8929785 -1.80978534 0.0588518   1.085178 0.1617523  0.5981975 -0.99200736
## 2  -1.3637066 -0.91976972 0.0588518   1.085178 0.1617523  0.5981975  0.41084465
## 3   2.9575947 -0.26204801 0.0588518   1.085178 0.1617523  0.5981975  1.24627785
## 4  -0.4513149 -0.36115761 0.0588518   1.085178 0.1617523  0.5981975 -0.07999263
## 5   1.4708316  1.54949605 0.0588518   1.085178 0.1617523  0.5981975  0.60692362
## 6   1.8250487 -0.18599499 0.0588518   1.085178 0.1617523  0.5981975  0.98080842
## 7  -1.0226457 -0.47901527 0.0588518   1.085178 0.1617523  0.5981975 -0.11309214
## 8   0.5126517  1.07530623 0.0588518   1.085178 0.1617523  0.5981975 -0.45103722
## 9   1.3306505 -1.36650396 0.0588518   1.085178 0.1617523  0.5981975  0.71480299
## 10 -2.3825997  0.05147684 0.0588518   1.085178 0.1617523  0.5981975 -2.54734658
## 11  2.5459297  0.34937111 0.0588518   1.085178 0.1617523  0.5981975  1.39594598
## 12  1.0908831 -1.83828245 0.0588518   1.085178 0.1617523  0.5981975  0.54874017
## 13 -0.1222957 -0.13303389 0.0588518   1.085178 0.1617523  0.5981975 -0.02931245
## 14  2.7255433  0.90984488 0.0588518   1.085178 0.1617523  0.5981975  1.82736314
## 15  0.4693991 -0.35557207 0.0588518   1.085178 0.1617523  0.5981975  0.03022781
## 16 -1.6923948  1.45885249 0.0588518   1.085178 0.1617523  0.5981975 -1.28777736
## 17 -2.3801511 -1.96969000 0.0588518   1.085178 0.1617523  0.5981975 -1.18116780
## 18  0.9086790  0.45014096 0.0588518   1.085178 0.1617523  0.5981975 -0.04292380
## 19  0.6281504  0.43513773 0.0588518   1.085178 0.1617523  0.5981975 -0.23437953
## 20  0.5003277  1.47204209 0.0588518   1.085178 0.1617523  0.5981975 -0.57500082
##     residual1  residual2 ind_slope1:environment ind_slope2:environment
## 1  -1.3793220 -1.6757235            -1.07650459            -0.59341631
## 2  -2.0738203 -1.2040348             0.44583958             0.24576623
## 3   0.9231706 -0.7954372             1.35243333             0.74552027
## 4  -0.3833642 -0.4990563            -0.08680624            -0.04785139
## 5   0.4498978  1.2067607             0.65862018             0.36306018
## 6   0.2114410 -0.6402219             1.06435175             0.58671713
## 7  -0.9022263 -0.6070438            -0.12272511            -0.06765143
## 8   1.1687742  1.0480521            -0.48945568            -0.26980933
## 9   0.1387087 -1.7414087             0.77568850             0.42759335
## 10  1.5965463  0.6493369            -2.76432453            -1.52381630
## 11  0.2742550 -0.2286488             1.51484990             0.83505137
## 12  0.1621804 -2.1636677             0.59548078             0.32825499
## 13 -0.1346820 -0.2860453            -0.03180923            -0.01753464
## 14 -0.2300044  0.2031775             1.98301433             1.09312403
## 15  0.3626309 -0.5263382             0.03280255             0.01808220
## 16  0.2901097  1.6811122            -1.39746770            -0.77034518
## 17 -0.5666417 -1.7792210            -1.28177734            -0.70657160
## 18  0.9178691  0.3011884            -0.04657996            -0.02567691
## 19  0.9408319  0.3432768            -0.25434351            -0.14020524
## 20  1.3529545  1.4817536            -0.62397825            -0.34396404
##    individual squid_pop
## 1           1         1
## 2           1         1
## 3           1         1
## 4           1         1
## 5           1         1
## 6           1         1
## 7           1         1
## 8           1         1
## 9           1         1
## 10          1         1
## 11          1         1
## 12          1         1
## 13          1         1
## 14          1         1
## 15          1         1
## 16          1         1
## 17          1         1
## 18          1         1
## 19          1         1
## 20          1         1
```
