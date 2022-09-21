
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
##            y1         y2 individual_effect1 individual_effect2   residual1
## 1  0.23251007 -0.8672451          0.7582864         -0.5334644 -0.52577637
## 2  0.83987472 -2.8463141          0.7582864         -0.5334644  0.08158827
## 3 -0.04688398  0.5168086          0.7582864         -0.5334644 -0.80517043
## 4  1.74460797 -0.7498519          0.7582864         -0.5334644  0.98632152
## 5  1.34106659 -0.6937312          0.7582864         -0.5334644  0.58278014
## 6 -0.28332345 -0.4575241          0.7582864         -0.5334644 -1.04160990
##    residual2 individual squid_pop
## 1 -0.3337806          1         1
## 2 -2.3128497          1         1
## 3  1.0502730          1         1
## 4 -0.2163875          1         1
## 5 -0.1602668          1         1
## 6  0.0759403          1         1
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
##            y1         y2 individual_effect1 individual_effect2  residual1
## 1  1.06540952  0.2956750          0.8156289         0.04419247  0.2497806
## 2  1.22929304  1.2797233          0.8156289         0.04419247  0.4136641
## 3 -0.06176212 -0.3769881          0.8156289         0.04419247 -0.8773910
## 4 -0.91778344 -0.9197119          0.8156289         0.04419247 -1.7334124
## 5  1.55319851 -0.1491969          0.8156289         0.04419247  0.7375696
## 6  0.68008489 -1.3484642          0.8156289         0.04419247 -0.1355440
##    residual2 individual squid_pop
## 1  0.2514825          1         1
## 2  1.2355308          1         1
## 3 -0.4211806          1         1
## 4 -0.9639044          1         1
## 5 -0.1933894          1         1
## 6 -1.3926567          1         1
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
##           y1          y2 individual_effect1 individual_effect2 temperature
## 1  0.8975492 -0.52045257          0.5980344         -0.8973196   0.1951066
## 2 -0.9914160 -0.87637661          0.5980344         -0.8973196  -0.6517151
## 3 -0.2100449  0.07352046          0.5980344         -0.8973196  -1.6922604
## 4  0.7503495 -2.17351925          0.5980344         -0.8973196   0.1014024
## 5 -1.0182325 -1.76260461          0.5980344         -0.8973196  -0.2250660
## 6  2.3044040  1.45807180          0.5980344         -0.8973196   0.2176160
##    rainfall       wind  residual1  residual2 individual squid_pop
## 1  0.193413 -1.2032780  0.5242623  0.4390016          1         1
## 2 -2.302576 -0.5597763 -0.6351447  0.6026074          1         1
## 3 -1.692443 -0.3181061  0.4719713  1.5103654          1         1
## 4 -1.129455  0.4584987  0.1899554 -1.1062987          1         1
## 5  1.732611 -1.0754340 -1.5276259 -1.0817572          1         1
## 6  1.006365 -1.3186860  1.7918943  2.2642253          1         1
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
##           y1         y2 individual_effect1 individual_effect2 temperature
## 1  0.8027661  0.1261918          0.9577513          0.8616709  -0.6385837
## 2  0.4145257  0.8147683          0.9577513          0.8616709   1.1434945
## 3 -0.4559415  0.7627055          0.9577513          0.8616709   0.6285537
## 4  1.3125145 -0.5262009          0.9577513          0.8616709   0.1003357
## 5  1.1422422  3.4363049          0.9577513          0.8616709  -0.9751368
## 6  1.2074861  1.2465607          0.9577513          0.8616709   0.2260829
##     rainfall       wind   residual1   residual2 temperature:rainfall individual
## 1  0.2401262 -0.7159226  0.34639219 -0.69405591          -0.15334070          1
## 2  0.7565383 -1.0097542 -1.04986399  0.04794497           0.86509738          1
## 3 -0.2115915 -0.2016374 -1.61186048 -0.13923764          -0.13299664          1
## 4  0.1831123 -0.3425352  0.36889607 -1.39476248           0.01837271          1
## 5  1.0195708  1.7284781  0.04902379  1.99711944          -0.99422094          1
## 6 -0.3551342  0.5077738  0.06341696  0.35844406          -0.08028977          1
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
##             y1 y2 individual_effect1 individual_effect2  residual1  residual2
## 1  -0.41318841  0           1.227749         -0.6211506 -1.6409373 -1.3064994
## 2   1.54329456  0           1.227749         -0.6211506  0.3155456 -0.1063386
## 3   3.22560228  1           1.227749         -0.6211506  1.9978534  1.1126366
## 4   2.14514439  1           1.227749         -0.6211506  0.9173955  0.4736353
## 5   2.20005168  0           1.227749         -0.6211506  0.9723028  1.4729377
## 6  -0.20654444  0           1.227749         -0.6211506 -1.4342934 -0.9413551
## 7   2.05115085  0           1.227749         -0.6211506  0.8234019  0.8541014
## 8   2.11847849  1           1.227749         -0.6211506  0.8907296 -1.1626690
## 9   0.05524614  1           1.227749         -0.6211506 -1.1725028 -1.3948301
## 10  0.77663108  0           1.227749         -0.6211506 -0.4511178  0.1795693
## 11  1.45898637  0           1.227749         -0.6211506  0.2312374 -0.7151491
## 12 -0.12438940  0           1.227749         -0.6211506 -1.3521383 -0.9697208
## 13  0.61539024  1           1.227749         -0.6211506 -0.6123587 -0.4695595
## 14  1.41775595  1           1.227749         -0.6211506  0.1900070 -0.8023823
## 15  0.74899770  1           1.227749         -0.6211506 -0.4787512 -1.0179344
## 16  1.73401760  0           1.227749         -0.6211506  0.5062687 -0.1617221
## 17  1.98394939  0           1.227749         -0.6211506  0.7562005  0.2604367
## 18  0.53540243  0           1.227749         -0.6211506 -0.6923465 -1.6023605
## 19  2.77209719  1           1.227749         -0.6211506  1.5443483  1.2121889
## 20  1.07292774  0           1.227749         -0.6211506 -0.1548212  2.1417684
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
##            y1          y2   ind_int1 ind_slope1  ind_int2 ind_slope2
## 1  -1.1087873 -2.63864429 -0.8117948 -0.7047831 -0.876019 -0.4087823
## 2  -0.8197928 -1.18567890 -0.8117948 -0.7047831 -0.876019 -0.4087823
## 3  -1.4668273 -1.88488274 -0.8117948 -0.7047831 -0.876019 -0.4087823
## 4   0.5702188 -0.09297932 -0.8117948 -0.7047831 -0.876019 -0.4087823
## 5   0.7892671  3.39799411 -0.8117948 -0.7047831 -0.876019 -0.4087823
## 6  -1.3690632 -2.60880232 -0.8117948 -0.7047831 -0.876019 -0.4087823
## 7  -0.8022851 -1.59302778 -0.8117948 -0.7047831 -0.876019 -0.4087823
## 8  -0.6993757 -1.97398525 -0.8117948 -0.7047831 -0.876019 -0.4087823
## 9  -0.2259698 -0.33922429 -0.8117948 -0.7047831 -0.876019 -0.4087823
## 10 -1.7827531 -2.69335759 -0.8117948 -0.7047831 -0.876019 -0.4087823
## 11 -0.9152705 -2.26754678 -0.8117948 -0.7047831 -0.876019 -0.4087823
## 12 -0.2500843 -3.55031714 -0.8117948 -0.7047831 -0.876019 -0.4087823
## 13 -0.5525026 -3.01883292 -0.8117948 -0.7047831 -0.876019 -0.4087823
## 14 -1.9586781 -0.75957050 -0.8117948 -0.7047831 -0.876019 -0.4087823
## 15  0.3856926 -0.90488833 -0.8117948 -0.7047831 -0.876019 -0.4087823
## 16 -1.1265425 -1.70162421 -0.8117948 -0.7047831 -0.876019 -0.4087823
## 17 -2.5577329 -0.45715906 -0.8117948 -0.7047831 -0.876019 -0.4087823
## 18 -1.5943084 -1.22256177 -0.8117948 -0.7047831 -0.876019 -0.4087823
## 19 -1.2213812 -1.93512603 -0.8117948 -0.7047831 -0.876019 -0.4087823
## 20 -0.8538758 -2.92433269 -0.8117948 -0.7047831 -0.876019 -0.4087823
##    environment    residual1   residual2 ind_slope1:environment
## 1   0.61008928 -0.172056539 -1.33020481            -0.42998063
## 2  -0.37342846 -0.084469886 -0.57433937             0.26318608
## 3   0.56072300 -0.540205890 -0.61143322            -0.39518810
## 4  -1.13112902  1.150377427 -0.01868451             0.79720064
## 5  -1.69078899  1.254816832  3.07561184             1.19163955
## 6   0.27518179 -0.500915877 -1.53773934            -0.19394348
## 7  -0.08358573 -0.007607294 -0.77625287             0.05890981
## 8  -0.36018740  0.038658739 -1.35326069             0.25385400
## 9  -0.39042121  0.505873329  0.26007108             0.27516228
## 10  1.44156362 -0.675750456 -0.79558385            -1.01598971
## 11  0.01528645 -0.100345316 -1.38069301            -0.01077364
## 12  2.08896281  0.989494835 -1.19367833            -1.47226574
## 13  1.14294338  0.493347679 -1.33271590            -0.80552721
## 14 -0.24553300 -1.197164309 -0.05758094             0.17304751
## 15  0.70583248  1.342029947  0.47141222            -0.49745882
## 16  0.57860636 -0.196258941 -0.41549927            -0.40779200
## 17 -0.12955278 -1.772468334  0.32703523             0.09130661
## 18 -0.67287894 -0.920307819 -0.82346742             0.47423372
## 19  0.55228817 -0.296487096 -0.66765496            -0.38924338
## 20  0.39096255  0.037981523 -1.77120636            -0.27554381
##    ind_slope2:environment individual squid_pop
## 1            -0.249393684          1         1
## 2             0.152650936          1         1
## 3            -0.229213621          1         1
## 4             0.462385490          1         1
## 5             0.691164568          1         1
## 6            -0.112489437          1         1
## 7             0.034168365          1         1
## 8             0.147238222          1         1
## 9             0.159597268          1         1
## 10           -0.589285655          1         1
## 11           -0.006248832          1         1
## 12           -0.853930966          1         1
## 13           -0.467214994          1         1
## 14            0.100369537          1         1
## 15           -0.288531804          1         1
## 16           -0.236524023          1         1
## 17            0.052958879          1         1
## 18            0.275060981          1         1
## 19           -0.225765612          1         1
## 20           -0.159818559          1         1
```
