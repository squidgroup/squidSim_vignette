
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
##          y1       y2 individual_effect1 individual_effect2  residual1
## 1 1.3133200 1.831474          0.9999917           1.672215  0.3133283
## 2 1.4933566 2.707715          0.9999917           1.672215  0.4933649
## 3 1.1901784 3.159186          0.9999917           1.672215  0.1901867
## 4 0.2093253 1.562007          0.9999917           1.672215 -0.7906664
## 5 0.5472856 1.420850          0.9999917           1.672215 -0.4527061
## 6 1.1650743 1.194041          0.9999917           1.672215  0.1650826
##    residual2 individual squid_pop
## 1  0.1592599          1         1
## 2  1.0355001          1         1
## 3  1.4869719          1         1
## 4 -0.1102071          1         1
## 5 -0.2513645          1         1
## 6 -0.4781738          1         1
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
## 1  0.7860301  1.4830731          0.5656931         -0.1647357  0.2203370
## 2  0.9468484 -0.5945455          0.5656931         -0.1647357  0.3811553
## 3 -0.7602386 -1.4316041          0.5656931         -0.1647357 -1.3259318
## 4 -0.8063715 -1.7463591          0.5656931         -0.1647357 -1.3720647
## 5  0.8857932  0.4190894          0.5656931         -0.1647357  0.3201001
## 6 -0.1697151 -1.5077783          0.5656931         -0.1647357 -0.7354082
##    residual2 individual squid_pop
## 1  1.6478088          1         1
## 2 -0.4298097          1         1
## 3 -1.2668684          1         1
## 4 -1.5816234          1         1
## 5  0.5838251          1         1
## 6 -1.3430426          1         1
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
##          y1          y2 individual_effect1 individual_effect2 temperature
## 1 0.7580569  1.25601297          0.7979486          0.2588204   1.4291640
## 2 2.0246337  3.19222558          0.7979486          0.2588204   1.0101226
## 3 0.4763081  0.30660600          0.7979486          0.2588204   0.8463863
## 4 1.6544667  0.05353070          0.7979486          0.2588204   1.4564746
## 5 0.7883421 -0.01774183          0.7979486          0.2588204   0.5312947
## 6 1.8097151  1.51278590          0.7979486          0.2588204   0.3981529
##     rainfall        wind  residual1   residual2 individual squid_pop
## 1  0.2732416 -0.70674920 -0.5970973  0.87030278          1         1
## 2 -0.4268472  0.43214875  0.6773486  2.87454750          1         1
## 3 -1.4506394 -1.00005703 -0.1546887  0.35328057          1         1
## 4 -1.6448246 -0.09532097  0.4858420 -0.01244012          1         1
## 5  1.3537390  0.90330170 -0.8169922 -0.69076965          1         1
## 6  0.1190761 -1.03608429  1.0997001  1.29394344          1         1
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
##           y1          y2 individual_effect1 individual_effect2 temperature
## 1  1.1509807 -0.38977639          0.3589435          0.2103867  -0.2549145
## 2 -0.7232010 -0.29153870          0.3589435          0.2103867  -1.5692621
## 3 -1.0465802  0.19839427          0.3589435          0.2103867   0.1213757
## 4  0.7510183  0.85391748          0.3589435          0.2103867   0.1250863
## 5  1.6285123  0.47388662          0.3589435          0.2103867  -0.6020571
## 6 -0.8023283  0.09197145          0.3589435          0.2103867  -1.3428842
##     rainfall      wind  residual1  residual2 temperature:rainfall individual
## 1 -0.2468928 1.8813640  0.3981701 -0.6945485           0.06293656          1
## 2  0.1863782 0.3979466 -0.4249254 -0.5098123          -0.29247620          1
## 3 -1.9680761 1.1906064 -1.4058906  0.1787616          -0.23887660          1
## 4  0.5577643 1.1041828 -0.1202530  0.4299816           0.06976865          1
## 5  1.0402778 1.2063723  1.0632607 -0.1928791          -0.62630661          1
## 6 -0.3544173 1.0306178 -0.7757257  0.1264773           0.47594139          1
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
##             y1 y2 individual_effect1 individual_effect2   residual1  residual2
## 1   1.61928491  0           0.089718         -0.0319013  1.52956691 -0.8320741
## 2   0.07115411  1           0.089718         -0.0319013 -0.01856389 -0.3423347
## 3  -0.38236721  1           0.089718         -0.0319013 -0.47208521 -0.1958661
## 4  -0.35181603  1           0.089718         -0.0319013 -0.44153403 -0.1002483
## 5   0.49072247  1           0.089718         -0.0319013  0.40100447 -0.2131839
## 6   0.24993480  1           0.089718         -0.0319013  0.16021680 -0.5755633
## 7  -2.11716246  0           0.089718         -0.0319013 -2.20688046 -0.6703496
## 8  -2.12512582  0           0.089718         -0.0319013 -2.21484382 -2.3029096
## 9   0.20257401  1           0.089718         -0.0319013  0.11285601  0.1454820
## 10 -1.70056613  0           0.089718         -0.0319013 -1.79028413 -1.0506684
## 11 -0.43735808  0           0.089718         -0.0319013 -0.52707608 -1.5176008
## 12 -1.11461414  0           0.089718         -0.0319013 -1.20433214 -1.1724473
## 13 -0.07459436  0           0.089718         -0.0319013 -0.16431237  2.2018673
## 14  0.44286088  0           0.089718         -0.0319013  0.35314288 -1.4699753
## 15  0.20694715  0           0.089718         -0.0319013  0.11722915  0.6657859
## 16  0.13846332  1           0.089718         -0.0319013  0.04874531 -1.2186944
## 17 -0.38020964  0           0.089718         -0.0319013 -0.46992764  1.1685338
## 18  0.15653474  1           0.089718         -0.0319013  0.06681674 -1.1453769
## 19  0.64819966  0           0.089718         -0.0319013  0.55848166  0.8638714
## 20 -0.32038550  1           0.089718         -0.0319013 -0.41010351 -0.8966339
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
##             y1          y2  ind_int1 ind_slope1  ind_int2 ind_slope2
## 1   2.10908521  2.82484116 0.4005273  -1.483505 -0.349434  0.3888367
## 2  -1.92469228 -1.49877371 0.4005273  -1.483505 -0.349434  0.3888367
## 3   0.90314469 -0.56264891 0.4005273  -1.483505 -0.349434  0.3888367
## 4   0.71311148  0.65765499 0.4005273  -1.483505 -0.349434  0.3888367
## 5   1.01981061  0.81555410 0.4005273  -1.483505 -0.349434  0.3888367
## 6   1.28421742  0.16515248 0.4005273  -1.483505 -0.349434  0.3888367
## 7  -0.47198829 -0.56862767 0.4005273  -1.483505 -0.349434  0.3888367
## 8   2.27331984 -1.85865032 0.4005273  -1.483505 -0.349434  0.3888367
## 9  -0.47732345 -2.26966001 0.4005273  -1.483505 -0.349434  0.3888367
## 10  0.72088336 -0.38107515 0.4005273  -1.483505 -0.349434  0.3888367
## 11 -1.38882832 -1.26562238 0.4005273  -1.483505 -0.349434  0.3888367
## 12  0.52946494 -1.40224530 0.4005273  -1.483505 -0.349434  0.3888367
## 13  0.08367009 -0.20141657 0.4005273  -1.483505 -0.349434  0.3888367
## 14  1.08867842  0.07940446 0.4005273  -1.483505 -0.349434  0.3888367
## 15  1.38380990  1.18076389 0.4005273  -1.483505 -0.349434  0.3888367
## 16 -3.15959089 -0.85680594 0.4005273  -1.483505 -0.349434  0.3888367
## 17  2.24422839  0.16445849 0.4005273  -1.483505 -0.349434  0.3888367
## 18 -0.07611679 -1.38782358 0.4005273  -1.483505 -0.349434  0.3888367
## 19  0.15208118  0.01746564 0.4005273  -1.483505 -0.349434  0.3888367
## 20  0.19995076  1.28643302 0.4005273  -1.483505 -0.349434  0.3888367
##    environment   residual1   residual2 ind_slope1:environment
## 1   -0.4958353  1.22090129  3.21832354              0.7355743
## 2    0.6966670 -1.64004398 -1.21122929             -1.0335092
## 3   -1.0079991 -0.48875494 -0.12366760              1.4953718
## 4    0.1995385  0.50883129  0.98936266             -0.2960164
## 5   -0.5164499  0.11135215  1.21086781              0.7661561
## 6   -0.8239544  0.07332672  0.58778387              1.2223406
## 7    1.6112150  0.71212267 -0.36232866             -2.3902458
## 8   -3.1740939 -1.24894515 -1.22724033              4.7087846
## 9    0.6569260 -0.23176071 -1.97858513             -0.9745530
## 10   0.2848209  0.60047887 -0.05694369             -0.4225333
## 11   0.2036347 -1.58907984 -0.93427861             -0.3020932
## 12  -0.5286589 -0.39100117 -1.00584698              0.7842682
## 13   0.2324551 -0.08823641  0.12736690             -0.3448484
## 14   0.3342087  1.01684709  0.39914847             -0.4958004
## 15   0.2568707  1.23591627  1.50737835             -0.3810691
## 16   1.9014223 -1.69005955 -0.67628799             -2.8207698
## 17  -0.6112606  1.24252314  0.56819486              0.9068082
## 18  -0.5266065 -0.99456429 -0.99160760              0.7812234
## 19   0.2122586 -0.03968872  0.34804330             -0.3148867
## 20   0.2336260  0.02919583  1.61511246             -0.3465854
##    ind_slope2:environment individual squid_pop
## 1             -0.19279896          1         1
## 2              0.27088970          1         1
## 3             -0.39194702          1         1
## 4              0.07758789          1         1
## 5             -0.20081466          1         1
## 6             -0.32038369          1         1
## 7              0.62649951          1         1
## 8             -1.23420414          1         1
## 9              0.25543691          1         1
## 10             0.11074882          1         1
## 11             0.07918065          1         1
## 12            -0.20556199          1         1
## 13             0.09038709          1         1
## 14             0.12995261          1         1
## 15             0.09988077          1         1
## 16             0.73934276          1         1
## 17            -0.23768053          1         1
## 18            -0.20476391          1         1
## 19             0.08253393          1         1
## 20             0.09084237          1         1
```
