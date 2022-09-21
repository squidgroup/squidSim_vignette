
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
##         y1         y2 individual_effect1 individual_effect2  residual1
## 1 3.311552  2.1428013           2.816774           1.632711  0.4947778
## 2 3.634761  2.8725950           2.816774           1.632711  0.8179870
## 3 1.177740  0.6738357           2.816774           1.632711 -1.6390347
## 4 3.871525  1.9240478           2.816774           1.632711  1.0547505
## 5 1.097957 -0.7859113           2.816774           1.632711 -1.7188171
## 6 1.792108  0.6534280           2.816774           1.632711 -1.0246660
##    residual2 individual squid_pop
## 1  0.5100898          1         1
## 2  1.2398835          1         1
## 3 -0.9588758          1         1
## 4  0.2913363          1         1
## 5 -2.4186227          1         1
## 6 -0.9792834          1         1
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
## 1 -1.1912188 -1.4672467         -0.9065268         -0.7924739 -0.2846920
## 2 -1.1198922 -1.2097585         -0.9065268         -0.7924739 -0.2133654
## 3 -0.1218316 -0.5897121         -0.9065268         -0.7924739  0.7846952
## 4 -2.1473881 -2.1064087         -0.9065268         -0.7924739 -1.2408613
## 5 -1.3691517 -1.0671880         -0.9065268         -0.7924739 -0.4626250
## 6 -3.6086210 -2.4014834         -0.9065268         -0.7924739 -2.7020943
##    residual2 individual squid_pop
## 1 -0.6747728          1         1
## 2 -0.4172846          1         1
## 3  0.2027617          1         1
## 4 -1.3139349          1         1
## 5 -0.2747141          1         1
## 6 -1.6090095          1         1
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
##           y1        y2 individual_effect1 individual_effect2 temperature
## 1  1.3123930 3.9176599           1.218093           1.727094  -0.3936047
## 2  1.5063786 2.9603500           1.218093           1.727094   0.4341885
## 3  1.2283309 2.9737997           1.218093           1.727094  -0.3111164
## 4 -0.2230376 0.8114144           1.218093           1.727094   0.5354042
## 5  0.3029111 2.5795570           1.218093           1.727094  -1.4279655
## 6  2.6137652 3.8920016           1.218093           1.727094   1.7816647
##      rainfall        wind   residual1  residual2 individual squid_pop
## 1  0.92185218  0.09026687  0.07965174  2.0365295          1         1
## 2 -0.97528641 -0.45262573  0.40203625  1.4301573          1         1
## 3  1.35671403 -0.56833155  0.06495265  1.0633080          1         1
## 4 -0.09901259 -0.07879345 -1.66539227 -0.9415379          1         1
## 5 -0.16177004 -0.03072662 -0.15962727  1.0306865          1         1
## 6  0.48303329  0.89495200  0.13974750  1.8006395          1         1
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
## 1 -1.9316241  0.2477848         -0.1388063          0.7204855  -3.3837079
## 2 -0.2946881 -0.6047198         -0.1388063          0.7204855  -0.3449104
## 3  0.8287961  0.6863675         -0.1388063          0.7204855   1.3403073
## 4 -2.2738653  0.4264965         -0.1388063          0.7204855  -0.3834876
## 5  0.9317904  2.2926044         -0.1388063          0.7204855  -0.9141227
## 6 -0.6140320  1.6419038         -0.1388063          0.7204855  -1.0172216
##      rainfall        wind   residual1  residual2 temperature:rainfall
## 1  0.07978651  0.93831396 -0.37141783 -0.3251109           -0.2699742
## 2  0.69899635  0.25263653 -0.17490770 -1.5281045           -0.2410911
## 3  2.25214648 -0.18033533 -0.40073674  0.3050260            3.0185684
## 4  0.51504615 -1.46048801 -1.58842657 -0.2718549           -0.1975138
## 5 -0.72328991  0.07199624  1.58459966  1.9993422            0.6611758
## 6 -0.84339959  0.59105023 -0.06104245  1.3900926            0.8579243
##   individual squid_pop
## 1          1         1
## 2          1         1
## 3          1         1
## 4          1         1
## 5          1         1
## 6          1         1
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
##            y1 y2 individual_effect1 individual_effect2   residual1   residual2
## 1  -0.7278805  0          -0.690872         -0.2116948 -0.03700852 -0.42610474
## 2   0.1661384  1          -0.690872         -0.2116948  0.85701040 -0.38716251
## 3  -0.3594943  0          -0.690872         -0.2116948  0.33137772  0.85107001
## 4  -1.2378835  1          -0.690872         -0.2116948 -0.54701157 -0.26858748
## 5  -1.4074427  1          -0.690872         -0.2116948 -0.71657067 -0.05665809
## 6  -0.3629900  0          -0.690872         -0.2116948  0.32788193  1.69800608
## 7  -1.4146107  0          -0.690872         -0.2116948 -0.72373868  0.99612592
## 8  -0.9138361  0          -0.690872         -0.2116948 -0.22296408 -0.36951730
## 9  -1.2993065  0          -0.690872         -0.2116948 -0.60843449  0.12167817
## 10  0.8958694  0          -0.690872         -0.2116948  1.58674138 -1.30095579
## 11 -0.3261061  1          -0.690872         -0.2116948  0.36476587  0.57338443
## 12 -1.2851072  1          -0.690872         -0.2116948 -0.59423519  0.19471515
## 13 -0.7670021  0          -0.690872         -0.2116948 -0.07613011  0.77512157
## 14 -2.0767311  0          -0.690872         -0.2116948 -1.38585912 -0.82042427
## 15 -0.7983997  0          -0.690872         -0.2116948 -0.10752775 -0.71873339
## 16  0.5245768  0          -0.690872         -0.2116948  1.21544877  1.39568673
## 17  0.5452941  0          -0.690872         -0.2116948  1.23616606  0.82080336
## 18 -1.4456528  0          -0.690872         -0.2116948 -0.75478084 -0.02353251
## 19 -2.0964789  1          -0.690872         -0.2116948 -1.40560694 -1.32388442
## 20 -0.7722425  1          -0.690872         -0.2116948 -0.08137049  0.44855013
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
##             y1         y2   ind_int1 ind_slope1   ind_int2 ind_slope2
## 1   0.84118608  1.8306727 -0.3845981 -0.5680533 -0.1065674 -0.6074091
## 2   0.33472626  1.4139004 -0.3845981 -0.5680533 -0.1065674 -0.6074091
## 3   0.38913881  2.7633857 -0.3845981 -0.5680533 -0.1065674 -0.6074091
## 4  -0.94078200 -1.4182949 -0.3845981 -0.5680533 -0.1065674 -0.6074091
## 5   0.09328654 -0.4153381 -0.3845981 -0.5680533 -0.1065674 -0.6074091
## 6  -0.16933162  1.0030660 -0.3845981 -0.5680533 -0.1065674 -0.6074091
## 7   1.40926485  1.1629439 -0.3845981 -0.5680533 -0.1065674 -0.6074091
## 8  -1.20937471 -0.7301398 -0.3845981 -0.5680533 -0.1065674 -0.6074091
## 9  -1.69368913 -1.4048327 -0.3845981 -0.5680533 -0.1065674 -0.6074091
## 10 -0.21749435 -0.2907354 -0.3845981 -0.5680533 -0.1065674 -0.6074091
## 11  0.10497389  1.1122244 -0.3845981 -0.5680533 -0.1065674 -0.6074091
## 12 -1.03605804 -2.1202784 -0.3845981 -0.5680533 -0.1065674 -0.6074091
## 13  0.97451645 -0.1458392 -0.3845981 -0.5680533 -0.1065674 -0.6074091
## 14 -1.03916178  2.4097322 -0.3845981 -0.5680533 -0.1065674 -0.6074091
## 15  0.09807993  0.1336654 -0.3845981 -0.5680533 -0.1065674 -0.6074091
## 16 -0.62694627 -3.0011045 -0.3845981 -0.5680533 -0.1065674 -0.6074091
## 17  0.12454462  1.8067376 -0.3845981 -0.5680533 -0.1065674 -0.6074091
## 18 -0.41381856  1.1596434 -0.3845981 -0.5680533 -0.1065674 -0.6074091
## 19 -0.97357895  1.4093215 -0.3845981 -0.5680533 -0.1065674 -0.6074091
## 20 -0.40725440  1.8627581 -0.3845981 -0.5680533 -0.1065674 -0.6074091
##    environment  residual1    residual2 ind_slope1:environment
## 1   0.08568956  1.2316156  2.014995544            -0.04867624
## 2  -1.07838585  0.6459366  0.541930674             0.61258067
## 3   0.18444823  0.7862892  3.037323122            -0.10477643
## 4   0.32160160 -0.5342979 -1.019903282            -0.18268686
## 5   1.65225937  0.5903264  1.190504441            -0.93857143
## 6  -0.24618154  0.1985130  0.886246049             0.13984424
## 7  -0.25818063  1.7762929  1.035235864             0.14666037
## 8   1.53955491 -0.7200048  0.773433691            -0.87454928
## 9  -1.27236780 -1.3956799 -2.452823386             0.72277276
## 10  0.21154614  0.1815001  0.007790914            -0.12016949
## 11  0.32342140  0.5115819  1.512267347            -0.18372060
## 12  1.49020052 -0.5500469 -0.661489536            -0.84651336
## 13  0.76572031  1.4112243  0.655549749            -0.43496997
## 14 -1.92148031 -0.7853268  0.772730910             1.09150328
## 15 -0.97507590  0.4163208 -0.644559902             0.55389511
## 16  0.91681103 -0.1799562 -2.062614504            -0.52079756
## 17 -0.22268548  0.4939882  1.711238166             0.12649723
## 18 -1.16018263 -0.1081748  0.213450574             0.65904560
## 19 -1.22281525 -0.6721975  0.406295223             0.69462427
## 20 -1.76115606 -0.1425089  0.371236551             1.00043056
##    ind_slope2:environment individual squid_pop
## 1             -0.05204862          1         1
## 2              0.65502134          1         1
## 3             -0.11203553          1         1
## 4             -0.19534373          1         1
## 5             -1.00359732          1         1
## 6              0.14953290          1         1
## 7              0.15682126          1         1
## 8             -0.93513961          1         1
## 9              0.77284774          1         1
## 10            -0.12849504          1         1
## 11            -0.19644909          1         1
## 12            -0.90516131          1         1
## 13            -0.46510546          1         1
## 14             1.16712456          1         1
## 15             0.59226995          1         1
## 16            -0.55687933          1         1
## 17             0.13526118          1         1
## 18             0.70470545          1         1
## 19             0.74274907          1         1
## 20             1.06974216          1         1
```
