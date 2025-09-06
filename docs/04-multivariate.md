
# Multi-response Models {#multivariate}

We can simulate multiple response variables, that covary at different hierarchical levels. For example, we might have two phenotypes, body mass and behaviour, that covary at the between individual and within individual (residual) levels. In the case of such a simple random effects model, we have a covariance matrix at each level:

<!-- Anne: perhaps add a short sentence of introduction here such as “We simulate here no longer one phenotype y varying across I individuals, but a number of phenotypes that (co)vary across individuals..”
 -->

$$
\boldsymbol{y}_{ij} = \boldsymbol{\beta}_0 + \boldsymbol{u}_j + \boldsymbol{\epsilon}_{ij}
$$
$$
\boldsymbol{u}_i \sim \mathcal{N}(0, \Sigma_u)
$$
$$
\boldsymbol{\epsilon}_{i} \sim \mathcal{N}(0, \Sigma_{\epsilon})
$$
$$
\Sigma_u = \begin{bmatrix}
\sigma^2_{u_1} & \sigma_{u_1u_2} \\
\sigma_{u_1u_2} & \sigma^2_{u_2}
\end{bmatrix}
,
\Sigma_{\epsilon} = \begin{bmatrix}
\sigma^2_{\epsilon_1} & \sigma_{\epsilon_1\epsilon_2} \\
\sigma_{\epsilon_1\epsilon_2} & \sigma^2_{\epsilon_2}
\end{bmatrix}
$$

We can indicate that there are multiple phenotypes within the parameter list using the `n_response` argument. If we have $q$ response variables, `vcov` now needs to either be a vector of length $q$ (the variances, if we assume the covariances are 0) or a $q*q$ covariance matrix. So below, we simulate 2 response variables, with a covariance between them at both individual and residual levels. 


``` r
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
```

We haven't specified any names (of responses or predictors). By default, `simulate_population()` will add a number to the default name to indicate which reponse variable it refers to, so here we have `y1` and `y2` for the response variable, and `individual_effect1` and `individual_effect2` etc.


``` r
data <- get_population_data(squid_data)
head(data)
```

```
##          y1        y2 individual_effect1 individual_effect2   residual1
## 1 1.1054644 1.4127080           2.480034           1.634281 -1.37456980
## 2 4.1766734 0.8345915           2.480034           1.634281  1.69663917
## 3 1.7790364 0.5872531           2.480034           1.634281 -0.70099782
## 4 0.7353167 0.5515726           2.480034           1.634281 -1.74471750
## 5 2.4911567 0.9464126           2.480034           1.634281  0.01112246
## 6 5.1880032 3.3071899           2.480034           1.634281  2.70796900
##    residual2 individual squid_pop
## 1 -0.2215732          1         1
## 2 -0.7996898          1         1
## 3 -1.0470282          1         1
## 4 -1.0827086          1         1
## 5 -0.6878687          1         1
## 6  1.6729086          1         1
```

We can name the response variables easily, by giving the `response_name` argument a vector of names


``` r
squid_data <- simulate_population(
  data_structure=make_structure(structure = "individual(100)",repeat_obs=10),
  n_response = 2,
  response_name = c("body_mass","behaviour"),
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
##     body_mass  behaviour individual_effect1 individual_effect2   residual1
## 1 -0.45333155 -2.3819621         -0.5478939        -0.09634067  0.09456232
## 2 -0.43114173 -0.6964292         -0.5478939        -0.09634067  0.11675214
## 3 -2.57715631 -0.9151838         -0.5478939        -0.09634067 -2.02926243
## 4 -0.09299664 -0.1446033         -0.5478939        -0.09634067  0.45489723
## 5 -0.92695565  1.2149131         -0.5478939        -0.09634067 -0.37906178
## 6 -1.99656829 -0.6476084         -0.5478939        -0.09634067 -1.44867441
##     residual2 individual squid_pop
## 1 -2.28562139          1         1
## 2 -0.60008852          1         1
## 3 -0.81884311          1         1
## 4 -0.04826266          1         1
## 5  1.31125377          1         1
## 6 -0.55126775          1         1
```


## Predictors affecting multiple responses

If we look a little at what `simulate_population()` assumes underneath with the formulation above (just random effects), we can understand more how we simulate predictors that affect multiple response variable. In the above code, we are essentially simulating a predictor for each trait (`individual_effect1` and `individual_effect2`) with some covariance between them.

We can expand the equation above:

$$\boldsymbol{y}_{ij} = \boldsymbol{\beta}_0 + \boldsymbol{u}_j B_u + \boldsymbol{\epsilon}_{ij}B_{\epsilon}$$
$$
B_u = \begin{bmatrix}
1 & 0 \\
0 & 1
\end{bmatrix}
,
B_{\epsilon} = \begin{bmatrix}
1 & 0 \\
0 & 1
\end{bmatrix}
$$

to include matrices of $\beta$s, $B$, the columns of which refer to the response variable, and the rows predictors. In this case they are all identity matrices, which essentially controlling which predictors affects which response ($u_1$ affects $y_1$ but not $y_2$ and vice versa). Internally, `simulate_population()` does the same thing, and assigns `beta` as an identity matrix. 


``` r
squid_data <- simulate_population(
  data_structure= make_structure(structure = "individual(100)",repeat_obs=10),
  n_response=2,
  response_name = c("body_mass","behaviour"),
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
```

How is this helpful? Well now we could simulate one (or many) predictor(s) that both responses. In the form of an equation we could have

$$\boldsymbol{y}_{ij} = \boldsymbol{\beta}_0 + \boldsymbol{x}_{i} B_x + \boldsymbol{u}_j + \boldsymbol{\epsilon}_{ij}$$

$$\boldsymbol{x}_i \sim \mathcal{N}(\boldsymbol{\mu}_x, \Sigma_x)$$

$$\boldsymbol{u}_i \sim \mathcal{N}(0, \Sigma_u)$$

$$\boldsymbol{\epsilon}_{i} \sim \mathcal{N}(0, \Sigma_{\epsilon})$$

where $B$ is a $p*q$ matrix, where $p$ is number of predictors. So if we returned to our original example in Section \@ref(linearmod), we have three predictors at the observation level - temperature, rainfall and wind. So $B_x$ would be a `3*2` matrix, with 3 predictors and two responses. 



<!-- 
https://stackoverflow.com/questions/63007496/how-to-create-an-editable-matrix-in-shiny-app
make little shiny app that allows you to enter diagonal and 

 -->


``` r
Beta <- matrix(c(
  0.5, -0.1,
  0.2, -0.2,
  0.3, -0.1
  ),nrow=3,ncol=2,byrow=TRUE)
Beta
```

```
##      [,1] [,2]
## [1,]  0.5 -0.1
## [2,]  0.2 -0.2
## [3,]  0.3 -0.1
```

So here, the environment variables all positively affect body mass (response 1) and negatively affect behaviour (response 2). This then slots easily into our code.


``` r
squid_data <- simulate_population(
  data_structure= make_structure(structure = "individual(100)",repeat_obs=20),
  n_response=2,
  parameters= list(
    individual = list(
      vcov =matrix(c(1,0.5,0.5,1),nrow=2,ncol=2,byrow=TRUE)
    ),
    observation = list(
      names = c("temperature", "rainfall", "wind"),
      beta= Beta
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
## 1  0.2981452 -1.3155845          -0.311252          -0.897568  -1.1453264
## 2 -0.6873794 -0.6087159          -0.311252          -0.897568  -0.8578652
## 3  0.5970893 -0.6079079          -0.311252          -0.897568  -0.2261574
## 4 -1.4274647 -1.3401883          -0.311252          -0.897568   1.8213830
## 5  1.5506350 -0.6644989          -0.311252          -0.897568   1.1993774
## 6 -0.3039953 -1.9789316          -0.311252          -0.897568  -1.2945405
##    rainfall       wind  residual1  residual2 individual squid_pop
## 1 0.5882671  1.1457965  0.7206679 -0.3003161          1         1
## 2 0.2144061 -0.5558841  0.1766892  0.1903584          1         1
## 3 1.6741909  0.5743441  0.5142786  0.6593169          1         1
## 4 0.2989377 -0.8591160 -1.8289570 -0.2866060          1         1
## 5 0.4251985 -0.2197198  1.2430745  0.4160745          1         1
## 6 1.1467957  1.0675309  0.1049086 -0.8747054          1         1
```

``` r
# library(MCMCglmm)
# mod <- MCMCglmm(cbind(y1,y2)~1,random=~us(trait):individual, rcov=~us(trait):units,data=data,family=rep("gaussian",2),verbose=FALSE)
# summary(mod)
```


Equally if we want an interaction, we now have to expand the size of what we give to `beta`, with one $\beta$ for each response, in a matrix, with $q$ columns. 


``` r
squid_data <- simulate_population(
  data_structure= make_structure(structure = "individual(100)",repeat_obs=20),
  n_response=2,
  
  parameters= list(
    individual = list(
      vcov =matrix(c(1,0.5,0.5,1),nrow=2,ncol=2,byrow=TRUE)
    ),
    observation = list(
      names = c("temperature", "rainfall", "wind"),
      beta= Beta
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
## 1 -1.4752163 -0.53377659         -0.2872822         -0.6655179  0.03214917
## 2  0.1289671 -0.83203811         -0.2872822         -0.6655179 -0.08320398
## 3  0.7591382 -0.04450337         -0.2872822         -0.6655179 -0.28697095
## 4  2.1132225 -1.23146725         -0.2872822         -0.6655179  1.92678408
## 5 -0.8876520 -1.06185793         -0.2872822         -0.6655179  0.40335533
## 6  0.5234630 -0.94372291         -0.2872822         -0.6655179  0.55847261
##     rainfall       wind  residual1   residual2 temperature:rainfall individual
## 1 -0.4787747 -0.4815074 -0.9622622 -0.01356718          -0.01539221          1
## 2  0.5679376  0.2375696  0.2777184 -0.05167259          -0.04725466          1
## 3 -0.4349687 -0.8358394  1.5151691  0.45918672           0.12482338          1
## 4 -0.1022663  0.1218843  1.4407051 -0.44064937          -0.19704515          1
## 5  0.9295396 -0.7546885 -0.7990423 -0.13308504           0.37493476          1
## 6 -1.2836502 -1.3774485  1.2731618 -0.83189771          -0.71688346          1
##   squid_pop
## 1         1
## 2         1
## 3         1
## 4         1
## 5         1
## 6         1
```

``` r
# library(MCMCglmm)
# mod <- MCMCglmm(cbind(y1,y2)~1,random=~us(trait):individual, rcov=~us(trait):units,data=data,family=rep("gaussian",2),verbose=FALSE)
# summary(mod)
```



## One response repeatedly measured, the other not

In some circumstances we might want to simulate two responses, one that varies between measurements and one that doesn't. For example we might have one fixed measurement of body size for each individual, and repeated measurements of behaviour. We can simply set the variance of the singly measured variable to 0 at that particular level. So for this example:


``` r
squid_data <- simulate_population(
  data_structure= make_structure(structure = "individual(100)",repeat_obs=20),
  n_response = 2,
  parameters=list(
    individual = list(
      vcov = matrix(c(
        1,0.5,
        0.5,1
        ),nrow=2,ncol=2,byrow=TRUE)
      ), 
    residual = list(
      vcov = c(0.8,0)
    )
  )
)

data <- get_population_data(squid_data)
```


## Different distributions


``` r
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
##             y1 y2 individual_effect1 individual_effect2   residual1   residual2
## 1   0.71929104  0         -0.5257881          -1.042722  1.24507910  2.50335447
## 2   0.33325030  1         -0.5257881          -1.042722  0.85903836  1.44428517
## 3  -0.42718617  0         -0.5257881          -1.042722  0.09860189  1.26752630
## 4   0.44910069  0         -0.5257881          -1.042722  0.97488875  0.39978044
## 5  -0.62462727  0         -0.5257881          -1.042722 -0.09883921  0.73676976
## 6   0.12822742  0         -0.5257881          -1.042722  0.65401547  2.32798502
## 7  -0.10269091  0         -0.5257881          -1.042722  0.42309715  0.97088795
## 8  -1.16521217  0         -0.5257881          -1.042722 -0.63942411 -1.11697997
## 9  -2.02498920  0         -0.5257881          -1.042722 -1.49920114 -1.71662806
## 10  0.84702556  1         -0.5257881          -1.042722  1.37281362  0.31558774
## 11 -1.13218028  0         -0.5257881          -1.042722 -0.60639222  0.50095716
## 12 -1.01004461  0         -0.5257881          -1.042722 -0.48425655  0.92650060
## 13  0.71616352  0         -0.5257881          -1.042722  1.24195158 -1.25932342
## 14 -0.72239979  1         -0.5257881          -1.042722 -0.19661173 -0.51083678
## 15 -1.33642792  1         -0.5257881          -1.042722 -0.81063987 -1.35618916
## 16 -1.95735773  0         -0.5257881          -1.042722 -1.43156967 -1.13993143
## 17 -0.85504490  0         -0.5257881          -1.042722 -0.32925684  0.03603065
## 18  1.26558426  0         -0.5257881          -1.042722  1.79137232  2.14512837
## 19  0.04558226  0         -0.5257881          -1.042722  0.57137032  0.52313819
## 20  0.88752414  1         -0.5257881          -1.042722  1.41331220  0.87626903
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

``` r
data <- get_population_data(squid_data)
```


## Multivariate Random Slopes
Before reading this it is worth checking out how to simulate univariate random slopes in Section \@ref(randomslopes).

Here we have to think about the beta matrix. As we saw in an example above, in multivariate models beta can be thought of as switching on and off predictor variables for the response variables. We we can simulate 4 variables, an intercept and slope for each variable, and then use the beta matrix to tell `simulate_population` which response variable they link to 


``` r
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
##            y1         y2  ind_int1 ind_slope1 ind_int2 ind_slope2 environment
## 1   5.9645704  2.9137755 -2.077368  -3.278942  1.25763 0.05766287 -2.85212863
## 2   1.0398840  1.6374762 -2.077368  -3.278942  1.25763 0.05766287 -1.17738236
## 3  -0.4401833  0.7571278 -2.077368  -3.278942  1.25763 0.05766287 -0.35861753
## 4  -3.1023023  0.3740181 -2.077368  -3.278942  1.25763 0.05766287  0.56941572
## 5   1.0271930  0.5355074 -2.077368  -3.278942  1.25763 0.05766287 -1.19029314
## 6  -4.6348419  2.6277686 -2.077368  -3.278942  1.25763 0.05766287  1.19495085
## 7  -2.9226107  1.9356115 -2.077368  -3.278942  1.25763 0.05766287 -0.23240054
## 8  -5.4887265  0.5051600 -2.077368  -3.278942  1.25763 0.05766287  1.26443122
## 9  -3.3095597  3.3006845 -2.077368  -3.278942  1.25763 0.05766287  1.37812284
## 10  2.3956917  1.9750491 -2.077368  -3.278942  1.25763 0.05766287 -1.66791032
## 11 -3.0158890 -0.1913344 -2.077368  -3.278942  1.25763 0.05766287  0.24947043
## 12 -2.5032658 -0.5935247 -2.077368  -3.278942  1.25763 0.05766287  0.09862542
## 13 -4.2034435 -0.7978834 -2.077368  -3.278942  1.25763 0.05766287  0.08779494
## 14 -7.1099119 -1.9438330 -2.077368  -3.278942  1.25763 0.05766287  1.50568552
## 15 -5.1929793  2.0242915 -2.077368  -3.278942  1.25763 0.05766287  1.23433537
## 16 -3.8947619  1.6117452 -2.077368  -3.278942  1.25763 0.05766287  0.31679603
## 17 -0.9437016  0.8657072 -2.077368  -3.278942  1.25763 0.05766287 -0.70152517
## 18 -0.8672693 -0.8645813 -2.077368  -3.278942  1.25763 0.05766287 -0.91219027
## 19 -2.5933184  1.1027425 -2.077368  -3.278942  1.25763 0.05766287  0.30478046
## 20  4.0778526  2.4288065 -2.077368  -3.278942  1.25763 0.05766287 -1.76307523
##     residual1   residual2 ind_slope1:environment ind_slope2:environment
## 1   0.1160375  0.96496923              9.3519652           -0.164461926
## 2  -0.1546256  0.09452311              3.8605689           -0.067891248
## 3   0.6406074 -0.58740809              1.1758862           -0.020678916
## 4   0.5574392 -0.74562093             -1.8670813            0.032834145
## 5  -0.2031949 -1.01057441              3.9029025           -0.068635720
## 6   0.7632257  1.65972001             -3.9181749            0.068904297
## 7  -1.4910703  0.62166269              0.7620280           -0.013400883
## 8   0.1024230 -0.44605095             -4.1459970            0.072910735
## 9   2.5975322  2.37702528             -4.5187853            0.079466520
## 10 -0.1619668  0.31322288              5.4689817           -0.096176498
## 11 -0.2452570 -1.38850807             -0.8179992            0.014385182
## 12 -0.1518234 -1.82725366             -0.3233871            0.005687025
## 13 -1.8820984 -2.03423703             -0.2878745            0.005062508
## 14 -0.8483306 -2.83657912             -4.9370559            0.086822150
## 15  0.3145355  1.06578725             -4.0473145            0.071175322
## 16 -0.9370360  0.43088702             -1.0387559            0.018267369
## 17 -0.8158315 -0.56192798              2.3002606           -0.040451956
## 18 -1.3248253 -2.34326848              2.9910193           -0.052599510
## 19  0.3310170 -0.08102745             -0.9993575            0.017574516
## 20  1.2557363  0.74391836              5.7810220           -0.101663980
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
