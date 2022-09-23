
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
```

We haven't specified any names (of responses or predictors). By default, `simulate_population()` will add a number to the default name to indicate which reponse variable it refers to, so here we have `y1` and `y2` for the response variable, and `individual_effect1` and `individual_effect2` etc.


```r
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

We can name the response variables easily, by giving the `response_name` argument a vector of names


```r
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
##    body_mass  behaviour individual_effect1 individual_effect2  residual1
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


```r
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


```r
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
## 1  0.2629858 -0.0780353       -0.005446858         -0.2658786   0.3738621
## 2  1.0829243  1.1550551       -0.005446858         -0.2658786  -0.8473002
## 3 -0.2310409  0.3175489       -0.005446858         -0.2658786  -0.6371071
## 4  1.7982021 -0.2832260       -0.005446858         -0.2658786   0.5207047
## 5 -0.1813950  0.1731702       -0.005446858         -0.2658786   1.1298068
## 6  1.5139838  0.8386942       -0.005446858         -0.2658786   1.5284323
##     rainfall       wind  residual1 residual2 individual squid_pop
## 1  0.1456369 -0.6502334  0.2474442 0.1893336          1         1
## 2 -0.9514215  0.9920109  1.4047023 1.2451206          1         1
## 3 -1.2060802 -0.5203187  0.4902712 0.2264689          1         1
## 4  0.3514403  0.1582997  1.4255186 0.1208411          1         1
## 5 -0.5530579 -0.2232949 -0.5632515 0.4190884          1         1
## 6 -0.6243242 -0.1365409  0.9210416 1.1188972          1         1
```

```r
# library(MCMCglmm)
# mod <- MCMCglmm(cbind(y1,y2)~1,random=~us(trait):individual, rcov=~us(trait):units,data=data,family=rep("gaussian",2),verbose=FALSE)
# summary(mod)
```


Equally if we want an interaction, we now have to expand the size of what we give to `beta`, with one $\beta$ for each response, in a matrix, with $q$ columns. 


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
##           y1         y2 individual_effect1 individual_effect2 temperature
## 1  2.8739089  2.7223659           1.274479           0.806482   2.5349210
## 2 -0.2195532  0.4901810           1.274479           0.806482  -0.3163310
## 3  2.1650067  0.2206076           1.274479           0.806482   0.3833268
## 4 -0.7894193 -1.4047362           1.274479           0.806482  -0.9681842
## 5  2.8398480  0.9040718           1.274479           0.806482  -0.1220015
## 6  1.0100711  0.5755157           1.274479           0.806482  -0.9008461
##      rainfall        wind  residual1  residual2 temperature:rainfall individual
## 1 -0.49522796  2.29401469 -0.1316535  1.9231227          -1.25536379          1
## 2 -1.19507319 -0.80430268 -0.8933655 -0.5539674           0.37803876          1
## 3 -0.60617083  0.09328987  0.8153473 -0.7291554          -0.23236153          1
## 4  0.05970686  0.66797804 -1.7863607 -2.2466396          -0.05780724          1
## 5  1.32784401  1.37169437  0.9654921  0.4395282          -0.16199893          1
## 6  2.65030643  0.06492591 -0.1247725 -0.5007525          -2.38751816          1
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

In some circumstances we might want to simulate two responses, one that varies between measurements and one that doesn't. For example we might have one fixed measurement of body size for each individual, and repeated measurements of behaviour. We can simply set the variance of the singly measured variable to 0 at that particular level. So for this example:


```r
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
##            y1 y2 individual_effect1 individual_effect2     residual1
## 1  0.80376835  1           1.933232           1.132927 -1.129464e+00
## 2  0.11432330  1           1.933232           1.132927 -1.818909e+00
## 3  2.04598297  1           1.933232           1.132927  1.127508e-01
## 4  1.93333124  1           1.933232           1.132927  9.902713e-05
## 5  1.95105609  1           1.933232           1.132927  1.782388e-02
## 6  2.56116516  1           1.933232           1.132927  6.279329e-01
## 7  3.32733216  1           1.933232           1.132927  1.394100e+00
## 8  1.47309415  1           1.933232           1.132927 -4.601381e-01
## 9  0.86767664  1           1.933232           1.132927 -1.065556e+00
## 10 2.58031042  1           1.933232           1.132927  6.470782e-01
## 11 2.71157185  1           1.933232           1.132927  7.783396e-01
## 12 0.42173186  1           1.933232           1.132927 -1.511500e+00
## 13 1.65539512  1           1.933232           1.132927 -2.778371e-01
## 14 2.48173738  1           1.933232           1.132927  5.485052e-01
## 15 0.07352694  0           1.933232           1.132927 -1.859705e+00
## 16 0.27413919  1           1.933232           1.132927 -1.659093e+00
## 17 2.31435706  1           1.933232           1.132927  3.811248e-01
## 18 1.86984706  1           1.933232           1.132927 -6.338515e-02
## 19 2.01284413  0           1.933232           1.132927  7.961192e-02
## 20 2.26003535  1           1.933232           1.132927  3.268031e-01
##      residual2 individual squid_pop
## 1  -0.76705492          1         1
## 2  -0.66569912          1         1
## 3  -2.24392564          1         1
## 4   0.64642188          1         1
## 5  -0.17289871          1         1
## 6  -0.22721373          1         1
## 7   1.77025265          1         1
## 8  -0.06036272          1         1
## 9  -0.13107580          1         1
## 10  0.51965440          1         1
## 11 -0.94775391          1         1
## 12  0.21561112          1         1
## 13 -0.80144601          1         1
## 14 -0.76227263          1         1
## 15 -0.63847377          1         1
## 16 -2.33136433          1         1
## 17  0.10744440          1         1
## 18  1.32416729          1         1
## 19 -0.57956449          1         1
## 20 -1.10879960          1         1
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
##             y1         y2  ind_int1 ind_slope1 ind_int2 ind_slope2 environment
## 1  -1.07284118 -0.7063502 -1.379322  -1.675724 -2.07382 -0.6038441  0.27241358
## 2  -2.73615771 -3.8998721 -1.379322  -1.675724 -2.07382 -0.6038441  1.88695292
## 3   2.30907268 -0.1346602 -1.379322  -1.675724 -2.07382 -0.6038441 -2.48201511
## 4  -0.59162679 -1.9301263 -1.379322  -1.675724 -2.07382 -0.6038441 -0.52459897
## 5  -1.79334628 -1.7390556 -1.379322  -1.675724 -2.07382 -0.6038441  0.17781662
## 6   0.37926761 -1.1070232 -1.379322  -1.675724 -2.07382 -0.6038441 -0.62796804
## 7  -3.66798525 -3.1329732 -1.379322  -1.675724 -2.07382 -0.6038441  1.42127867
## 8  -1.60121532 -3.5016071 -1.379322  -1.675724 -2.07382 -0.6038441  0.73788631
## 9  -0.04876558 -0.9818298 -1.379322  -1.675724 -2.07382 -0.6038441  0.60326386
## 10  0.15490674 -1.6868303 -1.379322  -1.675724 -2.07382 -0.6038441 -0.42497852
## 11 -0.24363724  0.4803048 -1.379322  -1.675724 -2.07382 -0.6038441  0.20271543
## 12 -0.82656505  1.7013557 -1.379322  -1.675724 -2.07382 -0.6038441 -0.32382136
## 13  0.44331446 -0.4691145 -1.379322  -1.675724 -2.07382 -0.6038441 -1.15975731
## 14  1.87942657  0.4287303 -1.379322  -1.675724 -2.07382 -0.6038441 -1.76811131
## 15 -1.98410710 -2.4175230 -1.379322  -1.675724 -2.07382 -0.6038441  0.28958932
## 16 -1.12883705 -0.6458835 -1.379322  -1.675724 -2.07382 -0.6038441 -0.11048588
## 17 -2.10790515 -2.9632071 -1.379322  -1.675724 -2.07382 -0.6038441  1.15822172
## 18  1.16587270 -0.9111980 -1.379322  -1.675724 -2.07382 -0.6038441 -1.37427364
## 19 -0.87193964 -2.8776114 -1.379322  -1.675724 -2.07382 -0.6038441 -0.03008455
## 20 -0.21801425 -2.0496166 -1.379322  -1.675724 -2.07382 -0.6038441 -0.10592694
##     residual1    residual2 ind_slope1:environment ind_slope2:environment
## 1   0.6267639  1.613689536            -0.45648984            -0.16449532
## 2   0.8616993 -0.120540569            -3.16201141            -1.13942532
## 3   0.7702311 -0.304194525             4.15917112             1.49875008
## 4   0.1709119 -0.330461646             0.87908283             0.31677597
## 5  -0.2049611  0.495483184            -0.29797150            -0.10737351
## 6   1.0202728  0.399211998             1.05230082             0.37919477
## 7  -0.6176324  0.225461374            -2.38167012            -0.85823069
## 8   0.6456570 -0.760852634            -1.23649346            -0.44556827
## 9   2.0398280  1.637247029            -1.01090344            -0.36427730
## 10  1.0345715  0.002875761             0.71214650             0.25662075
## 11  1.3740221  2.737348294            -0.33969501            -0.12240851
## 12  0.1720326  3.482491999             0.54263507             0.19553761
## 13  0.4590825  0.556466028             1.94343262             0.70031257
## 14  1.1799385  0.904453719             2.96286573             1.06766351
## 15 -0.2643081 -0.081959097            -0.48527165            -0.17486679
## 16  0.1205841  1.328074778             0.18514379             0.06671624
## 17  0.6331654  0.157465033            -1.94085938            -0.69938530
## 18  0.9294289 -0.079506715             2.30290268             0.82984698
## 19  0.4720113 -0.830982832             0.05041339             0.01816638
## 20  1.0367670 -0.071537731             0.17750427             0.06396335
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
