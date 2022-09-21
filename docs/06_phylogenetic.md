# Phylogenetic Effects {#phylogenetic}

## Brownian motion



```r
library(ape)

data(bird.families)

squid_dat <- simulate_population(
	data_structure=data.frame(taxon=bird.families$tip.label),
	parameters=list(
		taxon=list(
			vcov=1
		),
		residual=list(
			vcov=1
		)
	),
	phylogeny=list(taxon=bird.families)
)

pop_dat <- get_population_data(squid_dat)
var(pop_dat$taxon_effect)
```

```
## [1] 0.7089991
```



```r
library(MCMCglmm)
Ainv<-inverseA(bird.families)$Ainv
prior<-list(
	R=list(V=1, nu=0.002), 
	G=list(G1=list(V=1, nu=0.002)))

model2<-MCMCglmm(y~1, 
	random=~taxon, 
	ginverse=list(taxon=Ainv),
	data=pop_dat, 
	prior=prior, 
	verbose=FALSE, nitt=13000, burnin=3000, thin=10)

summary(model2)
```

```
## 
##  Iterations = 3001:12991
##  Thinning interval  = 10
##  Sample size  = 1000 
## 
##  DIC: 447.5037 
## 
##  G-structure:  ~taxon
## 
##       post.mean l-95% CI u-95% CI eff.samp
## taxon    0.4609 0.001543    1.566    29.06
## 
##  R-structure:  ~units
## 
##       post.mean l-95% CI u-95% CI eff.samp
## units     1.412   0.5849    2.098    35.49
## 
##  Location effects: y ~ 1 
## 
##             post.mean l-95% CI u-95% CI eff.samp pMCMC
## (Intercept)   0.05707 -0.42173  0.57725    589.2  0.86
```


<br><br>



## Ornstein–Uhlenbeck Process
Coming Soon :D

<br><br>
