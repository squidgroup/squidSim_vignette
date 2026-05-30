# Phylogenetic Effects {#phylogenetic}




## Brownian motion

We can simulate some data using the bird families phylogeny from the {ape} package. Here we have a grouping variables called taxa in the data structure, which links to the phylogeny. We can then specify the phylogenetic variance.


``` r
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
```

Run model to check 

``` r
library(MCMCglmm)

Ainv<-inverseA(bird.families)$Ainv
prior<-list(
	R=list(V=1, nu=0.002), 
	G=list(G1=list(V=1, nu=0.002)))

model1<-MCMCglmm(y~1, 
	random=~taxon, 
	ginverse=list(taxon=Ainv),
	data=pop_dat, 
	prior=prior, 
	verbose=FALSE, nitt=13000, burnin=3000, thin=10)

summary(model1)
```

```
## 
##  Iterations = 3001:12991
##  Thinning interval  = 10
##  Sample size  = 1000 
## 
##  DIC: 420.2962 
## 
##  G-structure:  ~taxon
## 
##       post.mean l-95% CI u-95% CI eff.samp
## taxon     1.108    0.209    2.469    60.44
## 
##  R-structure:  ~units
## 
##       post.mean l-95% CI u-95% CI eff.samp
## units     1.034   0.1187    1.637    82.43
## 
##  Location effects: y ~ 1 
## 
##             post.mean l-95% CI u-95% CI eff.samp pMCMC
## (Intercept)    0.0956  -0.5763   0.7307    986.9 0.736
```
