library(squidSim)
	sim_dat<-simulate_population(
		data_structure = make_structure(paste0("individual(100) + age(50)")),
		response_names = "mortality",
		parameters = list(
			intercept= 0,
			residual=list(vcov=0)
		),
		family= "binomial",
		link="probit", 
		sample_type='survival',
		sample_param=list(y = "mortality",ID = "individual",age ="age",death=1,all=TRUE
)
		)
head(get_population_data(sim_dat),100)
get_sample_data(sim_dat)
head(get_sample_data(sim_dat),20)


ds <- make_structure(paste0("birth_year(10)/individual(10) + age(50)"))
ds$year <- ds$age + ds$birth_year

	sim_dat<-simulate_population(
		data_structure = ds,
		n_response=2,
		response_names = c("survival","recapture"),
		parameters = list(
			intercept= c(0.5,0),
			# age=list(covariate=TRUE,  beta=matrix(c(-0.2,0),ncol=2)),
			residual=list(vcov=c(0,0))
		),
		family= "binomial",
		link="probit", 
		sample_type='survival',
		sample_param=list(y = "survival",ID = "individual",age ="age",death=0,all=TRUE
)
)

	head(get_sample_data(sim_dat),20)


dat<-get_sample_data(sim_dat)
recapture_dat <- subset(dat, recapture==1)

head(recapture_dat)

table(dat$survival,dat$age)


plot(individual~year,dat, cex=1, pch=19, col="grey")
points(individual~year,recapture_dat, cex=0.5, pch=19, col="red")
for(i in unique(recapture_dat$individual) ){
	
	lines( individual~year, recapture_dat, subset= individual==i, col="red")	
}





##### species occurrence with spatial autocorrelation 



library(nlme)
library(squidSim)

ds <- make_structure("location(2500)")
locations <- matrix(1:2500,nrow=50,ncol=50)

locations2<-as.data.frame(t(sapply(1:2500,function(x)which(locations==x, arr.ind=TRUE))))
colnames(locations2) <- c("x","y")


cs1Exp <- corExp(1, form = ~ x + y)
cs1Exp <- Initialize(cs1Exp, locations2)
spM<- corMatrix(cs1Exp)
colnames(spM) <- rownames(spM) <- 1:2500
spM[1:10,1:10]
spm_c <- chol(spM)
 spm_c2 <- methods::as(spM, "dgCMatrix")
system.time({x1<-chol(spm_c)})
system.time({x2<-chol(spm_c2)})

x<-rnorm(2500)

Matrix::crossprod(x1,x)

sim_dat<-simulate_population(
	data_structure = ds,
	n_response=2,
	response_names = c("occurrence","observation"),
	parameters = list(
		intercept= c(-1,0),
		location = list(vcov=3),
		# age=list(covariate=TRUE,  beta=matrix(c(-0.2,0),ncol=2)),
		residual=list(vcov=c(0,0))
	),
	cov_str = list(location=spM),
	family= "binomial",
	link="probit"
)
dat<-get_population_data(sim_dat)

dat$seen <- dat$occurrence* dat$observation
plot(y~x,locations2, pch=19, col=dat$occurrence)
points(y~x,locations2[dat$seen==1,], pch=19, col="red", cex=0.5)


set.seed(26)

sim_dat<-simulate_population(
	data_structure = ds,
	n_response=2,
	response_names = c("occurrence","observation"),
	parameters = list(
		intercept= c(-3,0),
		location = list(vcov=c(4,0)),
		# age=list(covariate=TRUE,  beta=matrix(c(-0.2,0),ncol=2)),
		residual=list(vcov=c(0,0))
	),
	cov_str = list(location=spM),
	family= "binomial",
	link="probit"
)
dat<-get_population_data(sim_dat)
dat$seen <- dat$occurrence* dat$observation




sim_dat_R<-simulate_population(
	data_structure = ds,
	n_response=2,
	response_names = c("occurrence","observation"),
	parameters = list(
		intercept= c(-3,0),
		residual=list(vcov=c(4,0))
	),
	family= "binomial",
	link="probit"
)
dat_R<-get_population_data(sim_dat_R)
dat_R$seen <- dat_R$occurrence* dat_R$observation

par(mfrow=c(1,2))
plot(y~x,locations2, pch=19, col=dat$occurrence)
points(y~x,locations2[dat$seen==1,], pch=19, col="yellow", cex=0.5)

plot(y~x,locations2, pch=19, col=dat_R$occurrence)
points(y~x,locations2[dat_R$seen==1,], pch=19, col="yellow", cex=0.5)