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
