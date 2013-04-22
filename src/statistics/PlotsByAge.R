require(ggplot2)
require(survival)
options(scipen=100)

analysis_dir <- "~/git/OhlohAnalytics/analysis/amerish/"

monthlyFactsByYear <- read.csv(file=paste(analysis_dir,"allYearlyFacts.csv",sep="/"),header=TRUE,sep=",")
projectDeath <- read.csv(file=paste(analysis_dir, "projectDeathStatus.csv",sep="/"),header=TRUE,sep=",")

### Plot of monthly growth facts aggregated by year

	plotObj <- qplot(age,prod_loc_growth_factor,data=monthlyFactsByYear, group=age,geom="boxplot", xlab="Project age in years", log="y", 
		ylab="Yearly LOC growth factor")
	ggsave(file=paste(analysis_dir,"age-yearlygrowth.pdf",sep="/"),plot=plotObj)

### Plot of monthly commits summed by year
	plotObj <- qplot(age,sum_commits,data=monthlyFactsByYear, group=age,geom="boxplot", xlab="Project age in years", log="y",
		ylab="Yearly sum of commits")
	ggsave(file=paste(analysis_dir,"age-yearlycommits.pdf",sep="/"),plot=plotObj)

### Plot of project survival curve
	survivalCurve <- survfit(Surv(age,status) ~ 1, data=projectDeath)
	pdf(file=paste(analysis_dir,"age-survival.pdf",sep="/"))
	plot(survivalCurve, xlab="Project age in years", ylab="Project survival")
	dev.off()
	

	


