require(ggplot2)
require(survival)
options(scipen=100)

analysis_dir <- "~/git/OhlohAnalytics/analysis/amerish/"

monthlyFactsByYear <- read.csv(file=paste(analysis_dir,"allYearlyFacts.csv",sep="/"),header=TRUE,sep=",")
projectDeath <- read.csv(file=paste(analysis_dir, "projectDeathStatus.csv",sep="/"),header=TRUE,sep=",")

### Plot of project ages in 2012
projectAgesIn2012 <- subset(monthlyFactsByYear,year==2012,select=age)
pdf(file=paste(analysis_dir,"age-hist.pdf",sep="/"))
hist(projectAgesIn2012$age, xlab="Project age in years",
     ylab="Number of occurrences", 
     main=paste("Histogram of the age of", length(projectAgesIn2012$age), "projects in 2012", sep=" " ),
     freq=TRUE,
     breaks=40)
dev.off()

### Plot of monthly growth facts aggregated by year

	plotObj <- qplot(age,prod_loc_growth_factor,data=monthlyFactsByYear, group=age,geom="boxplot", log="y",
	             xlab="Project age in years",                    
		           ylab="Yearly LOC growth factor",
	             main=paste("Boxplots of yearly growth factor at a project's age, for",length(monthlyFactsByYear$projectName),"project years",sep=" "))
	ggsave(file=paste(analysis_dir,"age-yearlygrowth.pdf",sep="/"),plot=plotObj)

### Plot of monthly commits summed by year
	plotObj <- qplot(age,sum_commits,data=monthlyFactsByYear, group=age,geom="boxplot", log="y",
    xlab="Project age in years",
		ylab="Yearly sum of commits",
	  main=paste("Boxplots of yearly number of commits at a project's age, for",length(monthlyFactsByYear$projectName),"project years",sep=" "))
	ggsave(file=paste(analysis_dir,"age-yearlycommits.pdf",sep="/"),plot=plotObj)

### Plot of project survival curve
	survivalCurve <- survfit(Surv(age,status) ~ 1, data=projectDeath)
	pdf(file=paste(analysis_dir,"age-survival.pdf",sep="/"))
	plot(survivalCurve, xlab="Project age in years", ylab="Project survival",
       main=paste("Kaplan-Meier survival of",length(projectDeath$projectName),"projects",sep=" ")
	)
	dev.off()
	
### Plot of project ages at death
  ageAtDeath <- subset(projectDeath, status==2, select=age)
  pdf(file=paste(analysis_dir,"age-death-hist.pdf",sep="/"))
  hist(ageAtDeath$age, xlab="Project age at death in years",
                       xlim=c(0,40),
                       ylab="Number of occurrences", 
                       main=paste("Histogram of the age at death of", length(ageAtDeath$age), "projects", sep=" " ),
                       freq=TRUE,
                       breaks=20)
  dev.off()

	


