require(ggplot2)
options(scipen=100)

analysis_dir <- "~/git/OhlohAnalytics/analysis/amerish/"

monthlyFactsByYear <- read.csv(file=paste(analysis_dir,"allYearlyFacts.csv",sep="/"),header=TRUE,sep=",")

p1 <- qplot(age,prod_loc_growth_factor,data=monthlyFactsByYear, group=age,geom="boxplot", xlab="Project age in years", log="y", ylab="Yearly LOC growth factor")
ggsave(file=paste(analysis_dir,"age-growthfactor.pdf",sep="/"),plot=p1)

p2 <- qplot(age,sum_commits,data=monthlyFactsByYear, group=age,geom="boxplot", xlab="Project age in years", log="y", ylab="Yearly sum of commits")
ggsave(file=paste(analysis_dir,"age-commits.pdf",sep="/"),plot=p2)





