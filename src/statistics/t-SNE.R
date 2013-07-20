analysis_dir <- "~/git/OhlohAnalytics/analysis/"

monthlyFactsByYear <- read.csv(file=paste(analysis_dir,"allYearlyFacts.csv",sep="/"),header=TRUE,sep=",")
monthlyFacts       <- read.csv(file=paste(analysis_dir,"allMonthlyFacts.csv",sep="/"),header=TRUE,sep=",")

write.csv(subset(monthlyFactsByYear,
                 subset=year==2012,
                 select=c(sum_commits, median_contributors, max_loc_total,age)),
          file="~/git/OhlohAnalytics/analysis/tSNE-projects-2012.csv")

write.csv(subset(monthlyFactsByYear,
                 subset=projectName=="firefox",
                 select=c(sum_commits, median_contributors, max_loc_total)),
          file="~/git/OhlohAnalytics/analysis/tSNE-project-years-firefox.csv")

write.csv(subset(monthlyFacts,
                 subset=projectName=="firefox",
                 select=c(commits, contributors, loc_total, loc_added, loc_deleted)),
          file="~/git/OhlohAnalytics/analysis/tSNE-project-months-firefox.csv")

selectedProjects=c("firefox","apache","gnome","gcc","python")
write.csv(scale(subset(monthlyFacts,
                       subset=projectName %in% selectedProjects,
                       select=c(commits, contributors, abs_loc_growth, loc_total)),
                center=FALSE),
          file="~/git/OhlohAnalytics/analysis/tSNE-project-months-multi.csv")

### TODO Call t-SNE here...

tSNE.result <- read.csv("~/git/OhlohAnalytics/output/tSNE-result.csv", header=F)

df <- cbind(subset(monthlyFacts, 
                   subset=projectName %in% selectedProjects,
                   select=projectName),
            tSNE.result)

plotObj <- ggplot(df,aes(x=V1,y=V2)) + geom_point(aes(colour=df$projectName))
