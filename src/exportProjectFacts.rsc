module exportProjectFacts

import processProjectFacts;
import projectFactsRepository;
import Logging;
import lang::csv::IO;
import IO;
import analyzeProjectDeath;
import analyzeProjectMetaData;

public loc OutputFilesDirectory = |project://OhlohAnalytics/output|;

public void exportFactsForAllProjects() {
	logToConsole("exportFactsForAllProject", "Getting Ohloh facts for all projects from cache...");
	monthlyFactsMap OhlohFacts=getOhlohFactsFromCache();
	
	logToConsole("exportFactsForAllProject", "Getting monthly growth facts for all projects from cache...");
	monthlyFactsMap allMonthlyFacts=getMonthlyFactsFromCache(OhlohFacts);
	logToConsole("exportFactsForAllProject", "Exporting all monthly facts to CSV for all projects: " + "allMonthlyFacts.csv");
	writeFactsToCSV(convertMonthlyFactsMapToRel(allMonthlyFacts),"allMonthlyFacts.csv");
	
	logToConsole("exportFactsForAllProject", "Getting yearly growth facts for all projects from cache...");
	yearlyFactsMap allYearlyFacts=getYearlyFactsFromCache(allMonthlyFacts);
	logToConsole("exportFactsForAllProject", "Exporting all yearly facts to CSV for all projects: " + "allYearlyFacts.csv");
	writeFactsToCSV(convertYearlyFactsMapToRel(allYearlyFacts),"allYearlyFacts.csv");
	
	logToConsole("exportFactsForAllProject", "Calculating project activity for all projects...");
	projectActivityStats=getProjectActivityStatus(allYearlyFacts);
	logToConsole("exportFactsForAllProject", "Calculating project death events for all projects...");
	projectDeathStats=getProjectDeathStatus(projectActivityStats, "2012");
	logToConsole("exportFactsForAllProject", "Exporting project death events CSV for all projects: " + "projectDeathStatus.csv");
	writeFactsToCSV(projectDeathStats,"projectDeathStatus.csv");
}

public void doProjectMetaDataAnalyses () {
	logToConsole("doProjectMetaDataAnalyses", "Analyzing project tags for all projects...");
	OAL=generateOALForTags(getProjectNamesInRepository());
	writeValueToFile(OAL,"projects-tags.oal");
}

public void exportRepositoryFacts () {
	logToConsole("exportRepositoryFacts", "Exporting repository facts for all projects...");
	repoFacts=getRepositoryFactsForProjects(getProjectNamesInRepository());
	writeFactsToCSV(repoFacts,"projectRepositoryFacts.csv");
}


public void writeFactsToCSV(facts,str fileName) {
	writeCSV(facts,
			 OutputFilesDirectory + fileName,
			 ("separator" : ","));
}

public void writeValueToFile(v, str fileName) {
	writeFile(OutputFilesDirectory + fileName, v);
}

public rel [str projectName,
			 str year,
			 str month,
			 num loc_added,
		     num loc_deleted,
		     num commits,
		     num contributors,
		     num loc_total,
		     num abs_loc_growth,
		     num loc_growth_factor]
convertMonthlyFactsMapToRel (monthlyFactsMap monthlyFacts)
{
	return
	{
	    <projectName,
		 year,
		 month,
		 loc_added,
	     loc_deleted,
	     commits,
	     contributors,
	     loc_total,
	     abs_loc_growth,
	     loc_growth_factor>
	|
	key <- monthlyFacts,
	<str projectName, str year, str month> := key,
	loc_added_fact(num loc_added)           		<- monthlyFacts[key],
    loc_deleted_fact(num loc_deleted) 				<- monthlyFacts[key],
    commits_fact(num commits) 						<- monthlyFacts[key],
    contributors_fact(num contributors) 			<- monthlyFacts[key],
    loc_total_fact(num loc_total) 					<- monthlyFacts[key],
    abs_loc_growth_fact(num abs_loc_growth) 		<- monthlyFacts[key],
    loc_growth_factor_fact(num loc_growth_factor) 	<- monthlyFacts[key]
    };
}

public rel [str projectName,
			 str year,
			 str month,
			 num sum_loc_added,
		     num sum_loc_deleted,
		     num sum_commits,
		     num median_contributors,
		     num max_loc_total,
		     num sum_abs_loc_growth,
		     num prod_loc_growth_factor,
		     num age]
convertYearlyFactsMapToRel (yearlyFactsMap yearlyFacts)
{
	return
	{
	    <projectName,
		 year,
		 month,
		 sum_loc_added,
	     sum_loc_deleted,
	     sum_commits,
	     median_contributors,
	     max_loc_total,
	     sum_abs_loc_growth,
	     prod_loc_growth_factor < 0.0000001 ? 0 : prod_loc_growth_factor,
	     age>
	|
	key <- yearlyFacts,
	<str projectName, str year, str month> := key,
	sum_loc_added_fact(num sum_loc_added)           		 <- yearlyFacts[key],
    sum_loc_deleted_fact(num sum_loc_deleted) 				 <- yearlyFacts[key],
    sum_commits_fact(num sum_commits) 						 <- yearlyFacts[key],
    median_contributors_fact(num median_contributors) 		 <- yearlyFacts[key],
    max_loc_total_fact(num max_loc_total) 					 <- yearlyFacts[key],
    sum_abs_loc_growth_fact(num sum_abs_loc_growth) 		 <- yearlyFacts[key],
    prod_loc_growth_factor_fact(num prod_loc_growth_factor)  <- yearlyFacts[key],
    age_fact(num age) 										 <- yearlyFacts[key]
    };
}
