module exportProjectFacts

import processProjectFacts;
import projectFactsRepository;
import Logging;
import lang::csv::IO;

public loc OutputFilesDirectory = |project://OhlohAnalytics/output|;

public void exportFactsForAllProjects() {
	logToConsole("exportFactsForAllProject", "Getting Ohloh facts for all projects...");
	OhlohFactsMap OhlohFacts=getOhlohFactsFromCache();
	
	logToConsole("exportFactsForAllProject", "Getting monthly facts for all projects...");
	monthlyFacts=getMonthlyFactsFromCache(OhlohFacts);
	logToConsole("exportFactsForAllProject", "Exporting monthly facts to CSV for all projects: " + "monthlyFacts.csv");
	writeFactsToCSV(convertMonthlyFactsMapToRel(monthlyFacts),"monthlyFacts.csv");
	
	logToConsole("exportFactsForAllProject", "Getting grouped monthly facts by year for all projects...");
	monthlyFactsByYear=getYearlyFactsFromCache(monthlyFacts);
	logToConsole("exportFactsForAllProject", "Exporting monthly facts grouped by year to CSV for all projects: " + "monthlyFactsByYear.csv");
	writeFactsToCSV(convertYearlyFactsMapToRel(monthlyFactsByYear),"monthlyFactsByYear.csv");
}

public void writeFactsToCSV(facts,str fileName) {
	writeCSV(facts,
			 OutputFilesDirectory + fileName,
			 ("separator" : ","));
}

public rel [str projectName,
			 str year,
			 str month,
			 int loc_added,
		     int loc_deleted,
		     int commits,
		     int contributors,
		     int loc_total,
		     int abs_loc_growth,
		     real loc_growth_factor]
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
	loc_added_fact(int loc_added)           		<- monthlyFacts[key],
    loc_deleted_fact(int loc_deleted) 				<- monthlyFacts[key],
    commits_fact(int commits) 						<- monthlyFacts[key],
    contributors_fact(int contributors) 			<- monthlyFacts[key],
    loc_total_fact(int loc_total) 					<- monthlyFacts[key],
    abs_loc_growth_fact(int abs_loc_growth) 		<- monthlyFacts[key],
    loc_growth_factor_fact(real loc_growth_factor) 	<- monthlyFacts[key]
    };
}

public rel [str projectName,
			 str year,
			 str month,
			 int sum_loc_added,
		     int sum_loc_deleted,
		     int sum_commits,
		     int median_contributors,
		     int max_loc_total,
		     int sum_abs_loc_growth,
		     real prod_loc_growth_factor,
		     int age]
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
	     prod_loc_growth_factor,
	     age>
	|
	key <- yearlyFacts,
	<str projectName, str year, str month> := key,
	sum_loc_added_fact(int loc_added)           		<- yearlyFacts[key],
    sum_loc_deleted_fact(int loc_deleted) 				<- yearlyFacts[key],
    sum_commits_fact(int commits) 						<- yearlyFacts[key],
    median_contributors_fact(int contributors) 			<- yearlyFacts[key],
    max_loc_total_fact(int loc_total) 					<- yearlyFacts[key],
    sum_abs_loc_growth_fact(int abs_loc_growth) 		<- yearlyFacts[key],
    prod_loc_growth_factor_fact(real loc_growth_factor) <- yearlyFacts[key],
    age_fact(int age) 									<- yearlyFacts[key]
    };
}
