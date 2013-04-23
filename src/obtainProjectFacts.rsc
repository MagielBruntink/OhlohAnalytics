module obtainProjectFacts

import Prelude;
import projectFactsRepository;
import Logging;
import Sleep;
import lang::xml::DOM;
import util::Math;

private loc OhlohAPIKeyFile = |project://OhlohAnalytics/OhlohAPIKey.txt|;
public str OhlohAPIKey = readFile(OhlohAPIKeyFile);
private str OhlohBaseURL = "www.ohloh.net";
private int timeToSleepBetweenQueries = 1000; //milliseconds

public void obtainProjectListFromOhloh(int endAtPage) {
	obtainProjectListFromOhloh(1, endAtPage);
}

@doc{
	int startAtPage : page number to start at, counting from 1. 
	int endAtPage   : page number of last page to obtain.
}
public void obtainProjectListFromOhloh(int startAtPage, int endAtPage) {
	int pageToObtain = startAtPage;

	while (pageToObtain <= endAtPage) {
		loc projectsListURI = |http://<OhlohBaseURL>/projects.xml?api_key=<OhlohAPIKey>&page=<toString(pageToObtain)>|;
		logToConsole("obtainProjectsFromOhloh", "Reading projects from Ohloh, URL: <projectsListURI>");
		addProjectsListToRepository(readFile(projectsListURI));
		pageToObtain += 1;
		sleep(timeToSleepBetweenQueries);
	}
}

@doc{
	int startAtProject   : project number (line nr. in project list file) to start at, counting from 1. 
	int numberOfProjects : number of projects to obtain starting at startAtProject.
}
public void obtainProjectDataFromOhloh(int startAtProject, int numberOfProjects) {
	return obtainProjectDataFromOhloh(slice(getProjectNamesListFromRepository(),
											startAtProject-1,
											numberOfProjects));
}

public void obtainProjectDataFromOhloh(list[str] projectNames) {
	for (str p <- projectNames) {
		obtainMetaDataFromOhloh(p);
		sleep(timeToSleepBetweenQueries);
		obtainActivityFactsFromOhloh(p);
		sleep(timeToSleepBetweenQueries);
		obtainSizeFactsFromOhloh(p);
		sleep(timeToSleepBetweenQueries);
	}
}

public void obtainMetaDataFromOhloh(str projectName) {
	loc projectMetaDataURI = |http://<OhlohBaseURL>/projects/<projectName>.xml?api_key=<OhlohAPIKey>|;
	logToConsole("obtainMetaDataFromOhloh", "Reading meta data for project <projectName> from Ohloh, URL: <projectMetaDataURI>");
	try addMetaDataToRepository(readFile(projectMetaDataURI), projectName);
	catch: logToConsole("obtainMetaDataFromOhloh", "WARNING: problem when reading meta data for project <projectName> from Ohloh, skipped.");
}

public void obtainActivityFactsFromOhloh(str projectName) {
	loc projectActivityFactsURI = |http://<OhlohBaseURL>/projects/<projectName>/analyses/latest/activity_facts.xml?api_key=<OhlohAPIKey>|;
	logToConsole("obtainActivityFactsFromOhloh", "Reading activity facts for project <projectName> from Ohloh, URL: <projectActivityFactsURI>");
	try addActivityFactsToRepository(readFile(projectActivityFactsURI), projectName);
	catch: logToConsole("obtainActivityFactsFromOhloh", "WARNING: problem when reading activity facts for project <projectName> from Ohloh, skipped.");
}

public void obtainSizeFactsFromOhloh(str projectName) {
	loc projectSizeFactsURI = |http://<OhlohBaseURL>/projects/<projectName>/analyses/latest/size_facts.xml?api_key=<OhlohAPIKey>|;
	logToConsole("obtainSizeFactsFromOhloh", "Reading size facts for project <projectName> from Ohloh, URL: <projectSizeFactsURI>");
	try addSizeFactsToRepository(readFile(projectSizeFactsURI), projectName);
	catch: logToConsole("obtainSizeFactsFromOhloh", "WARNING: problem when reading size facts for project <projectName> from Ohloh, skipped.");
}