module obtainProjectFacts

import Prelude;
import projectFactsRepository;
import Logging;
import Sleep;
import lang::xml::DOM;
import util::Math;

private loc OhlohAPIKeyFile = |project://OhlohAnalytics/OhlohAPIKeys.txt|;
private str OhlohBaseURL = "www.ohloh.net";
private int timeToSleepBetweenQueries = 250; //milliseconds
private int projectsPerAPIKey = 80; 

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
	return obtainProjectDataFromOhloh(slice(getProjectNamesOnList(),
											startAtProject-1,
											numberOfProjects));
}

public void obtainProjectDataFromOhloh(list[str] projectNames) {
	loc logFile = openLogFile("raw-fetch-log");
	int projectsDone = 0;
	for (str APIKey <- readFileLines(OhlohAPIKeyFile) && projectsDone < size(projectNames)) {
		for (str p <- slice(projectNames,projectsDone,min(size(projectNames) - projectsDone,
													      projectsPerAPIKey))) {
			obtainMetaDataFromOhloh(p, APIKey, logFile);
			sleep(timeToSleepBetweenQueries);
			obtainEnlistmentsFromOhloh(p, APIKey, logFile);
			sleep(timeToSleepBetweenQueries);
			obtainActivityFactsFromOhloh(p, APIKey, logFile);
			sleep(timeToSleepBetweenQueries);
			obtainSizeFactsFromOhloh(p, APIKey, logFile);
			sleep(timeToSleepBetweenQueries);
			projectsDone += 1;
		}
	}
	if (projectsDone < size(projectNames)) {
		logToFile(logFile,"obtainProjectDataFromOhloh", "WARNING: Not enough API keys to get data for all projects from Ohloh!");
	}
}

public void obtainMetaDataFromOhloh(str projectName, str APIKey, loc logFile) {
	loc projectMetaDataURI = |http://<OhlohBaseURL>/projects/<projectName>.xml?api_key=<APIKey>|;
	logToFile(logFile,"obtainMetaDataFromOhloh", "Reading meta data for project <projectName> from Ohloh.");
	try addMetaDataToRepository(readFile(projectMetaDataURI), projectName);
	catch: logToFile(logFile,"obtainMetaDataFromOhloh", "WARNING: problem when reading meta data for project <projectName> from Ohloh, skipped.");
}

public void obtainEnlistmentsFromOhloh(str projectName, str APIKey, loc logFile) {
	loc projectEnlistmentsURI = |http://<OhlohBaseURL>/projects/<projectName>/enlistments.xml?api_key=<APIKey>|;
	logToFile(logFile,"obtainEnlistmentsFromOhloh", "Reading enlistments for project <projectName> from Ohloh.");
	try addEnlistmentsToRepository(readFile(projectEnlistmentsURI), projectName);
	catch: logToFile(logFile,"obtainEnlistmentsFromOhloh", "WARNING: problem when reading enlistments for project <projectName> from Ohloh, skipped.");
}

public void obtainActivityFactsFromOhloh(str projectName, str APIKey, loc logFile) {
	loc projectActivityFactsURI = |http://<OhlohBaseURL>/projects/<projectName>/analyses/latest/activity_facts.xml?api_key=<APIKey>|;
	logToFile(logFile,"obtainActivityFactsFromOhloh", "Reading activity facts for project <projectName> from Ohloh.");
	try addActivityFactsToRepository(readFile(projectActivityFactsURI), projectName);
	catch: logToFile(logFile,"obtainActivityFactsFromOhloh", "WARNING: problem when reading activity facts for project <projectName> from Ohloh, skipped.");
}

public void obtainSizeFactsFromOhloh(str projectName, str APIKey, loc logFile) {
	loc projectSizeFactsURI = |http://<OhlohBaseURL>/projects/<projectName>/analyses/latest/size_facts.xml?api_key=<APIKey>|;
	logToFile(logFile,"obtainSizeFactsFromOhloh", "Reading size facts for project <projectName> from Ohloh.");
	try addSizeFactsToRepository(readFile(projectSizeFactsURI), projectName);
	catch: logToFile(logFile,"obtainSizeFactsFromOhloh", "WARNING: problem when reading size facts for project <projectName> from Ohloh, skipped.");
}