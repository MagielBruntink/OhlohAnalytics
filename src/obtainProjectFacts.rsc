module obtainProjectFacts

import Prelude;
import projectFactsRepository;
import Logging;
import Sleep;
import lang::xml::DOM;

private loc OhlohAPIKeyFile = |project://OhlohAnalytics/OhlohAPIKey.txt|;
private str OhlohAPIKey = readFile(OhlohAPIKeyFile);
private str OhlohBaseURL = "www.ohloh.net";
private int timeToSleepBetweenQueries = 1000; //milliseconds

public void obtainProjectListFromOhloh(int endAtPage) {
	obtainProjectListFromOhloh(1, endAtPage);
}

@doc{
	Work in progress!
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

public void obtainActivityFacts(str ProjectName) {

}

//Activity Facts query
//https://www.ohloh.net/projects/rascal/analyses/latest/activity_facts.xml?api_key=

//Size Facts query
//https://www.ohloh.net/projects/firefox/analyses/latest/size_facts.xml?api_key=

