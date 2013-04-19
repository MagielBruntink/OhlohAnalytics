module analyzeProjectDeath

import Prelude;
import processProjectFacts;
import projectFactsRepository;
import IO;
import exportProjectFacts;

alias projectStatus = rel[str, list[tuple[str, bool]]];

public projectStatus getProjectLiveStatus (yearlyFactsMap monthlyFactsByYear)
{
	projectNames = domain(monthlyFactsByYear)<0>;
	return
	{
		<projectName,
			sort(
				[<year,alive> |
				 <projectName, str year, str month> <- monthlyFactsByYear,
			 	 sum_commits_fact(num sum_commits) <- monthlyFactsByYear[<projectName, year, month>],
			 	 (sum_commits == 0 && bool alive := false || sum_commits > 0 && bool alive := true) 
				],
				bool (tuple[str,bool] year1, tuple[str,bool] year2) {
					return toInt(year1<0>) < toInt(year2<0>);
				})>
		|
		str projectName <- projectNames
	};
}

public projectStatus findDeadProjectsInYear (projectStatus stats, str year) {
	return {
		project |
		project <- stats,
		<_, [Years*,<year,false>,MoreYears*]> := project
	};
}

public projectStatus findProjectsThatDiedAtLeastOnce (projectStatus stats) {
	return {
		project |
		project <- stats,
		<_, [Years*,<year,false>,MoreYears*]> := project
	};
}

public projectStatus findResurrectedProjects (projectStatus stats) {
	return {
		project |
		project <- stats,
		<_, [Years*,<year1,false>,MoreYears*,<year2,true>,EvenMoreYears*]> := project
	};
}

public void printProjectStatuses (projectStatus stats, yearlyFactsMap facts) {
	outfile = OutputFilesDirectory + "projectStatuses.csv";
	separator = ",";
	list[str] years = sort(domain(facts)<1>,bool (str a, str b) {return toInt(a) > toInt(b);});
	
	header = "projectName";
	for (str year <- years) {
		header += separator;
		header += year;
	};
	header += "\n";
	writeFile(outfile,header);
	
	for (str projectName <- domain(stats)) {
		line = projectName;
		for (str year <- years) {
			line += separator;
			if(<projectName,year,"12"> in facts) 				
				if ({[Years*,<year,true>,MoreYears*]} := stats[projectName]) 
					line += "alive";
				else
					line += "dead";
		};
		line += "\n";
		appendToFile(outfile,line);
	};
}
