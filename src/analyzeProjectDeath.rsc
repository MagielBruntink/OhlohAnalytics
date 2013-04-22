module analyzeProjectDeath

import Prelude;
import processProjectFacts;
import projectFactsRepository;
import IO;
import exportProjectFacts;

alias projectActivityStatus = rel[str, list[tuple[str, num, bool]]];

alias projectDeathStatus = rel[str projectName, int age, int status];

private set[str] excludedYears = {"2013"};

public projectActivityStatus getProjectActivityStatus (yearlyFactsMap monthlyFactsByYear)
{
	projectNames = domain(monthlyFactsByYear)<0>;
	return
	{
		<projectName,
			sort(
				[<year,age,active> |
				 <projectName, str year, str month> <- monthlyFactsByYear,
				 year notin excludedYears,
			 	 sum_commits_fact(num sum_commits) <- monthlyFactsByYear[<projectName, year, month>],
			 	 age_fact(num age) <- monthlyFactsByYear[<projectName, year, month>],
			 	 (sum_commits == 0 && bool active := false || sum_commits > 0 && bool active := true) 
				],
				bool (tuple[str,num,bool] year1, tuple[str,num,bool] year2) {
					return toInt(year1<0>) < toInt(year2<0>);
				})>
		|
		str projectName <- projectNames
	};
}

public projectActivityStatus findInactiveProjectsInYear (projectActivityStatus stats, str year) {
	return {
		project |
		project <- stats,
		<_, [Years*,<year,age,false>,MoreYears*]> := project
	};
}

public projectActivityStatus findProjectsThatBecomeInactiveAtLeastOnce (projectActivityStatus stats) {
	return {
		project |
		project <- stats,
		<_, [Years*,<year,age,false>,MoreYears*]> := project
	};
}

public projectActivityStatus findProjectThatAreReactivated (projectActivityStatus stats) {
	return {
		project |
		project <- stats,
		<_, [Years*,<year1,age,false>,MoreYears*,<year2,age,true>,EvenMoreYears*]> := project
	};
}

public void printProjectActivity (projectActivityStatus stats, yearlyFactsMap facts) {
	outfile = OutputFilesDirectory + "projectActivity.csv";
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
				if ({[Years*,<year,age,true>,MoreYears*]} := stats[projectName]) 
					line += "active";
				else
					line += "not active";
		};
		line += "\n";
		appendToFile(outfile,line);
	};
}

public projectDeathStatus getProjectDeathStatus (projectActivityStatus stats, str cutoffYear) {
	return {
		<projectName,age,status> |
		projectHistory <- stats,
		<projectName,  [Years*, <year,age,isActive>, MoreYears*]> := projectHistory,
		toInt(year) <= toInt(cutoffYear),
		(isActive == false && 
			[_*,<_,_,true>] := Years &&
			![_*,<_,_,true>,_*] := MoreYears && 
			int status := 2)
		||
		(isActive == true && year == cutoffYear && int status := 1)
	};
}


