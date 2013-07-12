module analyzeProjectDeath

import Prelude;
import processProjectFacts;
import projectFactsRepository;

alias projectActivityStatus = rel[str, list[tuple[str, num, bool]]];

alias projectDeathStatus = rel[str projectName, int age, int status, str yearOfEvent];

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

public projectDeathStatus getProjectDeathStatus (projectActivityStatus stats, str cutoffYear) {
	return {
		<projectName,age,status,year> |
		projectHistory <- stats,
		<projectName,  [Years*, <year,age,isActive>, MoreYears*]> := projectHistory,
		toInt(year) <= toInt(cutoffYear),
		(isActive == false &&
			//<_, [_*,<cutoffYear,_,false>]> := projectHistory && 
			[_*,<_,_,true>] := Years &&
			![_*,<_,_,true>,_*] := MoreYears &&
			int status := 2)
		||
		((isActive == true && (year == cutoffYear || 
							  (year != cutoffYear && [] := MoreYears)))
		  && int status := 1)
	};
}


