module processProjectFacts

import Prelude;
import projectFactsRepository;
import util::Math;
import Logging;
import Caching;

data factsKey = factsKey(str projectName, str year, str month);

data monthlyFact = 
		    loc_added_fact(int i) |
		    loc_deleted_fact(int i) |
		    commits_fact(int i) |
		    contributors_fact(int i) |
		    loc_total_fact(int i) |
		    abs_loc_growth_fact(int i) |
		    loc_growth_factor_fact(real r);

alias monthlyFactsMap = map[factsKey, set[monthlyFact]];
		    
data yearlyFact =
			sum_loc_added_fact(int i) |
			sum_loc_deleted_fact(int i) |
			sum_commits_fact(int i) |
			median_contributors_fact(num n) |
			max_loc_total_fact(int i) |
			sum_abs_loc_growth_fact(int i) |
			prod_loc_growth_factor_fact(real r) |
			age(int i);

alias yearlyFactsMap = map[factsKey, set[yearlyFact]];

alias monthlyFactsRel =
		rel[str projectName,
		    str year,
		    str month,
		    int loc_added,
		    int loc_deleted,
		    int commits,
		    int contributors,
		    int loc_total,
		    int abs_loc_growth,
		    real loc_growth_factor];

alias yearlyFactsRel = 
		rel[str projectName,
		    str year,
		    num sum_loc_added,
		    num sum_loc_deleted,
		    num sum_commits,
		    num median_contributors,
		    num max_loc_total,
		    num sum_abs_loc_growth,
		    num prod_loc_growth_factor,
		    num age];
		    
public OhlohFactsRel getOhlohFactsRelFromCache() {
	return getValueFromCache("OhlocFactsRel", getOhlohFactsRelForAllProjects);
}

public monthlyFactsRel getMonthlyFactsRelFromCache(OhlohFactsRel ohlohFacts) {
	return getValueFromCache("monthlyFactsRel", getMonthlyFacts, ohlohFacts);
}

public monthlyFactsRel getMonthlyFacts(OhlohFactsRel facts) {
	factsMap = (
		<projectName,year,month> : monthlyFacts |
		monthlyFacts <- facts,
		<str projectName,str year,str month,_,_,_,_,_> := monthlyFacts
	);

	return {
		<projectName, year, month, loc_added, loc_deleted, commits, contributors, locThisMonth,
			locThisMonth - locPreviousMonth, 
			1.0 + toReal(locThisMonth - locPreviousMonth) / toReal(locPreviousMonth)> |
			
		<str projectName,str year, str month> <- factsMap,
		<projectName,year,month,int loc_added,int loc_deleted,int commits,int contributors,int locThisMonth> :=
		 	factsMap[<projectName, year, month>],
		datetime previousYearMonth := decrementMonths(createDateTime(toInt(year,10),toInt(month,10),1,0,0,0,0)),
		str previousYear := printDateTime(previousYearMonth,"yyyy"),
		str previousMonth := printDateTime(previousYearMonth,"MM"),
		<projectName,previousYear,previousMonth> in factsMap,
		<_,_,_,_,_,_,_,int locPreviousMonth> := factsMap[<projectName, previousYear, previousMonth>]
	};
}

public yearlyFactsRel getMonthlyFactsGroupedByYear(monthlyFactsRel monthlyFacts)
{
	monthlyFactsMap = (
		factsKey(projectName,year,month) :
		{ loc_added_fact(monthlyLocAdded),
		  loc_deleted_fact(monthlyLocDeleted),
		  commits_fact(monthlyCommits),
	      contributors_fact(monthlyContributors),
	      loc_total_fact(monthlyLocTotal),
	      abs_loc_growth_fact(monthlyAbsoluteGrowth),
	      loc_growth_factor_fact(monthlyGrowthFactor)
	    } |
		<str projectName,
		 str year,
		 str month,
		 int monthlyLocAdded,
		 int monthlyLocDeleted,
		 int monthlyCommits,
		 int monthlyContributors,
		 int monthlyLocTotal,
		 int monthlyAbsoluteGrowth,
		 real monthlyGrowthFactor> <- monthlyFacts
	);

	minYearForProject = (
		projectName : minYear |
		projectYears := monthlyFacts<projectName,year>,
		str projectName <- projectYears<0>,
		int minYear := min([ toInt(year) | <projectName, str year> <- projectYears])
	);
	
	months = monthlyFacts<month>;

	return {
		<projectName, year, 
		 sum([monthlyLocAdded   	  | loc_added_fact(int monthlyLocAdded)              <- factsForProjectInYear]),
		 sum([monthlyLocDeleted 	  | loc_deleted_fact(int monthlyLocDeleted)          <- factsForProjectInYear]),
		 sum([monthlyCommits          | commits_fact(int monthlyCommits)                 <- factsForProjectInYear]),
		 median([monthlyContributors  | contributors_fact(int monthlyContributors)       <- factsForProjectInYear]),
		 max([monthlyLocTotal         | loc_total_fact(int monthlyLocTotal)              <- factsForProjectInYear]),							
		 sum([monthlyAbsoluteGrowth   | abs_loc_growth_fact(int monthlyAbsoluteGrowth)   <- factsForProjectInYear]),							
		 product([monthlyGrowthFactor | loc_growth_factor_fact(real monthlyGrowthFactor) <- factsForProjectInYear]),
		 toInt(year) - minYearForProject[projectName]>
		 
		|
		
		<str projectName, str year> <- monthlyFacts<projectName,year>,
		factsForProjectInYear :=
		[ 
			fact | 
			str month <- months,
			factsKey(projectName, year, month) in monthlyFactsMap,
			monthlyFact fact <- monthlyFactsMap[factsKey(projectName,year,month)]
		]
	};
}

private num product (list[num] listOfNumbers) {
	num result = 1;
	for (num number <- listOfNumbers) {
		result = result * number;
	}
	return result;
}

public num median (list[num] listOfNumbers) {
	list[num] sortedNumbers = sort(listOfNumbers);
	int length = size(listOfNumbers);
	num theMedian = 0;
	if(length > 0)
		if (length % 2 == 1)
			 theMedian = sortedNumbers[(length-1) / 2];
		else
			 theMedian = (sortedNumbers[(length-1) / 2] + sortedNumbers[(length / 2)]) / 2;
	else
		throw("Empty list provided as input, median does not exist.");
	return theMedian;
}

