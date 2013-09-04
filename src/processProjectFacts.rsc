module processProjectFacts

import Prelude;
import projectFactsRepository;
import util::Math;
import Logging;
import Caching;
		    
data yearlyFact =
			sum_loc_added_fact(num i) |
			sum_loc_deleted_fact(num i) |
			sum_commits_fact(num i) |
			median_contributors_fact(num n) |
			max_loc_total_fact(num i) |
			sum_abs_loc_growth_fact(num i) |
			prod_loc_growth_factor_fact(num r) |
			age_fact(num i);

alias yearlyFactsMap = map[factsKey, set[yearlyFact]];
    
public factsMap getOhlohFactsFromCache() {
	return getValueFromCache("OhlohFactsMap", #factsMap, mergeFactsForAllProjects);
}

public factsMap getMonthlyFactsFromCache(factsMap OhlohFacts) {
	return getValueFromCache("factsMap",
							 #factsMap,
							 factsMap () {return addMonthlyGrowthFacts(OhlohFacts);});
}

public yearlyFactsMap getYearlyFactsFromCache(factsMap monthlyFacts) {
	return getValueFromCache("yearlyFactsMap",
							#yearlyFactsMap,
							yearlyFactsMap () {return groupMonthlyFactsByYear(monthlyFacts);});
}

public factsMap addMonthlyGrowthFacts(factsMap OhlohFacts) {

	return (
		<projectName,year,month> :
		baseFactsForMonth + { 
			abs_loc_growth_fact(locThisMonth - locPreviousMonth), 
			loc_growth_factor_fact(1.0 + toReal(locThisMonth - locPreviousMonth) / toReal(locPreviousMonth))
		} |

		<str projectName,str year, str month> <- OhlohFacts,
		set[monthlyFact] baseFactsForMonth := OhlohFacts[<projectName,year,month>],
		loc_total_fact(num locThisMonth) <- baseFactsForMonth,
		datetime previousYearMonth := decrementMonths(createDateTime(toInt(year,10),toInt(month,10),1,0,0,0,0)),
		str previousYear := printDateTime(previousYearMonth,"yyyy"),
		str previousMonth := printDateTime(previousYearMonth,"MM"),
		<projectName,previousYear,previousMonth> in OhlohFacts,
		loc_total_fact(num locPreviousMonth) <- OhlohFacts[<projectName, previousYear, previousMonth>],
		locPreviousMonth > 0
	);
}

public yearlyFactsMap groupMonthlyFactsByYear(factsMap monthlyFacts)
{
	projectNamesYearsMonths = domain(monthlyFacts);
	projectNamesYears = projectNamesYearsMonths<0,1>;
	years = projectNamesYearsMonths<1>;
	months = projectNamesYearsMonths<2>;

	return (
		<projectName, year, "12"> : {
		 sum_loc_added_fact(sum([monthlyLocAdded   	  			  | loc_added_fact(int monthlyLocAdded)              <- factsForProjectInYear])),
		 sum_loc_deleted_fact(sum([monthlyLocDeleted 	  		  | loc_deleted_fact(int monthlyLocDeleted)          <- factsForProjectInYear])),
		 sum_commits_fact(sum([monthlyCommits          			  | commits_fact(int monthlyCommits)                 <- factsForProjectInYear])),
		 median_contributors_fact(median([monthlyContributors     | contributors_fact(int monthlyContributors)       <- factsForProjectInYear])),
		 max_loc_total_fact(max([monthlyLocTotal         		  | loc_total_fact(int monthlyLocTotal)              <- factsForProjectInYear])),							
		 sum_abs_loc_growth_fact(sum([monthlyAbsoluteGrowth   	  | abs_loc_growth_fact(int monthlyAbsoluteGrowth)   <- factsForProjectInYear])),							
		 prod_loc_growth_factor_fact(product([monthlyGrowthFactor | loc_growth_factor_fact(num monthlyGrowthFactor) <- factsForProjectInYear])),
		 age_fact(toInt(year) - minYearForProject)
		}
		 
		|
		
		str projectName <- projectNamesYearsMonths<0>,
		yearsMonthsForProject := 
		{
			<year,month> |
			str year <- years,
			str month <- months,
			<projectName, year, month> in monthlyFacts
		},
		int minYearForProject := min([toInt(year) | str year <- yearsMonthsForProject<0>]),
		
		str year <- yearsMonthsForProject<0>,
		factsForProjectInYear :=
		[ 
			fact |
			<year, str month> <- yearsMonthsForProject,
			monthlyFact fact <- monthlyFacts[<projectName,year,month>]
		]
	);
}

public num product (list[num] listOfNumbers) {
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

