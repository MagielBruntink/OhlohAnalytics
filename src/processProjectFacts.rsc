module processProjectFacts

import Prelude;
import projectFactsRepository;

public void processProjectFactsTest() {
	str project = "firefox";
	activityFacts = getActivityFacts(project);
	sizeFacts = getSizeFacts(project);
	rel[str project,
	    str month,
		str loc_added,
		str loc_removed,
		str commits,
		str contributors,
		str total_loc] mergedFacts =
	{<project, month> + activityFact + <sizeFact> | month <- activityFacts.month + sizeFacts.month,
	                                                activityFact <- activityFacts[month], sizeFact <- sizeFacts[month]};
	println(mergedFacts<month,contributors>);
}
