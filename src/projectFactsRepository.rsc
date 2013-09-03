module projectFactsRepository

import Prelude;
import lang::xml::DOM;
import Logging;
import Caching;
import util::Maybe;

private loc LocalOhlohProjectsRepository = |project://OhlohAnalytics/data|;
private loc ProjectNamesListFile = LocalOhlohProjectsRepository + "ProjectNamesList.txt";
private loc ProjectsFile = LocalOhlohProjectsRepository + "Projects.xml";
private str MetaDataFileName = "MetaData.xml";
private str EnlistmentsFileName = "Enlistments.xml";
private str SizeFactsFileName = "SizeFacts.xml";
private str ActivityFactsFileName = "ActivityFacts.xml";

public list[str] getProjectNamesInRepository() {
	return listEntries(LocalOhlohProjectsRepository + "projects");
}

public list[str] getProjectNamesOnList() {
	return readFileLines(ProjectNamesListFile);
}

alias repositoriesRel = rel[str projectName, str repositoryType, str repositoryURL];

alias metaDataRel = rel[str projectName, str elementValue];

alias factsKey = tuple[str projectName, str year, str month];

data monthlyFact =
		    loc_added_fact(Maybe[num] i) |
		    loc_deleted_fact(Maybe[num] i) |
		    comments_added_fact(Maybe[num] i) |
		    comments_deleted_fact(Maybe[num] i) |
		    blanks_added_fact(Maybe[num] i) |
		    blanks_deleted_fact(Maybe[num] i) |
		    commits_fact(Maybe[num] i) |
		    contributors_fact(Maybe[num] i) |
		    loc_fact(Maybe[num] i) |
		    comments_fact(Maybe[num] i) |
		    blanks_fact(Maybe[num] i) |
		    comment_ratio_fact(Maybe[num] r) |
		    cumulative_commits_fact(Maybe[num] i) |
		    man_months_fact(Maybe[num] i) |
		    loc_growth_absolute_fact(Maybe[num] i) |
		    loc_growth_factor_fact(Maybe[num] r);
		    
alias monthlyFactsMap = map[factsKey, set[monthlyFact]];

public monthlyFactsMap mergeFactsForProjects (list[str] projectNames) {

     return (key : maybeGetSizeFacts(sizeFacts, key) +
                   maybeGetActivityFacts(activityFacts, key)
             |
             str projectName <- projectNames,
             monthlyFactsMap sizeFacts := getSizeFacts(projectName),
             monthlyFactsMap activityFacts := getActivityFacts(projectName),
             factsKey key <- domain(activityFacts) + domain(sizeFacts)
            );
}

private set[monthlyFact] maybeGetSizeFacts(monthlyFactsMap sizeFacts, factsKey key) {
	if(key in sizeFacts) {
		return sizeFacts[key];
	}
	else {
		return {loc_fact(nothing()),
			    comments_fact(nothing()),
			    blanks_fact(nothing()),
			    comment_ratio_fact(nothing()),
			    cumulative_commits_fact(nothing()),
			    man_months_fact(nothing())};
	}
}

private set[monthlyFact] maybeGetActivityFacts(monthlyFactsMap activityFacts, factsKey key) {
	if(key in activityFacts) {
		return activityFacts[key];
	}
	else {
		return {loc_added_fact(nothing()),
		    	loc_deleted_fact(nothing()),
		    	comments_added_fact(nothing()),
		    	comments_deleted_fact(nothing()),
		    	blanks_added_fact(nothing()),
		    	blanks_deleted_fact(nothing()),
		    	commits_fact(nothing()),
		    	contributors_fact(nothing())};
	}
}

public monthlyFactsMap mergeFactsForAllProjects () { 
	return mergeFactsForProjects(getProjectNamesInRepository());
}

public metaDataRel getMetaDataElements(list[str] projectNames, str elementName) {
	return {
		valuesForProject |
		projectName <- projectNames,
		valuesForProject <- getMetaDataElements(projectName, elementName)
	};
}

public metaDataRel getMetaDataElements(str projectName, str elementName) {
	result = {};
		
	top-down visit(getProjectMetaDataDOM(projectName)) {
		case element(_,
					 elementName,
					 [Node*,charData(str elementValue)]):
		{
             result += <projectName,
			 			elementValue>;
		}
	}
	return result;
}

public repositoriesRel getRepositoryFactsForProjects(list[str] projectNames) {
	return {
		<projectName, repositoryType, repositoryURL> |
		projectName <- projectNames,
		<projectName,repositoryType,repositoryURL> <- getRepositoryFacts(projectName)
	};
}

public repositoriesRel getRepositoryFacts(str projectName) {	   
	result = {};
	try
		top-down visit(getProjectEnlistmentsDOM(projectName)) {
			case element(_,"repository",
					[
					 _,
					 element(_,"type",[charData(str repositoryType)]),
					 element(_,"url",[charData(str repositoryURL)]),
					 Node*
					]):
			{
	             result += <projectName,
				 			repositoryType,
				 			repositoryURL>;
			}
		}
	catch: logToConsole("getRepositoryFacts", "WARNING error while getting repository facts for project: <projectName>.");
	return result;
}

public monthlyFactsMap getActivityFacts(str projectName)
{	   
    result = ();
    try 
    	top-down visit(getActivityFactsDOM(projectName)) {
			case element(none(),"activity_fact",
					[
					 element(_,"month",[charData(str monthAsString)]),
					 element(_,"code_added",[charData(str LOCAddedAsString)]),
					 element(_,"code_removed",[charData(str LOCDeletedAsString)]),
					 element(_,"comments_added",[charData(str CommentsAddedAsString)]),
					 element(_,"comments_removed",[charData(str CommentsDeletedAsString)]),
					 element(_,"blanks_added",[charData(str BlanksAddedAsString)]),
					 element(_,"blanks_removed",[charData(str BlanksDeletedAsString)]),
					 element(_,"commits",[charData(str CommitsAsString)]),
					 element(_,"contributors",[charData(str ContributorsAsString)])
					]):
	        {
				 str year = getYear(monthAsString);
				 str month = getMonth(monthAsString);
				 result += (<projectName,
	                         year,
				 			 month> :
				 			{loc_added_fact(just(toInt(LOCAddedAsString))),
				 			 loc_deleted_fact(just(toInt(LOCDeletedAsString))),
				 			 comments_added_fact(just(toInt(CommentsAddedAsString))),
				 			 comments_deleted_fact(just(toInt(CommentsDeletedAsString))),
				 			 blanks_added_fact(just(toInt(BlanksAddedAsString))),
				 			 blanks_deleted_fact(just(toInt(BlanksDeletedAsString))),
				 			 commits_fact(just(toInt(CommitsAsString))),
				 			 contributors_fact(just(toInt(ContributorsAsString)))});
	        }
		}
	catch: logToConsole("getActivityFacts", "WARNING error while getting activity facts for project: <projectName>.");
	return result;
}

public monthlyFactsMap getSizeFacts(str projectName) {	   
	result = ();
	try
		top-down visit(getSizeFactsDOM(projectName)) {
			case element(none(),"size_fact",
					[
					 element(_,"month",[charData(str monthAsString)]),
					 element(_,"code",[charData(str LOCTotalAsString)]),
					 element(_,"comments",[charData(str CommentsAsString)]),
					 element(_,"blanks",[charData(str BlanksAsString)]),
					 element(_,"comment_ratio",[charData(str CommentRatioAsString)]),
					 element(_,"commits",[charData(str CumulativeCommitsAsString)]),
					 element(_,"man_months",[charData(str ManMonthsAsString)])
					]):
			{
	             str year = getYear(monthAsString);
	             str month = getMonth(monthAsString);
	             result += (<projectName,
				 			 year,
				 			 month> :
				 			{loc_fact(just(toInt(LOCTotalAsString))),
				 			 comments_fact(just(toInt(CommentsAsString))),
				 			 blanks_fact(just(toInt(BlanksAsString))),
				 			 comment_ratio_fact(just(toInt(CommentRatioAsString))),
				 			 cumulative_commits_fact(just(toInt(CumulativeCommitsAsString))),
				 			 man_months_fact(just(toInt(ManMonthsAsString)))});
			}
		}
	catch: logToConsole("getSizeFacts", "WARNING error while getting size facts for project: <projectName>.");
	return result;
}

// broken
//private sizeFactsMap validateAndFilterSizeFacts (sizeFactsMap unfilteredSizeFacts) {
//	return (key : <projectName, year, month, loc_total> |
//		str key <- unfilteredSizeFacts,
//		<str projectName, str year, str month, str loc_total> <- [unfilteredSizeFacts[key]],
//		!(toInt(loc_total) < 0 && logToConsole("validateAndFilterSizeFacts", "WARNING data excluded because lines of code count is below 0 for project: <projectName> in year/month: <year>/<month>"))
//	);
//}

public void addMetaDataToRepository(str metaDataXML, str projectName) {
	addXMLFileToRepository(metaDataXML, projectName, MetaDataFileName);
}

public void addEnlistmentsToRepository(str enlistmentsXML, str projectName) {
	addXMLFileToRepository(enlistmentsXML, projectName, EnlistmentsFileName);
}

public void addActivityFactsToRepository(str activityFactsXML, str projectName) {
	addXMLFileToRepository(activityFactsXML, projectName, ActivityFactsFileName);
}


public void addSizeFactsToRepository(str sizeFactsXML, str projectName) {
	addXMLFileToRepository(sizeFactsXML, projectName, SizeFactsFileName);
}

private void addXMLFileToRepository(str XML, str projectName, str fileName) {
	loc file = LocalOhlohProjectsRepository + "projects" + projectName + fileName;
	if (!validateXML(XML)) throw "addXMLFileToRepository: Validation of XML contents failed while adding <fileName> for project: <projectName>";
	else writeFile(file, XML);
}

public void addProjectsListToRepository(str projectsListPage) {
	str outputString = "";
	
	list[str] projectList = extractProjectNames(projectsListPage);
	for(str projectName <- projectList) {
		outputString += (projectName + "\n");
	}
	appendToFile(ProjectNamesListFile, outputString);
}

public list[str] getProjectNamesListFromRepository() {
	return readFileLines(ProjectNamesListFile);
}

private list[str] extractProjectNames (str XML) {
	list[str] result = [];
	top-down visit(getXMLContentsDOM(XML)) {
		case element(_,"url_name",[charData(str projectName)]):
			 result += projectName;
	}
	return result;
}

public Node getProjectMetaDataDOM(str projectName) {
	return getXMLContentsDOM(projectName, MetaDataFileName);
}

public Node getProjectEnlistmentsDOM(str projectName) {
	return getXMLContentsDOM(projectName, EnlistmentsFileName);
}

private Node getActivityFactsDOM(str projectName) {
	return getXMLContentsDOM(projectName, ActivityFactsFileName);
}

private Node getSizeFactsDOM(str projectName) {
	return getXMLContentsDOM(projectName, SizeFactsFileName);
}

private Node getXMLContentsDOM(str projectName, str fileName) {
	return getValueFromCache("projects" + "/" + projectName + "/" + fileName + ".cache",
		   			         #Node, Node () {return uncachedGetXMLContentsDOM(projectName, fileName);});
}

private Node uncachedGetXMLContentsDOM(str projectName, str fileName) {
	loc file = LocalOhlohProjectsRepository + "projects" + projectName + fileName;
	str XML = readFile(file);
	if (!validateXML(XML)) throw "getXMLContentsDOM: Validation of XML contents failed while reading <fileName> for project: <projectName>";
	XMLContentsDOM = parseXMLDOMTrim(XML);
	return XMLContentsDOM;
}

private str reformatDateTime(str dateTimeString) {
	datetime dt = parseDateTime(dateTimeString,"yyyy-MM-dd\'T\'HH:mm:ss\'");
	return printDate(dt, "yyyy-MM");
}

private str getYear(str dateTimeString) {
	return printDate(getDateTime(dateTimeString), "yyyy");
}

private str getMonth(str dateTimeString) {
	return printDate(getDateTime(dateTimeString), "MM");
}

private datetime getDateTime(str dateTimeString) {
	return parseDateTime(dateTimeString,"yyyy-MM-dd\'T\'HH:mm:ss");
}

private bool validateXML(str input) {
	return (/\s*\<response\>\s*\<status\>\s*success\s*\<\/status\>.*/i := input);
}

