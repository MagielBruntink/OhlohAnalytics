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

alias maybeFacts = map[maybeFactKey, Maybe[value]];
alias factsKey = tuple[str projectName, str year, str month];
alias factsMap = map[factsKey, maybeFacts];

public alias maybeFactKey = str;
public maybeFactKey project_name_fact        = "project_name_fact";
public maybeFactKey year_fact    			 = "year_fact";
public maybeFactKey month_fact   			 = "month_fact";
public maybeFactKey loc_added_fact           = "loc_added_fact";
public maybeFactKey loc_deleted_fact         = "loc_deleted_fact";
public maybeFactKey comments_added_fact      = "comments_added_fact";
public maybeFactKey comments_deleted_fact    = "comments_deleted_fact";
public maybeFactKey blanks_added_fact        = "blanks_added_fact";
public maybeFactKey blanks_deleted_fact      = "blanks_deleted_fact";
public maybeFactKey commits_fact             = "commits_fact";
public maybeFactKey contributors_fact        = "contributors_fact";
public maybeFactKey loc_fact                 = "loc_fact";
public maybeFactKey comments_fact            = "comments_fact";
public maybeFactKey blanks_fact              = "blanks_fact";
public maybeFactKey comment_ratio_fact       = "comment_ratio_fact";
public maybeFactKey cumulative_commits_fact  = "cumulative_commits_fact";
public maybeFactKey man_months_fact          = "man_months_fact";
public maybeFactKey loc_growth_absolute_fact = "loc_growth_absolute_fact";
public maybeFactKey loc_growth_factor_fact   = "loc_growth_factor_fact";
public maybeFactKey main_language_fact  	 = "main_language_fact";

public list[maybeFactKey] identificationFactKeys = [
			project_name_fact,
		    year_fact,
		    month_fact
		   ];

public list[maybeFactKey] activityFactKeys = [
			loc_added_fact,
		    loc_deleted_fact,
		    comments_added_fact,
		    comments_deleted_fact,
		    blanks_added_fact,
		    blanks_deleted_fact,
		    commits_fact,
		    contributors_fact
		   ];
		   
public list[maybeFactKey] sizeFactKeys = [
			loc_fact,
		    comments_fact,
		    blanks_fact,
		    comment_ratio_fact,
		    cumulative_commits_fact,
		    man_months_fact
		   ];
		   
public list[maybeFactKey] growthFactKeys = [
			loc_growth_absolute_fact,
		    loc_growth_factor_fact
		   ];

public list[maybeFactKey] metaDataFactKeys = [
			main_language_fact
		   ];

public factsMap mergeFactsForProjects (list[str] projectNames) {

     return (key : (project_name_fact : just(project), year_fact : just(year), month_fact : just(month)) +
     			   maybeGetSizeFacts(sizeFacts, key) +
                   maybeGetActivityFacts(activityFacts, key)
             |
             str projectName <- projectNames,
             logToConsole("mergeFactsForProjects", "INFO merging facts for project: <projectName>."),
             factsMap sizeFacts := getSizeFacts(projectName),
             factsMap activityFacts := getActivityFacts(projectName),
             ks := domain(activityFacts) + domain(sizeFacts),
             size(ks) > 0 || (size(ks) == 0 && logToConsole("mergeFactsForProjects", "WARNING no size or activity facts for project: <projectName>")),
             factsKey key <- ks,
             <project,year,month> := key
            );
}

private map[maybeFactKey, Maybe[num]] maybeGetActivityFacts(factsMap activityFacts, factsKey key) {
	switch (maybeGetFromMap(activityFacts, key)) {
		case nothing():
				return (activityFactKey : nothing()
			    |
				activityFactKey <- activityFactKeys);

		case just(maybeFacts):		
			return maybeFacts;
	}
}

private map[maybeFactKey, Maybe[num]] maybeGetSizeFacts(factsMap sizeFacts, factsKey key) {
	switch (maybeGetFromMap(sizeFacts, key)) {
		case nothing():
				return (sizeFactKey : nothing()
			    |
				sizeFactKey <- sizeFactKeys);

		case just(maybeFacts):		
			return maybeFacts;
	}
}

public Maybe[&V] maybeGetFromMap (map[&K, &V] theMap, &K theKey) {
	if (theKey in theMap) {
		return just(theMap[theKey]);
	}
	else {
		return nothing();
	}
}

public factsMap mergeFactsForAllProjects () { 
	return mergeFactsForProjects(getProjectNamesInRepository());
}

public rel[str,str] getMetaDataElements(list[str] projectNames, str elementName) {
	return {
			<projectName, valueForProject>
			|
			projectName <- projectNames,
			valueForProject <- getMetaDataElements(projectName, elementName)
	};
}


public set[str] getMetaDataElements(str projectName, str elementName) {
	result = {};
	
	try
		top-down visit(getProjectMetaDataDOM(projectName)) {
			case element(_,
						 elementName,
						 [Node*,charData(str elementValue)]):
			{
	             result += {elementValue};
			}
		}
	catch: logToConsole("getMetaDataElements", "WARNING error while getting meta data facts for project: <projectName>.");
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

public factsMap getActivityFacts(str projectName)
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
				 			(loc_added_fact 		: just(toInt(LOCAddedAsString)),
				 			 loc_deleted_fact 		: just(toInt(LOCDeletedAsString)),
				 			 comments_added_fact 	: just(toInt(CommentsAddedAsString)),
				 			 comments_deleted_fact 	: just(toInt(CommentsDeletedAsString)),
				 			 blanks_added_fact		: just(toInt(BlanksAddedAsString)),
				 			 blanks_deleted_fact 	: just(toInt(BlanksDeletedAsString)),
				 			 commits_fact			: just(toInt(CommitsAsString)),
				 			 contributors_fact		: just(toInt(ContributorsAsString))));
	        }
		}
	catch: logToConsole("getActivityFacts", "WARNING error while getting activity facts for project: <projectName>.");
	return result;
}

public factsMap getSizeFacts(str projectName) {	   
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
				 			(loc_fact 					: just(toInt(LOCTotalAsString)),
				 			 comments_fact 				: just(toInt(CommentsAsString)),
				 			 blanks_fact 				: just(toInt(BlanksAsString)),
				 			 comment_ratio_fact 		: tryToReal(CommentRatioAsString),
				 			 cumulative_commits_fact	: just(toInt(CumulativeCommitsAsString)),
				 			 man_months_fact			: just(toInt(ManMonthsAsString))));
			}
		}
	catch: logToConsole("getSizeFacts", "WARNING error while getting size facts for project: <projectName>: ");
	return result;
}

public Maybe[real] tryToReal (str realAsString) {
	Maybe[real] r = nothing();
	try r = just(toReal(realAsString));
	catch IllegalArgument(): ;
	return r;
}

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

