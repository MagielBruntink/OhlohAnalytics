SAMPLING SOFTWARE PROJECTS
==========================
(c) Microsoft Corporation. All rights reserved. 


Implementation of algorithms to sample software projects for empirical
research. The implementation is provided as an R package and provides
two functions: 

(1) Score a selection of software projects (sample) with respect to
how representative it is of a broader population. 

(2) Recommend additional projects from the broader population in order
to maximize the score from the previous step.

For the license, please see file License.txt

For more information, please read the following paper:

@inproceedings{nagappan-esecfse-2013,
 title = "Diversity in Software Engineering Research",
 author = "Meiyappan Nagappan and Thomas Zimmermann and Christian Bird",
 year = "2013",
 month = "August",
 booktitle = "ESEC/FSE '13: Proceedings of the 9th joint meeting of the 
European Software Engineering Conference and the ACM SIGSOFT Symposium 
on the Foundations of Software Engineering",
 location = "Saint Petersburg, Russia",
 publisher = "ACM",
 } 

If you use this package for your research, please cite the above
paper. http://research.microsoft.com/apps/pubs/default.aspx?id=193433

For questions, please contact christian.bird @ microsoft.com and 
tzimmer @ microsoft.com

For sample data, please visit
http://sailhome.cs.queensu.ca/replication/representativeness/


USAGE: How to compute the score?

This example below uses the Ohloh universe to score the Mozilla
Firefox project along the space (Lines of Code, Developers). The text
id ~ total_code_lines + twelve_month_contributor_count is R syntax and
commonly used to define models.

url <- "http://sailhome.cs.queensu.ca/replication/representativeness/masterdata.txt"
ohloh <- read.delim(url, header=T, na.strings=c("", "NA"))
sample <- ohloh[ohloh$name=="Mozilla Firefox",]
score <- score.projects(sample, universe=ohloh, id ~ total_code_lines + twelve_month_contributor_count)
The resulting total score is in score$score and the dimension scores are in score$dimension.score.


USAGE: How to select the next projects?

This example adds 10 more projects to the sample from the previous
example. The result is a data frame np$new.projects with the projects
to be added to the sample and the score object of the combined sample
np$score.

np <- next.projects(10, sample, universe=ohloh, id ~ total_code_lines + twelve_month_contributor_count)


USAGE: How to change the configuration?

Provide a list with the similarity functions. Values NA indicates that
the default similarity function should be used for a dimension. In the
example below the function custom.similarity will be used the first
dimension.

score <- score.projects(sample, universe=ohloh, id ~ total_code_lines + twelve_month_contributor_count, configuration=c(custom.similarity, NA))


CHANGELOG:
0.1 Initial Release
0.1.1 Updated Readme.txt with the ESEC/FSE'13 paper