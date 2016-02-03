# Naive Bayes method for the classification of ethnicity based on a name
In 2014 we published a paper describing a naive Bayes strategy for the classification of a person's ethnicity based on an analysis of their name. What was different about our approach was that we broke a person's name up into a set of overlapping "x-lets".  An x-let is a sequence of x-letters. In our case we used triplets -- 3-lets.  If the name was "Marco Rubio", the triplets of the name would be: "MAR", "ARC", "RCO", "CO ", "O R", etc.  We looked at the frequency of each x-let for each ethnic  group and used that to calculate the likelihoods.

You can find a more complete description of the paper here: http://www.ncbi.nlm.nih.gov/pubmed/24944286.

## Implementation
We have put together a set of R-scripts and data here so that those interested can see our general approach, and with luck they will improve upon it.  We provide some play data in the file "names-data.RData". The data are derived from some real data:  

* There are three ethnic groups ... group 1, group 2, and group 3
* There are an approximately equal numbers of people from each ethnic group in the data
* The data are divided into a training dataframe (n=5,184) and a test dataframe (n=5,424)
* The dataframes have a "name" column, containing the name as a character string, and an "actual_race" colum with an integer indicating the group membership.
* The names have been scrambled -- *do not* try to recover the unscrambled names.  While a name is not a secret, you don't need to know the actual names to see the workings of the approach.  And frankly, who cares if the name is "Marco Rubio" or "Donald Trump"; a rose by any other name ....

The "workflow-bayesnames.R" file is the work horse.  You can run that file and it will load the data, and the appropriate source files.  Read the file to get a sense of what's going on. As it runs, you can see a name fly by, and then each triplet of the name with the posterior "probabilities" for each group's membership associated with the triplet.

When it has finished running you can see the actual and predicted classifications for the training and test data with:

print(classDF.train.tab)  # Classification table for the training data

print(classDF.test.tab)  # Classification table for the test data

More detailed information is saved in: "BayesTestOutput.csv" and "BayesTrainOutput.csv"


