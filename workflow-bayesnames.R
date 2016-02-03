## Classify Function
# Attribute the class to the most likely (i.e, with the greatest probability)
Classify <- function(pr_group1, pr_group2, pr_group3){
  return(which.max(c(pr_group1, pr_group2, pr_group3)))
}

# Load the test and training data
load("names-data.RData")

# Load the R file for processing names into xlets
source("make-xlets.R")
# Load the R file for calculating likelihoods and estimating bayes
source("bayes.R")

######## Use the training data to estimate the empirical likelihoods
# Calculate the triplets frequency for each triplet in each ethnic group
datafreq <- CalcXletFreqs(trainDF, 'name', 'actual_race', xlets=3)
# Apply a laplace smoothing; i.e., when a triplet doesn't exist for an ethnic
# group, add a small, non-zero constant
datafreq <- LaplaceSmoothing(datafreq)
# Calculate the likelihoods for each xlet for each group
dataLik <- LikelihoodGroupGvnData(datafreq)



######## Evaluate the naive bayes for classifying the training data
# Set up the database for the results
classDF <- cbind(name='Name', race1=.01, race2=.01, race3=.01)
classDF <- classDF[-1, ]  # Destroy the first row so it is an empty DF

# Loop through every name in the data DF, classify the names, and save
priors <- c(1/3, 1/3, 1/3)
for(i in 1:length(trainDF$name)){
  print(i)
  newName <- trainDF$name[i]
  classOfName <- NameBayes(priors , newName, dataLik)
  classDF <- rbind(classDF, classOfName)
}
remove( classOfName )  # Clean up the tmp dataframe
classDF[1] <- lapply(classDF[1], as.character)  # Make sure that "name" is a string not a factor
classDF[, 2:4] <- lapply(classDF[, 2:4], as.numeric) # Make sure that posteriors are numeric



# Round the numbers
classDF[,2:4] <- round(classDF[,2:4], 3)

# Add the actual Race
classDF$ethnic <- trainDF$actual_race

# Classify final group membership based on the probabilities
classDF$pred <- NA

for(i in 1:length(classDF$pred)){
  classDF$pred[i] <- Classify(classDF$group1[i], classDF$group2[i], classDF$group3[i])
}

# Get the 3x3 classification table
classDF.train.tab <- table(classDF$pred, classDF$ethnic)
print(classDF.train.tab)

# Write out the results as a csv
write.csv(classDF, file = "BayesTrainOutput.csv")




######## Evaluate the naive bayes for classifying the test data

# Set up the database for the results
classDF <- cbind(name='Name', race1=.01, race2=.01, race3=.01)
classDF <- classDF[-1, ]  # Destroy the first row so it is an empty DF

# Loop through every name in the data DF, classify the names, and save
priors <- c(1/3, 1/3, 1/3)
for(i in 1:length(testDF$name)){
  print(i)
  newName <- testDF$name[i]
  classOfName <- NameBayes(priors , newName, dataLik)
  classDF <- rbind(classDF, classOfName)
}
remove( classOfName )  # Clean up the tmp dataframe
classDF[1] <- lapply(classDF[1], as.character)  # Make sure that "name" is a string not a factor
classDF[, 2:4] <- lapply(classDF[, 2:4], as.numeric) # Make sure that posteriors are numeric



# Round the numbers
classDF[,2:4] <- round(classDF[,2:4], 3)

# Add the actual Race
classDF$ethnic <- testDF$actual_race

# Classify final group membership based on the probabilities
classDF$pred <- NA

for(i in 1:length(classDF$pred)){
  classDF$pred[i] <- Classify(classDF$group1[i], classDF$group2[i], classDF$group3[i])
}

# Get the 3x3 classification table
classDF.test.tab <- table(classDF$pred, classDF$ethnic)
print(classDF.test.tab)

# Write out the results as a csv
write.csv(classDF, file = "BayesTestOutput.csv")

