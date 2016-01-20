# Libraries
library(stringr)
library(epicalc)


## Function

Classify <- function(prMalay, prIndian, prChinese){
  return(which.max(c(prMalay, prIndian, prChinese)))
}

SensSpec <- function(tab){
  
  tp <- tab[4]
  fn <- tab[3]
  sensitivity <- tp/(tp+fn)
  
  tn <- tab[1]
  fp <- tab[2]
  specificity <- tn/(tn+fp)
  
  return(c(sensitivity, specificity))
}


## Constants


## DO

# Load the test and training data
load("names-data.RData")

# Load the R file for processing names into xlets
source("make-xlets.R")
# Load the R fiel for calculating likelihoods and estimating bayes
source("bayes.R")

# Calculate the triplets frequency for each triplet in each ethnic group
datafreq <- CalcXletFreqs(trainDF, 'name', 'actual_race', xlets=3)
# Apply a laplace smoothing; i.e., when a triplet doesn't exist for an ethnic
# group, add a small, non-zero constant
datafreq <- LaplaceSmoothing(datafreq)
# Calculate the likelihoods fr each xlet for each group
dataLik <- LikelihoodGroupGvnData(datafreq)


# Set up the database for the results
classDF <- cbind(name='Name', race1='.01', race2='.01', race3='.01')
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

# Round the numbers
classDF[,2:4] <- round(classDF[,2:4], 3)

# Add the actual Race
classDF$ethnic <- data$actual_race

# Classify final group membership based on the probabilities
classDF$pred <- NA

for(i in 1:length(classDF$pred)){
  classDF$pred[i] <- Classify(classDF$group1[i], classDF$group2[i], classDF$group3[i])
}

# Get the 3x3 classification table
classDF.tab <- table(classDF$pred, classDF$ethnic)
# Calculate the Kappa statistics
kap(classDF.tab)


# Write out the results as a csv
write.csv(classDF, file = "BayesOutput.csv")




#######################

# Read the test data
dataT <- read.csv('test.csv', sep='\t', header=T, stringsAsFactors=F)
dataT$actual_race <- as.integer(dataT$actual_race)

#  To try this out, remove non-Malay, Indian, and Chinese
dataT <- dataT[ which(!dataT$actual_race==0), ]  # remove people whose race is '0'
dataT <- dataT[ which(!dataT$actual_race>3), ]  # remove non-Malay, Chinese, Indian

# Remove as many typos from the names data
dataT <- CleanNames(dataT, 'residents_name')


# Set up the database for the results
classDFT <- cbind(name='Name', malay='.01', indian='.01', chinese='.01')
classDFT <- classDFT[-1, ]  # Destroy the first row so it is an empty DF

# Loop through every name in the data DF, classify the names, and save
for(i in 1:length(dataT$residents_name)){
  print(i)
  newName <- dataT$residents_name[i]
  classOfName <- NameBayes(priors , newName, dataLik)
  classDFT <- rbind(classDFT, classOfName)
}
remove( classOfName )  # Clean up the tmp dataframe

# Round the numbers
classDFT[,2:4] <- round(classDFT[,2:4], 3)

# Add the actual Race
classDFT$ethnic <- dataT$actual_race

classDFT$pred <- NA

for(i in 1:length(classDFT$pred)){
  classDFT$pred[i] <- Classify(classDFT$group1[i], classDFT$group2[i], classDFT$group3[i])
}

classDFT.tab <- table(classDFT$pred, classDFT$ethnic)

kap(classDFT.tab)


## Sensitivity and specificity
# Training Data
actMalay <- ifelse(classDF$ethnic==1, 1, 0)
predMalay <- ifelse(classDF$pred==1, 1, 0)
SensSpec(table(predMalay, actMalay))

actInd <- ifelse(classDF$ethnic==2, 1, 0)
predInd <- ifelse(classDF$pred==2, 1, 0)
SensSpec(table(predInd, actInd))


actChin <- ifelse(classDF$ethnic==3, 1, 0)
predChin <- ifelse(classDF$pred==3, 1, 0)
SensSpec(table(predChin, actChin))


# Test Data
actMalay <- ifelse(classDFT$ethnic==1, 1, 0)
predMalay <- ifelse(classDFT$pred==1, 1, 0)
SensSpec(table(predMalay, actMalay))

actInd <- ifelse(classDFT$ethnic==2, 1, 0)
predInd <- ifelse(classDFT$pred==2, 1, 0)
SensSpec(table(predInd, actInd))


actChin <- ifelse(classDFT$ethnic==3, 1, 0)
predChin <- ifelse(classDFT$pred==3, 1, 0)
SensSpec(table(predChin, actChin))



# Write out the results as a csv
write.csv(classDFT, file = "BayesOutputT.csv")
