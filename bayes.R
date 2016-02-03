# This file is part of the set of files for developing a naive Bayes classifier
# of ethnicity.
# 
# Copyright (C) 2013  Daniel D Reidpath and Kridaraan Komahan
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# 
###############################################################################
# This file contains a set of functions for calculating the Bayesian probs.   #
# of a name given a dataframe of xlet frequencies. Not that 'xlet' refers to  #
# a number of letters, in this case a triplet of letters from a name.  If the #
# is "Daniel" then the xlets will be DAN ANI NIE IEL                          #
###############################################################################

# Note the construction of an xlet dataframe is
# [ , xlet, ethnicgrp_1, ethnicgrp_2, ..., ethnicgrp_n ]
# This means columns 2 .. length(a_dataframe_row) are the columns for each 
# ethnic group.

## Functions

LaplaceSmoothing <- function(xletDF, alpha=.005){
  # Takes a Data Frame of xlets and returns a Data Frame of xlets in which all 
  # NA have been replaced by a factor alpha.  In Laplace smoothing this often
  # has a value of 1, but could be much smaller. In this case .005.
  #
  # Args:
  #   xletDF: A dataframe with frequencies for xlets in each ethnic group
  #   alpha: the replace ment value for the NAs
  #
  # Returns:
  #   a DataFrame with NAs replaced by the value alpha
  #
  xletDF[is.na(xletDF)] <- alpha  # Replace every NA with alpha
  return(xletDF)  # Return the Laplace smoothed frequencies
}


PrDataGvnGroup <- function( xletDF ){
  # Takes a Data Frame of xlet frequencies (with Laplace smoothing) and
  # Calculate Pr( xlet | ethnicity ).  This is the probability of each xlet
  # GIVEN the ethnic group (i.e., column)
  #
  # Args:
  #   xletDF: A dataframe with smoothed frequencies of xlets
  #
  # Returns:
  #   a DataFrame with Pr( xlet | ethnicity ) replaced by the value alpha
  #
  ethnicCols <- 2:length( xletDF[ 1, ] )  # Which columns are the groups
  ethnicColTotals <- colSums( xletDF[ ethnicCols ] ) # column totals
  xletDF[ ethnicCols ] <- xletDF[ ethnicCols ]/ethnicColTotals # Pr(D|Group)
  return( xletDF )
}



LikelihoodGroupGvnData <- function( xletDataGvnGroupDF ) {
  # Takes a Data Frame of Pr( xlet | ethnicity ) and calculates
  # the likelihoods:
  #
  # Args:
  #   xletDataGvnGroupDF: A dataframe with Pr( xlet | ethnicity )
  #
  # Returns:
  #   a DataFrame of likelihoods
  #
  tmpDF <- xletDataGvnGroupDF
  ethnicCols <- 2:length( tmpDF[ 1, ] )  # Which columns
  # Calculate the likelihoods
  tmpDF[ ethnicCols  ] <- tmpDF[ ethnicCols ] / rowSums( tmpDF[ ethnicCols ] )
   
return(tmpDF)  # Return the dataframe of likelihoods
}


Bayes <- function (priors, likelihoods){
  # Takes a vector of priors and a vectors of likelihoods
  # And returns the posterior
  #
  # Args:
  #   priors: a vector of prior probabilities for each ethnic group
  #   likelihoods: a vector of likelihoods for each ethnic group
  #
  # Returns:
  #   a vector of posterior probabilities for each ethnic group
  #
  
  updatedLikelihoods <- priors * likelihoods  # vector multiply 
  normaliser <- sum(updatedLikelihoods)  # sum them for denominator
  posteriors <- updatedLikelihoods/normaliser # P( Ethnic | xlet)
  
  return(posteriors)
  
}


NameBayes <- function (priors, name, likelihoodDF, pad=F, xletSize=3){
  # This function calculates the Pr( Ethnicicty | Name )
  #
  # Args:
  #   priors: a vector of prior probabilities for each ethnic group
  #   name: a string representing a person's name
  #   likelihoodDF: a dataframe with:
  #      a column for the xlets (xlet)
  #      a colum for each group representing the likelihoods
  #   pad: add '#' to the beginning or end of a name (Boolean)
  #   xletSize: the size of the substring (integer)
  #
  # Returns:
  #   a vector of posterior probabilities for each ethnic group
  #
  print(name)
  print( priors )
  
  ethnicCols <- 2:length( likelihoodDF[ 1, ] )  # Which columns represent groups
  
  
  nameXlets <- X.Lets(name, pad, xletSize)
  for( i in 1:length( nameXlets ) ){
    if(nameXlets[i] %in% likelihoodDF[ , 1 ]){
      row <- which(nameXlets[i] == likelihoodDF[ , 1 ])
      xletLikelihoods <- likelihoodDF[ row , ethnicCols ]
      priors <- Bayes(priors, xletLikelihoods)
      cat(nameXlets[i])
      print(priors)
    }
    else{
    }
  }
  # Return a one row data frame with the name and the posterior probabilities
  returnDF <- data.frame( cbind( name, rbind( priors ) ) )
  names(returnDF) <- c("name", "group1", "group2", "group3")
  return( returnDF ) 
}





