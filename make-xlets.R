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
# This file contains a set of functions for taking a data frame with names and#
# ethnicity and creating a data frame of triplets and frequencies by ethnic   #
# group.                                                                      #
###############################################################################

## Functions

X.Lets <- function(name, pad = T, xlets = 3){
  # Returns a vector of substrings of length xlets.  E.g., if the string 
  # "abcdef" was to be broken into triplets (i.e., xlets=3), the output 
  # vector would be: ("abc", "bcd", "cde", "def").  The string is first padded
  # with '#' at the beginning and end of the string to mark start and end of 
  # the name.
  #
  # Args:
  #   name: A string representing a person's name
  #   pad: Boolean.  Indicates whether 'name' should be '#name#'
  #   xlets: the length of the substrings
  #
  # Returns:
  #   a vector of all the possible substrings
  #
  if( pad == T ){
    name <- paste('#', name, '#', sep='')  # Pad the name string with '#'
  }
  xletsVector <- c()  # create an empty vector to be returned by the function
  for( i in 1:(nchar(name) - (xlets-1))){  # for loop to cut up the name
    xletsVector <- c( xletsVector, substr(name, i, i+(xlets-1)) )   
  }
  return ( xletsVector )  # return the vector
}


XletLotsOfNames <- function(vectorOfNames, xlets=3){
  # Successivley applies the function X.Lets to a vector of names, returning a
  # vector of substrings
  # i.e., use the function 'lapply', and then 'unlist' the output
  # Args:
  #   vector.of.names: A vector of strings representing people's names
  #   xlets: the length of each name substring
  #
  # Returns:
  #   a vector of all the possible substrings of all the names 
  #   (repetitions are allowed)
  #
  pad <- F  # pad 'name' to '#name#'
  unlist( lapply( vectorOfNames, X.Lets, pad, xlets ) )   
}



CreateDF <- function (DF, nameColumn='name', ethnicityColumn='ethnic'){
  # Create a dataframe with a column with each possible xlet, and separate
  # columns for each ethnic group.  The data frame should be of the form
  # xlets  group1 ... groupn
  # The values for each group are set to NA
  #
  # Args:
  #   DF: the data frame that contains a column of names and ethnic groups
  #   nameColumn: a string with the name of the "names" column
  #   ethnicityColumn: a string with the name of the "ethnicity" column
  #
  # Returns:
  #   a data frame    
  #
  nEthnicGroups <- length( unique( DF[,ethnicityColumn] ) )
  uniqueSubstrings <- unique( XletLotsOfNames( DF[,nameColumn], ) )
  
  newDF <- data.frame(xlets = uniqueSubstrings, stringsAsFactors = FALSE)
  for(i in 1:nEthnicGroups){
    eval(parse(text=paste("newDF$group", toString(i), "=NA",sep="")))
  }   
  return(newDF)
}


CalcXletFreqs <- function(DF, nameColumn='name', ethnicityColumn='ethnic', xlets=3){
  # Use CreateDF to create an empty dataframe
  # Fill in the frequencies of each xlet under each ethnic group
  #
  # Args:
  #   DF: the data frame that contains a column of names and ethnic groups
  #   nameColumn: a string with the name of the "names" column
  #   ethnicityColumn: a string with the name of the "ethnicity" column
  #
  # Returns:
  #   a data frame    
  #
  # Create the data frame to hold the xlet frequencies and populate it with NA
  newDF <- CreateDF(DF, nameColumn, ethnicityColumn)
  # Work out the ethnic group column names
  groupNames <- names( table( DF[ , ethnicityColumn] ) )
  ethnicGroupCounter <- 0  # Set up a simple counter: ethnic group 1..n
  for( i in groupNames){  # Start a For-Loop: one loop for each ethnic group
    ethnicGroupCounter <- ethnicGroupCounter + 1  # increment ethnic grp count
    # Column names are "group" + str(ethnicGroupCounter)
    ethnicGroupColumName <- paste("group", ethnicGroupCounter, sep='')
    # In the noriginal names datafile, which rows belong the ethnic group i
    rowsForEthnicGroup <- which( DF[ , ethnicityColumn] %in% i )
    # Extract the names that belong to that ethnic group only
    namesForEthnicGroup <- DF[ rowsForEthnicGroup , nameColumn ]
    # Create the vector of all the xlets for that ethnic group's names
    xletsNamesEthnicGroup  <- XletLotsOfNames( namesForEthnicGroup, 3 )
    # Use the table command to count the occurrence of each of the xlets
    tableNamesEthnicGroup <- table( xletsNamesEthnicGroup )
    # Start a For-Loop to go through each xlet in the table
    for( j in 1:length( tableNamesEthnicGroup )){
      # Work out which Row in the Frequencies data frame fo current item
      xletRow <- which( newDF$xlets %in% names( tableNamesEthnicGroup )[j] )
      # In that Row, in the group_n column, insert the frequency
      newDF[ xletRow, ethnicGroupColumName ] <- tableNamesEthnicGroup[j]
    }   
  }
  return(newDF)  # Return the data frame with the frequency counts.
}
