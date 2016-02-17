#Script with functions relevant for more than one clusters datasets

#Function to loads nspl data with the variables I am interested in
load_nspl <- function(variables,
                      path=
                           "metadata/NSPL_AUG_2015_csv/Data/NSPL_AUG_2015_UK.csv") {
     
     #Load a test dataset to extract the locations of the right variables
     nspl_test <- read.csv(path,nrow=10)
     
     #Get an index of col names I am interested in
     nspl.vars.index <- grepl(
          paste0(variables,
                 collapse="|"),names(nspl_test))
     
     #Create a NULL vector to skip columns in the dataset and
     #then replace the variables of interest with NAs
     column_selector <- rep("NULL",length(names(nspl_test)))
     column_selector[nspl.vars.index] <- NA
     
     my_nspl <- read.csv(path,colClasses = column_selector)
     return(my_nspl)
}