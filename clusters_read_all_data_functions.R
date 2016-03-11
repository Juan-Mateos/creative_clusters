#Script with functions relevant for more than one clusters datasets

#Function to loads nspl data with the variables I am interested in
load_nspl <- function(variables,
                      path=
                           "metadata/NSPL_FEB_2016_csv/Data/NSPL_FEB_2016_UK.csv") {
     
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

#Make_keyed_frame function takes a vector or table and turns it into
#a dataframe where the first column are the row names

Make_keyed_frame <- function(x) {
     names <-  names(x)
     values <-  as.data.frame(x)
     row.names(values) <- NULL
     df <- data.frame(names,values)
     return(df)
}

#Create 2011 mapping DF
CreateTTWAmapping_df_2011 <- function(){
     #Read the Shapefiles
     uk <- readOGR("metadata/Travel_to_Work_Areas_(UK)_2011_boundaries_(super_generalised_clipped)_V3/",
                   layer="TTWA_2011_UK_BSC_V3")
     #Crete dfs.
     uk@data$id <- row.names(uk@data)
     uk.points <- fortify(uk,region="id")
     uk.ttwa_df <- join(uk.points,uk@data,by="id")
     return(uk.ttwa_df)
}


