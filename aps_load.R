#######
#READ AND CLEAN APS DATA
#######
#Load functions used to load and clean the APS data
source("Rcode/aps_load_functions.R")

#Set working directory
setwd("/Users/juanmateos-garcia/Desktop/APS-data/")

#######
#Read metadata
#######
#Creative industries for Creative Clusters
#Read and tidy metadata (SIC and SOC codes)
ce.metadata_df <- read.csv("Metadata/ce_codes.csv")
#ce.metadata_df$code <- gsub("\\.","",ce.metadata_df$code)

#Create dataframes with CE sics and socs
ce_sics <- subset(ce.metadata_df,type=="SIC")
ce_socs <- subset(ce.metadata_df,type=="SOC")

######
#READ DATA
######
#Read and clean data

aps_2014 <- GetAPSData("APSP_JD14_Nestav2.csv")
aps.2014_ttwa <- GetJobsReport(aps_2014,"ttwa.name","2014")

aps_2011 <- GetAPSData("APSP_JD11_Nestav2.csv")
aps.2011_ttwa <- GetJobsReport(aps_2014,"ttwa.name","2014")

setwd("/Users/juanmateos-garcia/Desktop/Creative Clusters")





