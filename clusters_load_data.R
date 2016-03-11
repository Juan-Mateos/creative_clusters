#Read and prepare data script for Creative Clusters project

#######
#SUMMARY
#######
#Read and combine:
#Core datasets
#Business (ABS + BSD)
#APS
#Secondary datasets
#HESA
#REF
#HE-BCI
#Meetup (with scraping)
#Outputs:
#Core output:
# Dataframe with 2014 statistics, change over period,
#contemporary statistics for Secondary datasets
#Other outputs:
#TBC

#######
#PREPARATIONS
#######

#Set working directory
setwd("/Users/juanmateos-garcia/Desktop/Creative Clusters")

#Install generic packages
source("Rcode/genericPackages-copy.R")

#Load utility functions
source("Rcode/myUtilityFunctions-copy.R")

#Load cross-datasets functions
source("Rcode/clusters_read_all_data_functions.R")

#Output path
opath <- "/Users/juanmateos-garcia/Desktop/Creative Clusters/markdown outputs/"

########
#LOAD METADATA
########
#Load nspl for matching postcodes with 2011 TTWAs
variables <- c("pcd","pcd2","pcds","ttwa","gor")
nspl2015 <- load_nspl(variables)

#Load nspl ttwa labels for matching
#TTWAs
ttwa_names <- read.delim(
     "metadata/NSPL_FEB_2016_csv/Documents/TTWA names and codes UK as at 12_11 v5.txt")
#GORs
gor_names <- read.delim(
     "metadata/NSPL_AUG_2015_csv/Documents/GOR names and codes EN as at 12_10.txt")

#Merge with nspl for labelling
nspl2015_labelled_tmp <- merge(nspl2015,ttwa_names,by.x="ttwa",by.y="TTWA11CD",
                               all.x=T) %>% rename(ttwa.name=TTWA11NM)
nspl2015_labelled_tmp2 <- merge(nspl2015_labelled_tmp,
                                gor_names,by.x="gor",by.y="GOR10CD",
                                all.x=T) %>% rename(gor.name=GOR10NM,
                                                    gor.code=gor)
#Include names for Scotland, NI and Wales
#Need to add them as levels first

levels(nspl2015_labelled_tmp2$gor.name) <- c(levels(nspl2015_labelled_tmp2$gor.name),
                                             "Northern Ireland","Wales","Scotland")
#Then transform
nspl2015_labelled_tmp2$gor.name[grepl("^N",nspl2015_labelled_tmp2$gor.code)] <- 
     "Northern Ireland"
nspl2015_labelled_tmp2$gor.name[grepl("^W",nspl2015_labelled_tmp2$gor.code)] <- 
     "Wales"
nspl2015_labelled_tmp2$gor.name[grepl("^S",nspl2015_labelled_tmp2$gor.code)] <- 
     "Scotland"

#NB some missing values here - in the Channel Islands and Isle of Man
nspl2015_labelled <- nspl2015_labelled_tmp2 %>% tbl_df() %>%
     select(pcd,pcds,ttwa,ttwa.name,gor.code,gor.name)

#######
#LOAD CORE DATASETS (BSD, APS AND ABS)
#######
#Load bsd and abs data
source("Rcode/bsd_abs_load.R")

#For the purposes of analysis,
#we will consider separately the subsectoral and the
#sectoral data.

#all.cis_ttwa contains yearly (2007,2010,2014) BSD and ABS data for
#all_creative industries.
#Remove unnecessary codes
all.cis_ttwa <- all.cis_ttwa %>%
     select(-2,-4)

WriteOut(all.cis_ttwa,opath)

#subsectors_ttwa contains data for two periods (2007_2010 and 2011_2014)
#in all subsectors. 
#Remove unnecessary codes.
subsectors_ttwa <- subsectors_ttwa %>%
     select(-c(3,5))

#Shorter, cleaner labels
shorter.labels <- as.list(c("Advertising",
                            "All creative industries",
                            "Architecture",
                            "Design",
                            "Film, radio & TV",
                            "Software & digital",
                            "Music & performing arts",
                            "Publishing"))
#Name the list
names(shorter.labels) <- levels(subsectors_ttwa$industry)

#Reallocate (as if it was a dictionary)
subsectors_ttwa$industry.short <- unlist(shorter.labels[subsectors_ttwa$industry])

#Remove some garbage data at the end of the table
subsectors_ttwa <- subsectors_ttwa %>% extract(1:(nrow(subsectors_ttwa)-3),) %>%
     filter(industry!="All creative industries")

#Write out in the markdown folder
WriteOut(subsectors_ttwa,opath)

#Load aps data
source("Rcode/aps_load.R")

#We have extracted APS data for two years:
#2011: aps_2011 contains respondent level information; 
#aps.2011_ttwa contains area level creative economy information.
#2014: sames as above but for 2014.

#Write out in the markdown folder
WriteOut(aps.2014_ttwa,opath)

#######
#LOAD SECONDARY DATASETS
#######

#Load HESA data
source("Rcode/HESA_data_28Oct2015.R")

#ttwa.subject.year has subject by ttwa graduates totals
#ttwa.jacs.2013.14 has two elements:
#1: Graduates per TTWA in all JACS subjects
#2: Graduates per TTWA in top 15 JACS subjects (this is a list
#of 15 dfs with that information)


#Load REF data
source("Rcode/ref_load.R")
#The main dataset is ref.outcomes_with.lq - it contains
#scores by discipline for different variables.


#Load HE-BCI data
source("Rcode/he-bci-load.R")
#The main dataset is he.bci_cc -it contains area level information
#across a range of measures of activity. 

#Need to scrape meetup data





