#######
#READ AND CLEAN APS DATA
#######

#Set working directory
setwd("/Users/juanmateos-garcia/Desktop/APS-data/")

#######
#Read metadata
#######
#Creative industries for Creative Clusters
#Read and tidy
ce.metadata_df <- read.csv("Metadata/ce_codes.csv")
#ce.metadata_df$code <- gsub("\\.","",ce.metadata_df$code)

#Create dataframes with CE sics and socs
ce_sics <- subset(ce.metadata_df,type=="SIC")
ce_socs <- subset(ce.metadata_df,type=="SOC")

#TTWA Lookup
ttwa.lu <- read.delim("/Users/juanmateos-garcia/Desktop/2015 core/Technation/Analysis/Datasets/NSPL_AUG_2014_csv (1) copy/Documents/TTWA names and codes UK as at 12_10.txt")



#GetAPSData function that takes an aps dataset and returns the clean version.

GetAPSData <- function(aps.dataset) {
     #Read data (selected columns)
     aps_test <- read.csv(aps.dataset,nrow=10)
     
     names(aps_test) <- tolower(names(aps_test))
     
     #My variables
     aps.analysis.vars <- c("inecac05",
                            "indsc07m","indsc07s",
                            "soc10m","soc10s",
                            "ttwa07","ttwa08",
                            "country","nuts3",
                            "pwta14")
     
     #Get their indices for reading the data in
     aps.var.index <- grep(paste(aps.analysis.vars,collapse="|"),
                           names(aps_test))
     
     col.classes <- rep("NULL",ncol(aps_test))
     col.classes[aps.var.index] <- NA
     
     #Read the data (in a raw format to begin with) 
     aps_rawish_df <- read.csv(aps.dataset,colClasses = col.classes)
     
     #Convert into table
     aps_selected.qs <- tbl_df(aps_rawish_df)
     
     #Reorder variables
     names(aps_selected.qs) <- tolower(names(aps_selected.qs))
     aps_selected.qs.2 <- aps_selected.qs[,order(colnames(aps_selected.qs),decreasing=F)] %>%
          tbl_df()
     
     names(aps_selected.qs.2) <- c("country",
                                   "sic.2007","sic.2007s",
                                   "econ.activity",
                                   "nuts3",
                                   "weight",
                                   "soc.2010","soc.2010s","ttwa.code.1",
                                   "ttwa.code.2")
     
     #Codes -8 and -9 are codes for NA 
     aps_selected.qs.2[aps_selected.qs.2==-8 |
                            aps_selected.qs.2==-9] <- NA
     
     aps_selected.qs.2[aps_selected.qs.2=="-8" |
                            aps_selected.qs.2=="-9"] <- NA
     
     #Create factor versions of econ.activity and country
     aps_selected.qs.2$econ.activity_factor <- factor(aps_selected.qs.2$econ.activity,
                                                      levels=c(1,2),
                                                      labels=c("employed",
                                                               "self-employed"))
     
     aps_selected.qs.2$country_factor <- factor(aps_selected.qs.2$country,
                                                levels=c(1:5),
                                                labels=c("england","wales",
                                                         "scotland","scotland north of caledonian canal",
                                                         "northern ireland"))
     
     #Create 4-digit SIC codes which gives us a "processed" dataframe
     aps_processed <- aps_selected.qs.2 %>% mutate(sic4.2007=
                                                        substring(sic.2007,0,4),
                                                   sic4.2007s=
                                                        substring(sic.2007s,0,4))
     
     #Create CE dummies for these SIC/SOC codes
     aps_processed$ce.sic <- as.numeric(aps_processed$sic4.2007 %in% ce_sics$code,
                                        aps_processed$sic4.2007s %in% ce_sics$code)
     
     aps_processed$ce.soc <- as.numeric(aps_processed$soc.2010 %in% ce_socs$code |
                                             aps_processed$soc.2010s %in% ce_socs$code)
     
     #Make them factors
     aps_processed$ce.sic_factor <- factor(aps_processed$ce.sic,
                                           levels=c(0,1),
                                           labels=c("no CE sector","CE sector"))
     
     aps_processed$ce.soc_factor <- factor(aps_processed$ce.soc,
                                           levels=c(0,1),
                                           labels=c("no CE occupation","CE occupation"))
     
     aps_geocoded <- merge(aps_processed,ttwa.lu[,c("TTWA07CD","TTWA07NM.1")],
                           by.x="ttwa.code.2",
                           by.y="TTWA07CD") %>% tbl_df() %>% 
          rename(ttwa.name=TTWA07NM.1)
     
     aps_final <- aps_geocoded %>% filter(econ.activity==1 |
                                               econ.activity==2)
     
     return(aps_final)
}

#GetJobsReport function takes an APS table and an area to summarise by and returns:
#Total ICT employment, occupations, economy
#% embeddedness
#Total employment in rest of economy
GetJobsReport <- function(my.aps.data) {
     myDf <- my.aps.data %>% tbl_df() %>% filter(ttwa.name!="")
     ttwa.report <- myDf %>% 
          mutate(ce.embed= ce.soc==1 & ce.sic==0,
                 ci.self=ce.sic==1 & econ.activity==2) %>%
          group_by(ttwa.name) %>%
          summarise(n=n(),
                    all.jobs=sum(weight),
                    ce.sic.n=sum(ce.sic),
                    ce.sic.jobs=sum(weight*ce.sic),
                    ce.soc.n=sum(ce.soc),
                    ce.soc.jobs=sum(weight*ce.soc),
                    ce.embed.n=sum(ce.embed),
                    ce.embed.jobs = sum(weight*ce.embed),
                    ce.econ.n=ce.sic.n + ce.soc.n + ce.embed.n,
                    ce.econ.jobs = ce.sic.jobs + ce.embed.jobs,
                    ce.share= Percentify(ce.sic.jobs/all.jobs),
                    ci.self.n=sum(ci.self),
                    ce.self.jobs=sum(weight*ci.self),
                    ce.self.share=ce.self.jobs/ce.sic.jobs)
     return(ttwa.report)
}

aps_2014 <- GetAPSData("APSP_JD14_Nestav2.csv")
aps.2014_ttwa <- GetJobsReport(aps_2014) %>%
     mutate(period="second")

ttwa.all_clean <- merge(ttwa.all_clean1,
                        aps.2014_ttwa,
                        by.x=c("ttwa.name","period"),
                        by.y=c("ttwa.name","period"),
                        all.x=TRUE) %>% tbl_df()
