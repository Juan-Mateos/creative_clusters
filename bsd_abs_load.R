#R Script to read in data for the creative clusters update project.
######
#0. PREPARATION
######

#Read function scripts
source("Rcode/bsd_abs_load_functions.R")

######
#1.READ
######

#Read data from Excel spreadsheet which contains the information I need.
#Load worksheet to get sheetNames
enterpriseData <- loadWorkbook("vml_datasets_jan2016/spd - data for creative clusters - enterprise unit - 29-01-16.xls")
sheetNames <- names(getSheets(enterpriseData))

#Focus on sheet names with content (excluding those with ">>")
sheetNames.data <- sheetNames[!grepl(">>",sheetNames)]

#Read the data with the function ReadWorkSheet
#Enterprise data is a list of tables extracted from the enterprise spreadsheet.
enterprise.data <- lapply(sheetNames.data,
                          ReadWorkSheet,
                          x="vml_datasets_jan2016/spd - data for creative clusters - enterprise unit - 29-01-16.xls",
                          z=3)
names(enterprise.data) <- gsub("-","_",sheetNames.data)

#The sheet names are not sufficiently informative
#Create a better vector of names for the datasets, nicely formatted
datasetNames <- tolower(paste0(names(enterprise.data),
                               c(rep("",2),
                                 rep("_all_creative",3),
                                 rep("_subsector",2),
                                 rep("_all_industries",3))))

#Extract all the data-frames from the list
for (i in 1:length(enterprise.data)) {
     myDataFrame <- enterprise.data[[i]]
     
     #Some tidying of variable names
     names(myDataFrame) <- tolower(names(myDataFrame))
     
     #Remove rows with no data (including in the first column)
     missingRows <- is.na(myDataFrame[,1])
     myDataFrame <- myDataFrame[!missingRows,]
     
     #Remove columns with no data (or, one level )
     lastCol <- myDataFrame[,ncol(myDataFrame)]
     if (length(levels(lastCol)[1])==1) {
          myDataFrame <- myDataFrame[,-ncol(myDataFrame)]
     }
     
     #Rename
     name <- datasetNames[i]
     #We assign the dataframe to the name
     assign(name,myDataFrame)
     remove(myDataFrame)
}

#Read GVA data
#Country (with subsectoral data)
gva_country_data <- ReadWorkSheet(x="vml_datasets_jan2016/spd - data for creative clusters - GVA - 27-01-16.xls",
                                  y="country - scaled up",4) %>%
     select(-contains("enterprises")) %>% select(-contains("NA"))

#Regional (with only all cis data)
gva_region_data <- ReadWorkSheet(x="vml_datasets_jan2016/spd - data for creative clusters - GVA - 27-01-16.xls",
                                 y="region - scaled up",4) %>%
     select(-contains("enterprises")) %>% select(-contains("NA")) %>%
     rename(region=Region,industry=Industry,
            gva_thGBP_abs=Total.basic.GVA_THGBP,
            employment_abs=Total.employment,
            gva_per_employee_thGBP_abs=Average.GVA.per.employee,
            period=Period) %>%
     #NB there was some sort of coding error in the gva per employee data
     mutate(gva_per_employee_thGBP_abs=gva_thGBP_abs/employment_abs)

#We need to incorporate the NI data.
#Read it with ldply. 
ni_data <- ldply(list("gva"=4,"employment"=5), function(x){
     out <- read.xlsx("vml_datasets_jan2016/spd - data for creative clusters - GVA - 27-01-16.xls",
                    sheetIndex = x,startRow = 5,endRow = 15)
     names(out)[1] <- "sector"
     out
})


#Reshape the ni_data to rbind it with the gva_region_data above.
#This is fidgety because we need to adapt to the format above
ni_data_2 <- ni_data %>% filter(sector=="All Creative Enterprises") %>%
     melt(id.vars=c(".id","sector")) %>% 
     mutate(value=as.numeric(value)) %>%
     dcast(sector+variable~.id,value.var="value") %>%
     mutate(region="Northern Ireland",
            industry="All creative industries",
            variable = as.numeric(gsub("X","",variable))) %>%
     select(region,industry,
            gva,employment,variable) %>%
     rename(gva_thGBP_abs=gva,employment_abs=employment) %>%
     mutate(period = ifelse(variable<=2010,"2008_10","2011_14")) %>%
     select(-variable) %>% group_by(period) %>%
     summarise(region=unique(region),
               industry=unique(industry),
               gva_thGBP_abs=mean(gva_thGBP_abs),
               employment_abs=mean(employment_abs)) %>%
     mutate(gva_per_employee_thGBP_abs=1000*gva_thGBP_abs/employment_abs) %>%
     select(region,industry,
            gva_thGBP_abs,employment_abs,gva_per_employee_thGBP_abs,
            period)

gva_region_data_all <- rbind(gva_region_data,ni_data_2)

#####
#2.CLEAN
#####
ttwa.all <- BindAreas(ttwa_2007_all_creative,ttwa_2010_all_creative,
                          ttwa_2014_all_creative) %>% tbl_df() %>%
     rename(ttwa.code=ttwa.code..2011.,
            ttwa.name=ttwa.name..2011.)

#NB there was an empty column #17 in ttwa_2011_14
ttwa.sub <- BindAreas(ttwa_2007_10_subsector,
                      ttwa_2011_14_subsector[,1:16]) %>% tbl_df() %>%
     rename(ttwa.code=ttwa.code..2011.,
            ttwa.name=ttwa.name..2011.)

#AND THEY USED 2011 TTWAS

######
#3. ESTIMATE GVA
######

#Estimate average employment in CIs per TTWA in 2008/9 and 2010/13
ttwa.all_gva1 <- tbl_df(ttwa.all) %>% 
     select(year,ttwa.name,ttwa.code,employment,business.count,turnover,
            contains("location.quotient"),contains("Proportion")) %>%
     rename(emp.lq=location.quotient...employment,
            business.lq=location.quotient...business.count,
            turnover.lq=location.quotient..turnover,
            business.share=proportion.of.local.enterprises.in.sector,
            employment.share=proportion.of.local.employment.in.sector,
            turnover.share.turnover=proportion.of.local.turnover.in.sector)

#Merge with TTWA employment data for all sectors - we use this
#data later for normalisation
ttwa.all_gva2 <- merge(ttwa.all_gva1,
                                ttwa.all.industries,
                                by.x=c("year","ttwa.code"),
                                by.y=c("year","ttwa.code..2007."),
                                all.x=T)

#Modify ttwa codes to match with a lookup later
ttwa.all_gva2$ttwa.code <- 
     sapply(ttwa.all_gva2$ttwa.code,
                                           function(x){
                                                x <- as.character(x)
                                                if(nchar(x)==1) {
                                                     out <- paste0("00",x)
                                                     return(out)
                                                } else if (nchar(x)==2) {
                                                     out <- paste0("0",x)
                                                     return(out)
                                                } else {
                                                     return(x)}
                                           })
#Read in TTWA-GOR lookup
lookup_ttwa.gor <- read.csv("final-report-metadata-lookups/ttwa.gor.codes_lookup2-2015-11-26.csv")

#Merge
ttwa.all_gva3 <- merge(ttwa.all_gva2,
                                lookup_ttwa.gor[,c("ttwa.code","gor.name")],
                                by.x="ttwa.code",
                                by.y="ttwa.code",
                                all.x=T) %>% tbl_df()

#Create a "period" variable for matching.
ttwa.all_gva3$period <- sapply(ttwa.all_gva3$year,
                                        function(x){
                                             if (x==2007) {return("first")
                                             } else if (x==2013) {
                                                  return("second")
                                             } else {return("middle")}
                                        })

#Do the same thing in the GVA data source
gva.region.data$period.2 <- sapply(gva.region.data$Period, function(x){
     if(x=="2008_09") {return("first")
     } else {return("second")}
})

#Merge by both
ttwa.all_gva4 <- merge(ttwa.all_gva3,
                                gva.region.data,
                                by.x=c("period","gor.name"),
                                by.y=c("period.2","Region"),
                                all.x=T) %>% tbl_df()

#Clean dataset with all statistics for creative industries
ttwa.all_clean1 <- ttwa.all_gva4 %>% 
     select(-ttwa.code) %>%
     rename(gva.per.worker=Average.GVA.per.employee) %>%
     mutate(gva.creativeThGBP = employment * gva.per.worker)

