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

gva_region_data_creative <- rbind(gva_region_data,ni_data_2)

#Read GVA data (all sectors) (including NI added by hand)
gva_region_all_sectors <- ReadWorkSheet(x="vml_datasets_jan2016/spd - data for digital tech clusters - GVA - 13-01-16 STC.xls",
                                y="region_all",z=3) %>% select(-1)
names(gva_region_all_sectors)[3:6] <- paste0(names(gva_region_all_sectors)[3:6],"_abs")

#####
#2.CLEAN
#####

#1. ttwa.all data
ttwa.all <- BindAreas(ttwa_2007_all_creative,ttwa_2010_all_creative,
                          ttwa_2014_all_creative) %>% tbl_df() %>%
     rename(ttwa.code=ttwa.code..2011.,
            ttwa.name=ttwa.name..2011.)

     #Label TTWAs and allocate them into regions (in the ttwa.all
          #dataset we are matching with GVA data)
     #Allocate TTWAs into regions.
          #Preamble-Create table to apply over 
          ttwa_region_table <- table(nspl2015_labelled$ttwa.name,
                                            nspl2015_labelled$gor.name)
          ttwa_region_df <- data.frame(ttwa=row.names(ttwa_region_table),
                                       as.data.frame.matrix(ttwa_region_table,row.names = F)) %>%
               filter(ttwa!="")
          
          #Apply to obtain region-ttwa pairs
          region_ttwas <- do.call(rbind,apply(ttwa_region_df,1,
                                function(x){
                                     top_reg <- names(x)[which.max(x)]
                                     return(data.frame(ttwa.name=x[1],region=top_reg))
                                })) %>% mutate(region=gsub("\\."," ",region))
     
     #Merge
     ttwa.all_labelled <- ttwa.all %>% left_join(region_ttwas)
     
     #Final cleaning of ttwa.all
     all.cis.bsd_ttwa <- ttwa.all_labelled %>%
          rename(business_local.share=proportion.of.local.enterprises.in.sector,
                 turnover_local.share=proportion.of.local.turnover.in.sector,
                 emp_local.share=proportion.of.local.employment.in.sector,
                 business_lq=location.quotient...business.count,
                 turnover_lq=location.quotient..turnover,
                 emp_lq=location.quotient...employment) %>%
          mutate(
               turn_pw = turnover/employment,
               turn_pb = turnover/business.count,
               work_pb = employment/business.count) %>%
          select(-contains("uk."))

#2. Subsectoral ttwa
#NB there was an empty column #17 in ttwa_2011_14
ttwa.sub <- BindAreas(ttwa_2007_10_subsector,
                      ttwa_2011_14_subsector[,1:16]) %>% tbl_df() %>%
     rename(ttwa.code=ttwa.code..2011.,
            ttwa.name=ttwa.name..2011.)

#Final cleaning of subsectoral ttwa including variable renames
subsectors_ttwa <- ttwa.sub %>%
     mutate(year=ifelse(year=="2007 - 2010","2007_10","2011_14")) %>%
     rename(business_local.share=proportion.of.local.enterprises.in.sector,
            turnover_local.share=proportion.of.local.turnover.in.sector,
            emp_local.share=proportion.of.local.employment.in.sector,
            business_lq=location.quotient...business.count,
            emp_lq=location.quotient...employment,
            turnover_lq=location.quotient..turnover) %>%
     mutate(
            turn_pw = turnover/employment,
            turn_pb = turnover/business.count,
            work_pb = employment/business.count) %>%
     select(-contains("uk."))

######
#3. ESTIMATE GVA
######
#To do this, we need to:
     #Estimate average creative employment 2008_10 and 2011_14
     ttwa_creative_for.gva <- ttwa.all_labelled %>% tbl_df() %>%
          select(year,ttwa.code,ttwa.name,employment,region) %>%
          filter(year!=2007) %>% 
          mutate(period=ifelse(year<=2010,"2008_10","2011_14")) %>%
          group_by(ttwa.name,ttwa.code,region,period) %>% 
          summarise(employment=mean(employment))
          
     #Estimate average all_sector employment 2008_10 and 2011_14
     ttwa_all.inds_for.gva_tmp <- ttwa_all_industries %>% tbl_df() %>%
          rename(ttwa.code=ttwa.code..2011.) %>%
          select(year,ttwa.code,total.employment) %>%
          filter(year!=2007) %>% 
          mutate(period=ifelse(year<=2010,"2008_10","2011_14")) %>%
          group_by(ttwa.code,period) %>% 
          summarise(total.employment=mean(total.employment))
     
     #Relabel creative GVA
     gva_region_data_creative_tmp <- gva_region_data_creative %>%
          rename(gva_pw_creative_abs=gva_per_employee_thGBP_abs)
     
     #Estimate average all sector GVA per worker 2008_10 and 2011_14
     gva_region_all_sectors_tmp <- gva_region_all_sectors %>%
          mutate(period=ifelse(Year<=2010,"2008_10","2011_14")) %>%
          group_by(Region,period) %>%
          summarise(gva_all_abs=mean(Total.basic.GVA...000._abs),
                    employment_all_abs=mean(Total.employment_abs)) %>%
          mutate(gva_pw_all_abs = gva_all_abs/employment_all_abs) %>%
          rename(region=Region)
     
     #Relabel Yorkshire & the Humber to YS AND the humber.
     levels(
          gva_region_all_sectors_tmp$region)[
               grep("Yorkshire",
                    levels(gva_region_all_sectors_tmp$region))] <- "Yorkshire and the Humber"
     
     #Merge them
     #First merge the ttwa dfs
     ttwas_for_gva_tmp <- ttwa_creative_for.gva %>%
          left_join(ttwa_all.inds_for.gva_tmp,by=c("ttwa.code","period"))

     #Second merge the gva dfs
     regions_for_gva_tmp <- gva_region_data_creative_tmp %>%
          left_join(gva_region_all_sectors_tmp,by=c("region","period"))
     
     #Final merge of TTWA and GVA Dfs
     ttwa_gva_data <- ttwas_for_gva_tmp %>%
          left_join(regions_for_gva_tmp,by=c("region","period")) %>%
          mutate(gva_total_creative = gva_pw_creative_abs*employment,
                 gva_total_all=gva_pw_all_abs*total.employment,
                 period.string=ifelse(period=="2008_10","first.period",
                                      "second.period")) %>%
          select(ttwa.name,ttwa.code,region,
                 period,period.string,
                 employment,total.employment,
                 gva_total_creative,gva_total_all,
                 gva_pw_creative_abs,gva_pw_all_abs)
     
     #WriteOut(ttwa_gva_data,"final-report-data/")

#Merge the all creative industries datasets
     #using a "period string"
     all.cis_ttwa <- all.cis.bsd_ttwa %>%
          mutate(period.string=
                      ifelse(year<2010,"first.period","second.period")) %>%
          left_join(ttwa_gva_data,by=c("ttwa.name","period.string")) %>%
          select(-contains(".y"),-total.employment,-industry) %>% 
          rename(employment=employment.x,
                 ttwa.code=ttwa.code.x,
                 region=region.x)
     
     
     
     
     

