#aps_load_functions

#GetAPSData function that takes an aps dataset and returns the clean version.
#We want it to:
     #Label creative occupations as creative using SOC codes
     #Label creative industries as creative using SIC codes
     #Label creative subsectors as such using SIC codes (we will use this
          #for an analysis of labour correlation later)

GetAPSData <- function(aps.dataset) {
     #Read data (selected columns)
     aps_test <- read.csv(aps.dataset,nrow=10)
     
     names(aps_test) <- tolower(names(aps_test))
     
     #My variables
     aps.analysis.vars <- c("inecac05",
                            "indsc07m","indsc07s",
                            "soc10m","soc10s",
                            "pcode",
                            "country",
                            "pwta14")
     
     #Get their indices for reading the data in
     aps.var.index <- grep(paste(aps.analysis.vars,collapse="|"),
                           names(aps_test))
     
     col.classes <- rep("NULL",ncol(aps_test))
     col.classes[aps.var.index] <- NA
     
     #Read the data (raw) 
     aps_rawish_df <- read.csv(aps.dataset,colClasses = col.classes)
     
     #Convert into table
     aps_selected.qs <- tbl_df(aps_rawish_df)
     
     #Reorder variables and clean their names
     names(aps_selected.qs) <- tolower(names(aps_selected.qs))
     aps_selected.qs.2 <- aps_selected.qs[,order(colnames(aps_selected.qs),decreasing=F)] %>%
          tbl_df()
     
     names(aps_selected.qs.2) <- c("country",
                                   "sic","sic.s",
                                   "econ.activity",
                                   "postcode","weight",
                                   "soc4","soc4.s")
                                   
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
     aps_processed <- aps_selected.qs.2 %>% mutate(sic4=
                                                        as.numeric(substring(sic,0,4)),
                                                   sic4.s=
                                                        as.numeric(substring(sic.s,0,4)))
     #Create CE dummies for creative SIC/SOCs
     aps_processed$ce.sic <- as.numeric(aps_processed$sic4 %in% ce_sics$code |
                                        aps_processed$sic4.s %in% ce_sics$code)
     
     aps_processed$ce.soc <- as.numeric(aps_processed$soc4 %in% ce_socs$code |
                                             aps_processed$soc4.s %in% ce_socs$code)
     
     #Transform into factors
     aps_processed$ce.sic_factor <- factor(aps_processed$ce.sic,
                                           levels=c(0,1),
                                           labels=c("ci","non_ci"))
     
     aps_processed$ce.soc_factor <- factor(aps_processed$ce.soc,
                                           levels=c(0,1),
                                           labels=c("co","non_co"))
     
     #Create embedded and creative economy dummies
     aps_processed$is.embedded <- 
          aps_processed$ce.soc==1 & aps_processed$ce.sic==0
     
     aps_processed$is.creative.economy <- 
          aps_processed$is.embedded==1 |
          aps_processed$ce.sic==1
     
     #Focus on workers
     aps_final <- aps_processed %>% filter(econ.activity==1 |
                                                econ.activity==2)
     
     
     #Create index for merging
     aps_final$index <- 1:nrow(aps_final)
     
     #Assign workers to creative sectors and creative occupations
     creative_assignments <- lapply(
          list("sic","soc"),
          function(x){
               #Get the meta_data
               meta_name <- paste0("ce_",x,"s")
               meta_data <- get(meta_name)
                                         
               #Merge with aps
               #First need to create the variables for merging
                    merge_var <- paste0(x,"4")
                    merge_var2 <- paste0(merge_var,".s")
                                         
                    #Merge on first job
                    merged <- merge(aps_final[,c("index",merge_var)],
                                    meta_data,
                                    by.x=c(merge_var),
                                    by.y=c("code"),
                                    all.x=T) %>% tbl_df %>%
                         arrange(desc(index)) %>%
                         droplevels()
                    #Then merge on second area.
                    merged2 <- merge(aps_final[,c("index",merge_var2)],
                                     meta_data,
                                     by.x=c(merge_var2),
                                     by.y=c("code"),
                                     all.x=T) %>% tbl_df %>%
                         arrange(desc(index)) %>%
                         droplevels()
                                         
                    #Then replace NAs in first with second
                    nalabel2s <- is.na(merged$label2)
                    merged$label2[nalabel2s] <- merged2$label2[nalabel2s]
                    
                    output <- merged[,c("index","label2")]
                    names(output) <- c("index",paste0(merge_var,"_label"))
                    #Return   
                    return(output)
          })
     
     #Labelled variables
     aps_working_labelled <- join_all(list(aps_final,creative_assignments[[1]],
                                      creative_assignments[[2]]),by="index")
     
     #Geocoding
     aps_geocoded <- merge(aps_working_labelled,nspl2015_labelled,
                           by.x="postcode",by.y="pcd",all.x=T) %>% tbl_df() %>%
          select(-pcds,-index,-postcode)
     #Out
     return(aps_geocoded)
}


#GetJobsReport function takes an APS table and an area to summarise by and returns:
#Total jobs, proportions and LQs in
     #creative economy
     #creative industries
     #creative occupations
GetJobsReport <- function(my.aps.data, area,year) {
     myDf <- my.aps.data %>% tbl_df()
     report <- myDf %>% 
          group_by_(area) %>%
          summarise(n=n(),
                    all.jobs=sum(weight),
                    ci.n=sum(ce.sic),
                    ci.jobs=sum(weight*ce.sic),
                    co.n=sum(ce.soc),
                    co.jobs=sum(weight*ce.soc),
                    c.embed.n=sum(is.embedded),
                    c.embed.jobs = sum(weight*is.embedded),
                    ce.n = sum(is.creative.economy),
                    ce.jobs = sum(weight*is.creative.economy)) %>%
          mutate(ci.lq=(ci.jobs/all.jobs)/(sum(ci.jobs)/sum(all.jobs)),
                 co.lq=(co.jobs/all.jobs)/(sum(co.jobs)/sum(all.jobs)),
                 ce.lq=(ce.jobs/all.jobs)/(sum(ce.jobs)/sum(all.jobs)),
                 year=year)
     return(report)
}

aps.dataset <- "APSP_JD14_Nestav2.csv"

#Get APS data for George

Get_APS_Data_for_George <- function(aps.dataset) {
     #Read data (selected columns)
     aps_test <- read.csv(aps.dataset,nrow=300)
     
     #Load country codes
     intnl_country_codes <- read.xlsx("Metadata/NSCC Classification_GW.xls",
                                      sheetName="NSCC Classification")
     
     #NAs are group 3 (Non EU Non UK nationals)
     intnl_country_codes$VAR[is.na(intnl_country_codes$VAR)] <- 3

     intnl_country_codes$VAR <- factor(intnl_country_codes$VAR,
                                       labels=c("UK","EU","Other"))
     
     #Continue processing the data
     names(aps_test) <- tolower(names(aps_test))
     
     #My variables
     aps.analysis.vars <- c("inecac05",
                            "indsc07m","indsc07s",
                            "soc10m","soc10s",
                            "ntnlty","nato7",
                            "pwta14")
     
     #Get their indices for reading the data in
     aps.var.index <- grep(paste(aps.analysis.vars,collapse="|"),
                           names(aps_test))
     
     col.classes <- rep("NULL",ncol(aps_test))
     col.classes[aps.var.index] <- NA
     
     #Read the data (raw) 
     aps_rawish_df <- read.csv(aps.dataset,colClasses = col.classes)
     
     #Convert into table
     aps_selected.qs <- tbl_df(aps_rawish_df)
     
     #Reorder variables and clean their names
     names(aps_selected.qs) <- tolower(names(aps_selected.qs))
     aps_selected.qs.2 <- aps_selected.qs[,order(colnames(aps_selected.qs),decreasing=F)] %>%
          tbl_df()
     
     names(aps_selected.qs.2) <- c("sic","sic.s",
                                   "econ.activity",
                                   "nat_other","nat",
                                   "weight",
                                   "soc4","soc4.s")
     
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
     
     #Create 4-digit SIC codes which gives us a "processed" dataframe
     aps_processed <- aps_selected.qs.2 %>% mutate(sic4=
                                                        as.numeric(substring(sic,0,4)),
                                                   sic4.s=
                                                        as.numeric(substring(sic.s,0,4)))
     #Create CE dummies for creative SIC/SOCs
     aps_processed$ce.sic <- as.numeric(aps_processed$sic4 %in% ce_sics$code |
                                             aps_processed$sic4.s %in% ce_sics$code)
     
     aps_processed$ce.soc <- as.numeric(aps_processed$soc4 %in% ce_socs$code |
                                             aps_processed$soc4.s %in% ce_socs$code)
     
     #Transform into factors
     aps_processed$ce.sic_factor <- factor(aps_processed$ce.sic,
                                           levels=c(0,1),
                                           labels=c("ci","non_ci"))
     
     aps_processed$ce.soc_factor <- factor(aps_processed$ce.soc,
                                           levels=c(0,1),
                                           labels=c("co","non_co"))
     
     #Create embedded and creative economy dummies
     aps_processed$is.embedded <- 
          aps_processed$ce.soc==1 & aps_processed$ce.sic==0
     
     aps_processed$is.creative.economy <- 
          aps_processed$is.embedded==1 |
          aps_processed$ce.sic==1
     
     #Focus on workers
     aps_final <- aps_processed %>% filter(econ.activity==1 |
                                                econ.activity==2)
     
     
     #Create index for merging
     aps_final$index <- 1:nrow(aps_final)
     
     #Assign workers to creative sectors and creative occupations
     creative_assignments <- lapply(
          list("sic","soc"),
          function(x){
               #Get the meta_data
               meta_name <- paste0("ce_",x,"s")
               meta_data <- get(meta_name)
               
               #Merge with aps
               #First need to create the variables for merging
               merge_var <- paste0(x,"4")
               merge_var2 <- paste0(merge_var,".s")
               
               #Merge on first job
               merged <- merge(aps_final[,c("index",merge_var)],
                               meta_data,
                               by.x=c(merge_var),
                               by.y=c("code"),
                               all.x=T) %>% tbl_df %>%
                    arrange(desc(index)) %>%
                    droplevels()
               #Then merge on second area.
               merged2 <- merge(aps_final[,c("index",merge_var2)],
                                meta_data,
                                by.x=c(merge_var2),
                                by.y=c("code"),
                                all.x=T) %>% tbl_df %>%
                    arrange(desc(index)) %>%
                    droplevels()
               
               #Then replace NAs in first with second
               nalabel2s <- is.na(merged$label2)
               merged$label2[nalabel2s] <- merged2$label2[nalabel2s]
               
               output <- merged[,c("index","label2")]
               names(output) <- c("index",paste0(merge_var,"_label"))
               #Return   
               return(output)
          })
     
     #Labelled variables
     aps_working_labelled <- join_all(list(aps_final,creative_assignments[[1]],
                                           creative_assignments[[2]]),by="index")
     
     
     #Merge nat and othe nat, and extract codes
     aps_working_labelled$nat_merged <- aps_working_labelled$nat 
     
     #Other nat boolean
     other_nat <- aps_working_labelled$nat == 997 &
          is.na(aps_working_labelled$nat) == FALSE
     
     #Replace "others" with their codes in merged
     aps_working_labelled$nat_merged[other_nat] <- 
          aps_working_labelled$nat_other[other_nat]
     
     #Make factor
     aps_working_labelled$nat_merged <- as.factor(aps_working_labelled$nat_merged)
     
     #Merge with the codes to select out variables.
     aps_working_labelled2 <- merge(aps_working_labelled,
                                    intnl_country_codes[,c("Numeric.3","VAR")],
                                    by.x="nat_merged",
                                    by.y="Numeric.3",
                                    all.x=T)
     names(aps_working_labelled2)[length(names(aps_working_labelled2))] <- "Nationality"  

     #Create table
      migrant_CIS <- aps_working_labelled2 %>%
           filter(complete.cases(Nationality)) %>%
           group_by(Nationality,sic4_label) %>%
           summarise(sample=n(),
                     total=sum(weight))

     migrant_CIS$sic4_label <- as.character(migrant_CIS$sic4_label)
     
     #Relabel non-CIs group
     missing_v <- is.na(migrant_CIS$sic4_label)
     migrant_CIS$sic4_label[missing_v] <- "Non_CIS"
     
     #What now? 
     #2 dcasts and cbind (one for sample, one for totals, and one for 
          #proportions)
     migrant_CIS_wide_List <- lapply(c("sample","total"),
                                function(x){
                                     out <- dcast(migrant_CIS,
                                          sic4_label~Nationality,value.var=x)
                                     names(out)[-1] <- paste(x,names(out)[-1],sep="_")
                                     return(out)
                                })
     
     migrant_CIS_wide <- merge(migrant_CIS_wide_List[[1]],
                               migrant_CIS_wide_List[[2]],
                               by="sic4_label")
     
     CIS <- migrant_CIS_wide$sic4_label != "Non_CIS"
     migrant_CIS_wide[nrow(migrant_CIS_wide)+1,-1] <- colSums(migrant_CIS_wide[CIS,-1])
     migrant_CIS_wide$sic4_label[nrow(migrant_CIS_wide)] <- "Creative Industries Totals"
      
     CIS_nat_props <- migrant_CIS_wide %>% select(contains("total_")) %>%
          mutate(rs = rowSums(.)) %>%
          mutate_each(funs(Percentify(./rs)))
     names(CIS_nat_props) <- gsub("total_","perc_of_total_",names(CIS_nat_props))
     
     migrant_CIS_final <- cbind(migrant_CIS_wide,CIS_nat_props)
     return(migrant_CIS_final)
}

#migrant_chart <- Get_APS_Data_for_George("APSP_JD14_Nestav2.csv")

#write.csv(migrant_chart,"final-data/migmigrant_chart.csv",row.names = FALSE)

