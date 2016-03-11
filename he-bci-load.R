#####
#Higher Education Business-Community Interaction (HE-BCI Survey) script.
#####

#####
#0: SETUP
#####
#Create directories and folders
source("Rcode/he-bci-functions.R")

#Plan
     #Load and tidy survey files
     #Create university indicators of activity
     #Geocode, normalise
     #Create ttwa indicators of activity
     #Plot

#####
#1. LOAD DATA
#####

#Process:
     #Read table.
     #Row 1 contains the name of the question
     #Row 2 contains the response options
     #The rest contains the data.
     #Read everything and then relabel in excel.

#Survey 1 (strategy)
#Load the first row of the strategy csv. NB I had to remove some multibyte
     #characters in TextEdit
he.bci.strategy_test <- read.csv("other_secondary_data/HESA_168293/tables/CSVs/Merged/he-bci-strategy.csv",
                            nrow=2)
#Idea: Use a loop that runs through the variable names. It stores
     #the name of the question that it found, and assigns it to any blanks that come after.

     #First, Get the question numbers with Apply.
     var.numbers.str <- sapply(names(he.bci.strategy_test),function(x) {
          question.number <- unlist(str_split(x,"\\.."))
          return(question.number[1])
     },USE.NAMES=F)
     
     #Then: read a lookup table with tidy question names
     
     var.names.str_tidy <- c(
          "id","code","region","name",
          "impact.area.all","impact.area.in.top.3",
          "sector.selection.criteria","impact.sector",
          "priority.sector.all", "priority.sector.one",
          "bus.eng.plan","comm.eng.plan","engagement.incentives",
          "licensing.capability","bus.eng.processes","bci.subsidiaries",
          "board.presence",
          "ip.ownership","ip.disclosure.reqs","ip.incentives","ip.incentives.long",
          "spinoff.support","startup.support",
          "academic.eng.days","other.bci.days","bci.metrics",
          "public.impacts","public.impacts.other",
          "regen.roles.all","regen.roles.top.3","partnership.arrangements",
          "course.provision","placement.org",
          "reg.strategy.involvement",
          "monitor.skills.needs","biz.review.curriculum")
     
     #Name metadata variables at the beginning of the dataset
     uni.metadata <- c("id","code","region","name")
     var.numbers.str[1:4] <- uni.metadata
     
     #repository vector
     var.names_repo <- var.numbers.str
     
     #Use this to keep track of the position in the loop
     v.name <- NULL
     number.count <-0
     #This is to loop over the tidy question names
     var.count <- 0
     
     #Run the loop
     for(i in 1:length(var.names_repo)) {
          if(var.names_repo[i]=="X") {
               number.count <- number.count+1
               new.value <- paste(v.name,
                                  number.count,
                                  var.names.str_tidy[var.count],
                                  he.bci.strategy_test[1,i],
                                  sep="_")
               var.names_repo[i] <- new.value
          } else {
               number.count <- 1
               var.count <- var.count+1
               v.name <- var.names_repo[i]
               var.names_repo[i] <- paste(v.name,
                                       number.count,
                                       var.names.str_tidy[var.count],
                                       he.bci.strategy_test[1,i],
                                       sep="_")
          }
     }

#Load full dataset (NB I'm removing rows with title information, and blank ones)
     he.bci.strategy_all <- read.csv("other_secondary_data/HESA_168293/tables/CSVs/Merged/he-bci-strategy.csv",
                                     header=F)[-c(1:2),]

#Rename variables
     names(he.bci.strategy_all) <- tolower(gsub(" ",".",var.names_repo))

#Remove \n line breaks while maintaining as a frame
     he.bci.strategy_all <- as.data.frame(lapply(he.bci.strategy_all,
                                   function(x) {
                                        x <- gsub("\n"," ",x)
                                        return(x)
                                   }))
     
#Reclass Y/N as dummies
     dummyvars <- names(he.bci.strategy_all)[
                   grepl(paste(
                        paste0("q",c(1,2,4,9,10,12,13,14,23,25,26,"1a","4a"),"_"),collapse="|"),
                         names(he.bci.strategy_all))]

#Dummify function to convert a yes/no into a dummy
Dummify <- function(x) {
     y <- vector(mode="logical",
                 length=length(x))
     y[x=="Yes"] <- TRUE
     y[x==""] <- NA
     return(y)
}


he.bci.strategy_all[,dummyvars] <- lapply(he.bci.strategy_all[,dummyvars],
                                          Dummify)

#Survey 2 (Programmes) NB I had to replace £ with GBP
     he.bci.programme_test <- read.csv("other_secondary_data/HESA_168293/tables/CSVs/Merged/he-bci-programmes.csv",
                                  nrow=4,header=F)

#Next task: paste the first three rows.
     he.bci.programme_names <- as.data.frame(t(he.bci.programme_test))
     prog.names_messy <- paste(he.bci.programme_names$V1,
                               he.bci.programme_names$V2,
                               he.bci.programme_names$V3,
                               he.bci.programme_names$V4,
                               sep="_")
     
#And run a loop to relabel variables
     #Create variables for the loop
     v.name <- NULL
     v.count <- 0
     
     #create a store vector
     prog.names_store <- prog.names_messy
     
     #The stores components of the name in the v.name variable
          #depending on the position in the hierarchy of labels,
          #and assigns them to those below.
     
     for (i in 1:length(prog.names_messy)){
          name_split <- unlist(str_split(prog.names_messy[i],"_"))
          
          char.vector <- sapply(name_split,nchar,USE.NAMES = F)
          
          if(char.vector[1]>0) {
               output <- paste(name_split[1:3],collapse="_")
               
               prog.names_store[i] <- output
               v.name <- name_split
          } else if (char.vector[2]>0) {
               prefix <- paste(v.name[1],collapse="_")
               #Pick it here
               v.name <- c(prefix,name_split[2])
               output <- paste(c(prefix,name_split[2:3]),
                               collapse="_")
               
               prog.names_store[i] <- output
          } else if (char.vector[3]>0) {
               prefix <- paste(v.name[1:2],collapse="_")
               #Pick it here
               output <- paste(c(prefix,name_split[3]),
                               collapse="_")
               
               prog.names_store[i] <- output
          }
     }
     
#Tidy up some of the variable names.
     cpd.name <- "\\(excluding pre-registration funded by the NHS or NCTL)_"
prog.names_store <- gsub(cpd.name,"_",prog.names_store)
     res.name <- "\\(excluding any already returned in collaborative research involving public funding)_"
     prog.names_store <- gsub(res.name,"_",prog.names_store)
     
#Read the programme file again with all results.
     he.bci.programme_all <- read.csv("other_secondary_data/HESA_168293/tables/CSVs/Merged/he-bci-programmes.csv")
     #Rename
     
     names(he.bci.programme_all) <- tolower(gsub(" ",".",prog.names_store))
     names(he.bci.programme_all) <- gsub("\\n","",names(he.bci.programme_all))

     #Remove empty/irrelevant rows
     he.bci.programme_all <- he.bci.programme_all[-c(1:2),]

     #Make variables numeric
     he.bci.programme_all[,-(1:4)] <- lapply(he.bci.programme_all[,-(1:4)],
                                             function(x){
                                                  return(as.numeric(as.character(x)))
                                             })
    WriteOut(he.bci.programme_all,"final-report-data/")
#Surve 3: IP and events
     he.bci.ip.events_test <- read.csv("other_secondary_data/HESA_168293/tables/CSVs/Merged/he-bci-ip-events.csv",
                                       nrow=4,header=F)
     
     #Next task: paste the first three rows.
     he.bci.ip.events_names <- as.data.frame(t(he.bci.ip.events_test))
     ip.names_messy <- paste(he.bci.ip.events_names$V1,
                               he.bci.ip.events_names$V2,
                               he.bci.ip.events_names$V3,
                               he.bci.ip.events_names$V4,
                               sep="_")
     
     #And run a loop to relabel variables
     #Create variables for the loop
     v.name <- NULL
     
     #create a store vector
     ip.names_store <- ip.names_messy
    
     #The stores components of the name in the v.name variable
     #depending on the position in the hierarchy of labels,
     #and assigns them to those below.
     
     for (i in 1:length(ip.names_messy)){
          name_split <- unlist(str_split(ip.names_messy[i],"_"))
          
          char.vector <- sapply(name_split,nchar,USE.NAMES = F)
          
          if(char.vector[1]>0) {
               output <- paste(name_split,collapse="_")
               
               ip.names_store[i] <- output
               
               v.name <- name_split
               
          } else if (char.vector[2]>0) {
               prefix <- paste(v.name[1],collapse="_")
               
               #Pick it here
               output <- paste(c(prefix,name_split[2:4]),
                               collapse="_")
               ip.names_store[i] <- output
               
               #New name for the tier below
               v.name <- c(v.name[1],name_split[2])
               
          } else if (char.vector[3]>0) {
               prefix <- paste(v.name[1:2],collapse="_")
               
               #Pick it here
               output <- paste(c(prefix,name_split[3:4]),
                               collapse="_")
               
               ip.names_store[i] <- output
               v.name <- c(v.name[1:2],name_split[3])
               
          } else if (char.vector[4]>0) {
               prefix <- paste(v.name[1:3],collapse="_")
               
               #Pick it here
               output <- paste(c(prefix,name_split[4]),
                               collapse="_")
               
               ip.names_store[i] <- output
          }
     }  
  
#Read the programme file again with all results.
     he.bci.ip.events_all <- read.csv("other_secondary_data/HESA_168293/tables/CSVs/Merged/he-bci-ip-events.csv")
     #Rename
     names(he.bci.ip.events_all) <- tolower(gsub(" ",".",ip.names_store))
     names(he.bci.ip.events_all) <- gsub("\\n","",names(he.bci.ip.events_all))
     
     #Remove empty/irrelevant rows
     he.bci.ip.events_all <- he.bci.ip.events_all[-c(1:3),]
     
     #Make variables numeric
     he.bci.ip.events_all[,-(1:4)] <- lapply(he.bci.ip.events_all[,-(1:4)],
                                             function(x){
                                                  return(as.numeric(as.character(x)))
                                             })

#Survey 4. NB did some changing of variable labels before loading
     he.bci.research_test <- read.csv("other_secondary_data/HESA_168293/tables/CSVs/Merged/he-bci-coll-research.csv",
                                       nrow=5,header=F)
     
     #Next task: paste the first three rows.
     he.bci.research_names <- as.data.frame(t(he.bci.research_test))
     res.names_messy <- paste(he.bci.research_names$V1,
                             he.bci.research_names$V2,
                             he.bci.research_names$V3,
                             he.bci.research_names$V4,
                             he.bci.research_names$V5,
                             sep="_")
     
     #And run a loop to relabel variables
     #Create variables for the loop
     v.name <- NULL
     
     #create a store vector
     res.names_store <- res.names_messy

     #The stores components of the name in the v.name variable
     #depending on the position in the hierarchy of labels,
     #and assigns them to those below.
     
     for (i in 1:length(res.names_messy)){
          name_split <- str_trim(unlist(str_split(res.names_messy[i],"_")))
          
          char.vector <- sapply(name_split,nchar,USE.NAMES = F)
          
          if(char.vector[1]>0) {

               output <- paste(name_split,collapse="_")
               
               res.names_store[i] <- output
               
               v.name <- name_split
               
          } else if (char.vector[2]>0) {
               prefix <- paste(v.name[1],collapse="_")
               
               #Pick it here
               output <- paste(c(prefix,name_split[2:5]),
                               collapse="_")
               res.names_store[i] <- output
               
               #New name for the tier below
               v.name <- c(v.name[1],name_split[2:5])
               
          } else if (char.vector[3]>0) {
               prefix <- paste(v.name[1:2],collapse="_")
               
               #Pick it here
               output <- paste(c(prefix,name_split[3:5]),
                               collapse="_")
               
               res.names_store[i] <- output
               v.name <- c(v.name[1:2],name_split[3:5])
               
          } else if (char.vector[4]>0) {
               prefix <- paste(v.name[1:3],collapse="_")
               
               #Pick it here
               output <- paste(c(prefix,name_split[4:5]),
                               collapse="_")
               
               res.names_store[i] <- output
               
               v.name <- c(v.name[1:3],name_split[4:5])
          } else if (char.vector[5]>0) {
               prefix <- paste(v.name[1:4],collapse="_")
               
               #Pick it here
               output <- paste(c(prefix,name_split[5]),
                               collapse="_")
               
               res.names_store[i] <- output
          } 
     }   

     
#Read the coll file again with all results.
     he.bci.research_all <- read.csv("other_secondary_data/HESA_168293/tables/CSVs/Merged/he-bci-coll-research.csv")
     #Rename
     names(he.bci.research_all) <- tolower(gsub(" ",".",res.names_store))
     names(he.bci.research_all) <- gsub("\\n","",names(he.bci.research_all))
     
     #Remove empty/irrelevant rows
     he.bci.research_all <- he.bci.research_all[-c(1:4),]
     
     #Make variables numeric
     he.bci.research_all[,-(1:4)] <- lapply(he.bci.research_all[,-(1:4)],
                                             function(x){
                                                  return(as.numeric(as.character(x)))
                                             })

          
#Combine all files to create a "pre" file (need to geotag and add university
          #sizes to normalise)
he.bci_temp <-do.call(cbind,list(he.bci.strategy_all,
                            he.bci.research_all[,-c(1:4)],
                            he.bci.programme_all[,-c(1:4)],
                            he.bci.ip.events_all[,-c(1:4)])) %>% tbl_df() 

#Read university addresses and staff numbers (nb renaming of codes for 
     #joining later)
university.address <- read.csv("metadata/hei.lu-2015-11-06.csv",
                               colClasses = rep("character",3)) %>% tbl_df() %>%
     rename(id.for.matching=code)
university.staff <- read.csv("metadata/hei-staff.csv",
                             colClasses = c(rep("character",4),
                                            rep("numeric",5))) %>% tbl_df() %>%
     rename(id.for.matching=INSTID)

#Adapt code in HE-BCI for matching
he.bci_temp$id.for.matching <- sapply(he.bci_temp$id_1_id_instid,function(x){
     id.code <- as.character(x)
     zeroes <- 4 - nchar(id.code)
     id <- paste0(
          paste0(rep("0",zeroes),collapse=""),
          id.code)
     return(id)
     })
#Join all by code
he.bci_temp2 <- join_all(list(he.bci_temp,university.address,university.staff),
                         by="id.for.matching")

#Give he.bci a TTWA
he.bci_temp3 <- merge(he.bci_temp2,nspl2015_labelled[,c("pcds","ttwa","ttwa.name")],
                      by.x="postcode",
                      by.y="pcds") %>% tbl_df()


#I have created a vector with variables of interest
vars.interest <- read.csv("metadata/he-bci_tn_var-selection.csv")

#Select only those variable names that are of interest
vars.interest_names <- vars.interest$VARIABLE[!is.na(vars.interest$RELEVANT)]
he.bci_cc <- he.bci_temp3[,names(he.bci_temp3) %in% vars.interest_names]

he.bci_cc$all.staff <- he.bci_cc$Academic.total + he.bci_cc$Non.academic

WriteOut(he.bci_cc,"final-report-data/")

#####
#Generate metrics
#####
#Impact index (add all impacts related to startups etc.)
he.bci_cc$my_local.impact_index <- rowSums(he.bci_cc[,grep("q1a",names(he.bci_cc))])
he.bci_cc$my_local.impact_quart <- HighScore(he.bci_cc$my_local.impact_index,prob=0.75)

###
#Spin-off support
###

#Need to convert the variables into Y/N
he.bci_cc[,paste0(names(he.bci_cc[,grep("q16|q17",names(he.bci_cc))]),".dummy")] <-
     lapply(he.bci_cc[,names(he.bci_cc[,grep("q16|q17",names(he.bci_cc))])],
            function(x) {
                 vec <- rep(TRUE,length(x))
                 nones <- x=="None"
                 vec[nones] <- FALSE
                 return(vec)
            })
#Index vectors to row sum the variables
spinoff.support.vars <- grepl("q16",names(he.bci_cc)) & grepl("dummy",names(he.bci_cc))
startup.support.vars <- grepl("q17",names(he.bci_cc)) & grepl("dummy",names(he.bci_cc))

#And row.sum the variables and create "rankings" (by quartile)
he.bci_cc$my_spinoff.support_index <- rowSums(he.bci_cc[,spinoff.support.vars])
he.bci_cc$my_startup.support_index <- rowSums(he.bci_cc[,startup.support.vars])


he.bci_cc$my_spinoff.support_quartile <- HighScore(he.bci_cc$my_spinoff.support_index,
                                                prob=0.75)
he.bci_cc$my_startup.support_quartile <- HighScore(he.bci_cc$my_startup.support_index,
                                                prob=0.75)


###
#Training provision
###
#Index and quartile
he.bci_cc$my_training.provision_index <- rowSums(he.bci_cc[,grep("q25",names(he.bci_cc))])

he.bci_cc$my_training.provision_quartile <- HighScore(he.bci_cc$my_training.provision_index,
                                                   0.75)

###
#Skills engagement
###

# Values need to be made numeric to average them
he.bci_cc[,paste0(names(he.bci_cc[,grep("q27|q28|q29",names(he.bci_cc))]),".numeric")] <-
     lapply(he.bci_cc[,names(he.bci_cc[,grep("q27|q28|q29",names(he.bci_cc))])],
            function(x) {
                 return(as.numeric(as.character(x)))
            })
#Select index for mean and quartile.
skills.biz.engagement.index <- grepl("q27|q28|q29",names(he.bci_cc)) & 
     grepl(".numeric",names(he.bci_cc))

#Calculate mean
he.bci_cc$my_skills.engagement <- rowMeans(he.bci_cc[,skills.biz.engagement.index])

he.bci_cc$my_skills.engagement_quartile <- HighScore(he.bci_cc$my_skills.engagement,
                                                  0.75)

######
#Spinoff/startup related activity
######

#Active firms
he.bci_cc$my_uni.business.active_measure <- 
     rowSums(he.bci_cc[,grep("number.of.active.firms",names(he.bci_cc))])
#Employment
he.bci_cc$my_uni.business.employment_measure <- 
     rowSums(he.bci_cc[,grep("estimated.current.employment.of.all.active.firms",names(he.bci_cc))])
#Turnover
he.bci_cc$my_uni.business.turnover.thGBP_measure <- 
     rowSums(he.bci_cc[,grep("estimated.current.turnover.of.all.active.firms",names(he.bci_cc))])

#Investment
he.bci_cc$my_uni.business.investment.thGBP_measure <- 
     rowSums(he.bci_cc[,grep("estimated.external.investment.received",names(he.bci_cc))])


#Collaborative research and consultancy (with SMEs)

#Knowledge exchange including contract research, consultancy and access to facilities
     he.bci_cc$my_uni.ke.exchange.smes.thGBP_measure <- 
          he.bci_cc$`contract.research._total.value.with.smes.(gbp.thousands)_2013/14` +
          he.bci_cc$`consultancy_total.value.with.smes.(gbp.thousands)_2013/14` +
          he.bci_cc$`facilities.and.equipment.related.services.-.organisations.involved.and.income_number.of.smes_2013/14`
     
     #Trainign for SMEs and individuals
     he.bci_cc$my_uni.training.smes.inds.thGBP_measure <- 
          he.bci_cc$`courses.for.business.and.the.community.-.continuing.professional.development.(cpd).courses.and.continuing.education.(ce)._cpd.for.smes.(gbp.thousands)_2013/14` +
          he.bci_cc$`courses.for.business.and.the.community.-.continuing.professional.development.(cpd).courses.and.continuing.education.(ce)._ce.and.cpd.for.individuals.(gbp.thousands)_2013/14`  

#Normalise these by staff and calculate quartiles.
he.bci_cc[,paste0(
     names(he.bci_cc)[grep("_measure",names(he.bci_cc))],"_norm")] <- 
     lapply(he.bci_cc[,grep("_measure",names(he.bci_cc))],
            function(x) {
                 return(x/he.bci_cc$all.staff)})

#Quartiles
he.bci_cc[,paste0(
     names(he.bci_cc)[grep("_measure_norm",names(he.bci_cc))],"_quartile")] <- 
          lapply(he.bci_cc[,grep("_measure_norm",names(he.bci_cc))],
                 HighScore,prob=0.75)

######
#Any additional measures?
#####
#Cultural activity for the creative clusters project
#Event attendees
he.bci_cc$my_uni.cult.event.attendees <- rowSums(he.bci_cc[,grep("attendees",
                                                                     names(he.bci_cc))],
                                                     na.rm=T)
#Normalise by staff and get quartiles
he.bci_cc[,paste0(
     names(he.bci_cc)[grep("event.attendees",names(he.bci_cc))],"_norm")] <- 
     lapply(he.bci_cc[,grep("event.attendees",names(he.bci_cc))],
            function(x) {
                 return(x/he.bci_cc$all.staff)})

he.bci_cc[,paste0(
     names(he.bci_cc)[grep("event.attendees_norm",names(he.bci_cc))],"_quartile")] <- 
     lapply(he.bci_cc[,grep("event.attendees_norm",names(he.bci_cc))],
            HighScore,prob=0.75)

#FTE staff time involved in cultural events
he.bci_cc$my_uni_cult.event.staff.time <- rowSums(he.bci_cc[,grep("staff.time",
                                                                names(he.bci_cc))])

#Normalise by staff and get quartiles
he.bci_cc[,paste0(
     names(he.bci_cc)[grep("event.staff.time",names(he.bci_cc))],"_norm")] <- 
     lapply(he.bci_cc[,grep("event.staff.time",names(he.bci_cc))],
            function(x) {
                 return(x/he.bci_cc$all.staff)})

he.bci_cc[,paste0(
     names(he.bci_cc)[grep("event.staff.time_norm",names(he.bci_cc))],"_quartile")] <- 
     lapply(he.bci_cc[,grep("event.staff.time_norm",names(he.bci_cc))],
            HighScore,prob=0.75)

#Coming up: 
          #Create area level measures.
               #Lqs based on aggregates?
               #Counts of succesful/active institutions.

#Df with universities and selected variables
he.bci.cc_uni <- he.bci_cc %>% select(4,ttwa.name,contains("my_"),all.staff) %>% 
     rename(hei= name_1_name_he.provider)

WriteOut(he.bci.cc_uni,"final-report-data/")

#ttwa level chart with variables for plotting
he.bci.cc_ttwa <- he.bci.cc_uni %>% group_by(ttwa.name) %>%
     summarise(all=n(),
               local.impact_high = sum(my_local.impact_quart),
               spinoff.support_high = sum(my_spinoff.support_quartile),
               startup.support_high = sum(my_startup.support_quartile),
               training.provision_high = sum(my_training.provision_quartile),
               skills.engagement_high = sum(my_skills.engagement_quartile),
               
               business.turnover = sum(my_uni.business.turnover.thGBP_measure),
               business.employment = sum(my_uni.business.employment_measure),
               business.investment = sum(my_uni.business.investment.thGBP_measure),
               
               business.turnover_high = sum(my_uni.business.turnover.thGBP_measure_norm_quartile),
               businesss.employment_high = sum(my_uni.business.employment_measure_norm_quartile),
               business.investment_high = sum(my_uni.business.investment.thGBP_measure_norm_quartile),
               
               sme.engagement = sum(my_uni.ke.exchange.smes.thGBP_measure),
               sme.engagement_high = sum(my_uni.ke.exchange.smes.thGBP_measure_norm_quartile),
               sme.training=sum(my_uni.training.smes.inds.thGBP_measure),
               sme.training_high=sum(my_uni.training.smes.inds.thGBP_measure_norm_quartile),
               
               event.attendees = sum(my_uni.cult.event.attendees,na.rm=T),
               event.attendees_high = sum(my_uni.cult.event.attendees_norm_quartile,na.rm=T),
               event.staff.time = sum(my_uni_cult.event.staff.time,na.rm=T),
               event.staff.time_high = sum(my_uni_cult.event.staff.time_norm_quartile,na.rm=T),
               all.staff = sum(all.staff)) %>%
     mutate(business.turnover_lq = (business.turnover/sum(business.turnover))/
                 (all.staff/sum(all.staff)),
            business.employment_lq = (business.employment/sum(business.employment))/
                 (all.staff/sum(all.staff)),
            business.investment_lq = (business.investment/sum(business.investment))/
                 (all.staff/sum(all.staff)),
            sme.engagement_lq = (sme.engagement/sum(sme.engagement))/
                 (all.staff/sum(all.staff)),
            sme.training_lq = (sme.training/sum(sme.training))/
                 (all.staff/sum(all.staff)),
            event.attendees_lq = (event.attendees/sum(event.attendees))/
                 (all.staff/sum(all.staff)),
            event.staff.time_lq = (event.staff.time/sum(event.staff.time))/
                 (all.staff/sum(all.staff)))

#Apply to check top 10 areas for all numerical variables
top5s_check <- sapply(as.list(names(he.bci.cc_ttwa)), function(x){
     #Extract the variable from tbl_df as a vector
     my_vector <- he.bci.cc_ttwa %>% select_(x) %>%
          extract2(1)
     
     #Check if the variable is numeric
     if (is.numeric(my_vector)==TRUE) {
          
          #Standardise variable names for arranging
          my_set <- he.bci.cc_ttwa %>% select_("ttwa.name",x) 
          names(my_set) <- c(paste0("top_ttwa_",x),"var")
          
          #Extract top 10
          top10 <- my_set %>% arrange(desc(var)) %>%
               extract(1:10,paste0("top_ttwa_",x)) %>% as.data.frame()
          return(top10)
     }
})




# #Barchart with levels of activity by TTWA.
# 
# #Investment
# he.bci.inv_barchart <- he.bci.tn_ttwa %>% select(ttwa.name,
#                                                   contains("business.investment")) %>% arrange(desc(business.investment)) %>%
#      extract(1:20,)
# he.bci.inv_barchart$ttwa.name <- ReorderFactor(he.bci.inv_barchart$ttwa.name,
#                                                he.bci.inv_barchart$ttwa.name,z=T)
# 
# he.bci.investment_barplot <- ggplot(data=he.bci.inv_barchart,
#                               aes(x=ttwa.name,
#                                   y=business.investment,
#                                   fill=business.investment_lq))+
#      geom_bar(stat="identity")+coord_flip()+
#      scale_y_continuous(labels=comma)+
#      theme(axis.text=element_text(size=8))+
#      scale_fill_continuous(low="lightblue",high="darkblue")+
#      labs(y="Investment (£K)",
#           x=NULL,fill="Relative specialisation",
#           title="Investment in active spin-offs and startups \n related to universities in the TTWA (HESA,2015)")
# 
# #Jobs
# he.bci.jobs_barchart <- he.bci.tn_ttwa %>% select(ttwa.name,
#                                                  contains("business.employment")) %>% arrange(desc(business.employment)) %>%
#      extract(1:20,)
# he.bci.jobs_barchart$ttwa.name <- ReorderFactor(he.bci.jobs_barchart$ttwa.name,
#                                                he.bci.jobs_barchart$ttwa.name,z=T)
# 
# he.bci.jobs_barplot <- ggplot(data=he.bci.jobs_barchart,
#                               aes(x=ttwa.name,
#                                   y=business.employment,
#                                   fill=business.employment_lq))+
#      geom_bar(stat="identity")+coord_flip()+
#      scale_fill_continuous(low="lightblue",high="darkblue")+
#      scale_y_continuous(labels=comma)+
#      theme(axis.text=element_text(size=8))+
#      labs(y="FTE",
#           x=NULL,fill="Relative specialisation",
#           title="Employment in active spin-offs and startups \n related to universities in the TTWA (HESA,2015)")
# 
# pdf("Figures/HE-BCI/uni.entrepreneurialism.pdf",w=9,h=6)
# multiplot(he.bci.investment_barplot,he.bci.jobs_barplot)
# dev.off()
# 
# ######
# #MAPS
# #####
# #Select my variables for plotting
# he.bci_mapping <- he.bci.tn_ttwa %>% select(1:7)
# 
# #Need to merge with all TTWAs to produce a plot
# ttwa.codes.merge <- data.frame(code1=ttwa.codes$TTWA07NM.1,
#                                code2=ttwa.codes$TTWA07NM.1) %>% tbl_df() %>%
#      filter(code1!="")
# 
# #Merge with ttwas
# he.bci_mapping_all.ttwas <- merge(he.bci_mapping,ttwa.codes.merge,
#                                   by.x="ttwa.name",
#                                   by.y="code1",all.y=T) %>% tbl_df()
# 
# he.bci_mapping_all.ttwas[is.na(he.bci_mapping_all.ttwas)] <- 0
# 
# he.bci_mapping_all.melt <- he.bci_mapping_all.ttwas %>% select(-code2,-all) %>%
#      melt(id.var="ttwa.name")
# 
# 
# #Load shapefiles
# uk.ttwa <- CreateTTWAmapping_df()
# #Merge
# he.bci.map.df <- merge(uk.ttwa,he.bci_mapping_all.melt,
#                         by.x="TTWA01NM",
#                         by.y="ttwa.name") %>% tbl_df() %>% arrange(order)
# str(he.bci.map.df)
# #Create map
# he.bci_map <- ggplot(data=he.bci.map.df,aes(x=long,y=lat))+
#      geom_polygon(aes(group=group,fill=value))+
#      facet_grid(.~variable)+
#      scale_fill_gradient(low="lightblue",high="darkblue")+
#      labs(title="Digital tech HE engagement scores by TTWA (HESA, 2015)",
#           fill="Number of universities in top quartile for score")+
#      theme(legend.position="bottom")+
#      map_theme
# 
# WriteChart(he.bci_map,"Figures/",w=9,h=6)
