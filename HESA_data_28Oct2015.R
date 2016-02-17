#Analysis of HESA data for TechNation and Creative Clusters project.

#####
#0. Set-up
#####

#1. LOGIC

#Input: a text file with HESA data
#Output:
        #Tables summarising qualifiers by discipline and location.
                #Some variables to include:
                        #Total number of qualifiers by level
                        #Change in qualifiers over the period
                        #Qualifiers as % of population
                        #Relative specialisation in discipline
                        #5-digit hotspots e.g. LQ 2sd above the mean.
        #Perhaps: A list of TTWAs with
                # distribution of qualifiers by 5-digit JACS code and level.
                #Perhaps: LQs.
                #To do this I can generate a table with 
                        #JACs codes as % of national totals for the 
                        #whole UK (denominator in the LQ)
#Build functions to generate outputs using discipline as parameter.

#2. OBSERVATIONS 
#Aggregate HEs over TTWAs. This might require matching
        #via lookups, or via 
#Take into account distance qualifiers/qualifiers
        #in other locations
#Use temp to refer to variables we will be removing to keep the workspace tidy

#Load functions created for this analysis
source("Rcode/HESA-functions.R")

#####
#1. READ AND TIDY DATA
#####

#Unzip HESA data
# unzip(zipfile = "Analysis/Datasets/HESA_167361.zip",
#       exdir="Analysis/Datasets/")

#Read the data careful to kep the HE codes as factors\
#Read the data from TechNation
hesa <- read.delim("/Users/juanmateos-garcia/Desktop/2015 core/Technation/Analysis/Datasets/ 37851_Data.txt",
                   colClasses=c(rep("factor",10),"numeric"),
                   na.strings="N/A")

#Read excel file with metadata and labels
labels <- read.xlsx("/Users/juanmateos-garcia/Desktop/2015 core/Technation/Analysis/Datasets/ 37851_FieldOrder_Labelfile.xlsx",
                         sheetIndex=3,startRow=3)

#Add postcodes to the data. 
#We download a list with postcodes from the HESA website
hei.postcodes.temp <- GetParseHtml("https://www.hesa.ac.uk/insts")

#Extract the variables we are interested in
hei.metadata <- xpathSApply(hei.postcodes.temp,
                             "//table[@class='tablesorter']//tbody/tr",
                             xmlValue)
hei.lu <- ldply(hei.metadata,function(x) {
        broken.up <- str_trim(unlist(str_split(x,"\r")))
        return(data.frame(code=broken.up[[1]],
                          name=broken.up[[4]],
                          postcode=broken.up[[6]]))
})
#Export
WriteOut(hei.lu,y="metadata/")

#Merge with HESA data to get postcodes.
hesa.pcode <- merge(hesa,hei.lu,
                  by.x="F_XINSTID01",
                  by.y="code",
                  all.x=T)

#Merge with hesa
hesa.geo <- merge(hesa.pcode, nspl2015_labelled,
                   by.x="postcode",by.y="pcds",
                   all.x=T)

#Add subjects to the data.
#Issue: the codes for 2010/11-2011/12 are different for the codes from
        #2012/13-13/14. We will need to (at least for now) remove
        #the older data.

#Create lookup between JACS codes and subjects (scraped from HESA)
#TODO: Need to revise this (they are older codes)
#Implication: should only consider the last couple of years since
        #the revision: 2012/13 - 2013/14
#Get and Parse the data

#Looks unnecessary: candidate for removal
# jacs.lu.temp <- GetParseHtml("https://www.hesa.ac.uk/component/content/article?id=1806")
# 
# #Extract the list using Xpath
# disciplines <- xpathApply(jacs.lu.temp, 
#                           "//div[@class='box']//h3",xmlValue)
# #Create df with JACS lookup
# jacs.lu <- ldply(disciplines, function(x) {
#         components <- unlist(str_split(x,"-"))
#         myDf <- data.frame(area=str_trim(components[1]),
#                            name=str_trim(tolower(components[2])))
#         return(myDf)
# })

#Create JACS3 (new JACS) lookup
jacs3.lu_tmp <- GetParseHtml("https://www.hesa.ac.uk/component/content/article?id=1787")
jacs3.content_tmp <- xpathSApply(jacs3.lu_tmp,
                                  "//div[@class='container_16']//h3",xmlValue)
jacs3.lu <- ldply(jacs3.content_tmp,function(x){
        components <- str_trim(unlist(str_split(x,"-")))
        myDf <- data.frame(subject.code=str_trim(components[1]),
                           subject.label=str_trim(tolower(components[2])))
})

#Back to the HESA data
#We will change this and only focus the analysis on the
        #new years. Can be done by filtering the dataset OR
        #just merging with JACS_NEW (which will have NAs for older years)

#Remove old years from the dataset
hesa.geo.new_tmp <- hesa.geo %>% filter(ACYEAR != "2010/11" &
                                                 ACYEAR != "2011/12")

#Create new variable with the first element in the JACS string
hesa.geo.new_tmp <- tbl_df(hesa.geo.new_tmp) %>% mutate(subject_code=
                                                 substr(JACS_NEW,0,1))

#Merge with jacs3 lookup to get subject names
qualifiers_tmp <- merge(hesa.geo.new_tmp,jacs3.lu,
                  by.x="subject_code",
                  by.y="subject.code",
                  all.x=T,
                  sort=F)

#Tidy data
qualifiers <- tbl_df(qualifiers_tmp)
names(qualifiers) <- tolower(names(qualifiers))
qualifiers <- droplevels(qualifiers)

#Select variables of interest
qualifiers <- qualifiers %>% select(f_xinstid01,subject.label,subject_code,
                                    jacs_new,
                                    acyear,levmkr,dlmkr,f_nation,total,
                                    ttwa,ttwa.name)

names(qualifiers)[1:9] <- c("hei","subject","subject.code","jacs.new",
                            "year","level",
                       "distance","nation","total")

#The Open University dominates the distance learners. We should remove
#these
qualifiers %>% group_by(distance) %>% summarise(tots=sum(total)) %>%
        ungroup() %>% mutate(props= 100*tots/(sum(tots)))
qualifiers %>% group_by(distance,hei) %>% summarise(tots = sum(total,na.rm=T)) %>%
        filter(distance=="DL") %>% ungroup() %>% arrange(desc(tots)) %>%
        mutate(props=100*tots/sum(tots))


#Remove large unnecessary files from the workspace
#rm(list=ls()[grep("hesa|nspl|tmp",ls())])

#Rename the levels in a couple of the variables
levels(qualifiers$level) <- c("phd","1st.degree","masters","other.pg",
                               "other.ug")
levels(qualifiers$distance) <- c("yes","no")

#Create variable with short subject names
qualifiers$subject.short <- factor(qualifiers$subject,
                                     labels=c(
                                             "medicine","medicine.allied",
                                             "biological.sciences",
                                             "veterinary.agriculture",
                                             "physical.sciences",
                                             "maths",
                                             "engineering",
                                             "compsci",
                                             "technologies",
                                             "architecture",
                                             "social.studies",
                                             "law",
                                             "business",
                                             "communications",
                                             "linguistics",
                                             "languages.european",
                                             "languages.other",
                                             "history.philosophy",
                                             "art.design",
                                             "education"))

#Add labels for the JACS codes
labels_tb <- tbl_df(labels)

labels_tb_jacs <- labels_tb %>% filter(Field.Name == "JACS_NEW" &
                                               Data != "N/A")

#Clean labels

labels_tb_jacs$labels.clean <- sapply(labels_tb_jacs$Label,
                                      function(x) {
                                              cl.lab <- as.vector(str_split_fixed(x,"\\) ",2))
                                              return(cl.lab[2])
                                      })

labels_tb_jacs <- droplevels(labels_tb_jacs)

#Merge labels with dataset
qualifiers2 <- merge(qualifiers,labels_tb_jacs[,c("Data",
                                                 "labels.clean")],
                      by.x="jacs.new",by.y="Data",all.x=T,sort=FALSE)

qualifiers_tb <- tbl_df(qualifiers2)


#Remove non-Euro students. Get EURO country codes
     countries <- GetParseHtml("http://www.pyrosoft.co.uk/blog/2015/06/01/comma-separated-list-of-eu-country-codes/")
     countries.codes <- xpathSApply(countries,"//div[@class='entry-content']//code",
                                    xmlValue)
     eu.28.countries <- unlist(str_split(countries.codes[[2]],",")) %>% 
             str_trim()
     #Clean
     eu.28.countries.clean <- gsub("[[:punct:]]","",eu.28.countries)

#Remove distance learners and non-Euro qualifiers
qualifiers_tb2 <- filter(qualifiers_tb,distance !="yes" &
                                 qualifiers_tb$nation %in% eu.28.countries.clean |
                              qualifiers_tb$nation == "ZZ")

#Extract some data using the GetQualStats function
ttwa.subject.2013.14 <- GetQualStats(x="2013/14",y=c("compsci",
                                                 "art.design"),
                                     z=qualifiers_tb2)
#Rbind ttwa subject data
ttwa.subject.year <- ldply(list("2012/13"="2012/13",
                                "2013/14"="2013/14"),
                            GetQualStats,y=NULL,
                            z=qualifiers_tb2) %>% tbl_df()

#Something similar for each TTWA, but
        #focused on smaller subjects.
#How to do this.
#Input a vector with JACS codes and the top JACS codes
        #to represent. 
#Returns a table with relevant information for those JACS codes:
        #Number of students, national proportion of students,
        #LQ. If we want a deep dive it represents a ranking for
        #vectors of JACS codes (as above). Could we create
        #a table with changes in qualifiers vs changes in economically
        #active residents? Difficult given changes in TTWA defs.

interestingJacs <- levels(labels_tb_jacs$Data)[grep("W|I",
                                                    levels(labels_tb_jacs$Data))]

ttwa.jacs.2013.14 <- GetJacsStats(x="2013/14",
                                  y=interestingJacs,z=qualifiers_tb2,
                                  top=15)

#QA checks
sum(!(unique(hesa$F_XINSTID01))%in% hei.lu$code)
100*sum(is.na(hesa.pcode$postcode))/nrow(hesa.pcode)
sum(!(qualifiers_tb$nation %in% eu.28.countries.clean))/
        nrow(qualifiers_tb2)




