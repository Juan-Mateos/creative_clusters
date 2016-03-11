#Script to scrape REF data
#####
#RATIONALE
#####

#Idea: access the data for subjects of interest, geo-code universities
        #and create indices.
#Directories
source("Rcode/ref_functions.R")

######
#Download the data
######

#Units of assessment (subjects) = there are 36 of them.
#Create a dictionary of them using the reference page here:
#http://results.ref.ac.uk/Results/SelectUoa

uoaParsed <- GetParseHtml("http://results.ref.ac.uk/Results/SelectUoa")

#Extract the list
uoaSubjects <- xpathSApply(uoaParsed,
                           "//div[@id='content']//li/a",xmlValue)
#Create a table
uoaTable <- ldply(uoaSubjects,
                  function(x) {
                          myData <- unlist(strsplit(x,"-"))
                          return(data.frame(code=myData[1],
                                            subject = myData[2]))
                  }
)

#Extract the data for "subjects of interest"
cc_subjects_df <- ldply(list("computer science"="computer science",
                           "arts and design"="art and design"),
                      GetREFData)


#WriteOut(tn.subjects_df,"Datasets")

#######
#Geocode and get staff sizes
#######
#Read postcode/institution lookup
hei.postcode_lookup <- read.csv("metadata/hei.lu-2015-11-06.csv")

#Read hei staff numbers from HESA.
hei.staff <- read.csv("metadata/hei-staff.csv")

#Match both using their codes
hei_metadata <- merge(hei.postcode_lookup,
                      hei.staff,
                      by.x="code",by.y="INSTID",all.x=T) %>%
     filter(!is.na(HE.provider))

#There are a couple of HEIs with no
        #staff numbers in the HESA data: NSFT and Plymouth School of Art.

#Tidy labels
names(hei_metadata) <- tolower(names(hei_metadata)) 

#Create new variable to facilitate the matching
hei_metadata$name_tidy <- str_trim(gsub("^The","",hei_metadata$name))

#Also clean some of the data in the REF scraped file
        #to deal with universities that generate more
        #than one submission, and universities that carry out
        #joint submissions.
cc_subjects_df$name_tidy <- sapply(cc_subjects_df$university,
                                       function(x){
                                               split.field <- unlist(str_split(x,"\\:|\\("))
                                               return(str_trim(split.field[1]))
                                       })
#Unique REF HEI names
hei.names_unique.tidy <- unique(cc_subjects_df$name_tidy)
names(hei.names_unique.tidy) <- hei.names_unique.tidy

#This loop tries to match each name from HEFCE with a name from 
        #HESA
hei.name.matches <- 
        ldply(hei.names_unique.tidy,
               function(x){
                           match <- agrep(x,hei_metadata$name_tidy,
                                          #The match has to be perfect
                                          max.distance=0)
                           myMatches <- paste(hei_metadata$name_tidy[match],
                                              collapse=", ")
                           return(c(length(match),myMatches))
                           })
#There is only a small number of non-matches (9)
# They are: 
#      hei.name.matches$.id[hei.name.matches$V1==0]

#We'll sort them by hand
WriteOut(hei.name.matches,"metadata/hei.hesa.matches.corrected.csv")

#And read them again
hei.names_lookup <- read.csv("metadata/hei.hesa.matches.corrected_2.csv",
                            encoding="UTF-8")
names(hei.names_lookup) <- c("hei","match.no","match_rough","match_clean")
        
#Careful with Glyndŵr University
hei.names_lookup$match_clean <- as.character(hei.names_lookup$match_clean)

hei.names_lookup$match_clean[grep("Glynd",hei.names_lookup$match_clean)] <- "Glyndŵr University"

#Next step: use this lookup to get the postcodes.
hei.hesa_metadata <- merge(hei_metadata,
                            hei.names_lookup,
                            by.x="name_tidy",
                            by.y="match_clean",
                           all.x=T)

hei.hesa_metadata$hei <- as.character(hei.hesa_metadata$hei)

hei.hesa_metadata$hei[grep("Glynd",hei.hesa_metadata$hei)] <- "Glyndŵr University"

WriteOut(hei.hesa_metadata,y="final-report-data/")

#Now: get the TTWAs for each university.
hei.hesa_ttwas <- merge(hei.hesa_metadata,nspl2015_labelled[,c("pcds","ttwa","ttwa.name")],
                                 by.x="postcode",by.y="pcds",all.x=T)

#Now, merge with the REF results
hei_metadata.df <- subset(hei.hesa_ttwas,
                                     select=-name_tidy)

#And merge with the tn_subjects Df.
#We will do this as an apply combine by subject.
#or shall we? Maybe we could just create a repository dataframe
        #with the combinations of variables
#tn.subjects_geocoded <- 

cc_subjects_geocoded <- merge(cc_subjects_df,
                              hei_metadata.df,
                              by.x="name_tidy",
                              by.y="hei",
                              all.x=T,
                              all.y=T) %>% tbl_df()

#This is the university data we want
#Next: Create some measures of university performance:
        #weighted score: Weighted submission score
        #fte.4star: FTEs in 4*
        #fte.2star: FTEs >3*

ref.outcomes <- cc_subjects_geocoded %>% 
        mutate(
        weighted.score = (4*star4 + 3*star3 + 2*star2 + 1*star1)/100,
        fte.4star = staff*star4/100,
        fte.2star = staff*(star4+star3+star2)/100) %>% tbl_df() %>%
        select(2:10,13,18:22,25:28) %>% 
        rename(subject=.id)
        
#WriteOut(ref.outcomes,"Datasets/")        
#Last operation for data merging: create aggregate academic staff numbers by TTWA
        #to normalise later.
all.ttwa.staff <- hei_metadata.df %>% tbl_df() %>% 
        group_by(ttwa.name) %>% summarise(
                academic.fte_sum=sum(academic.full.time,na.rm=T),
                academic.all_sum=sum(academic.total,na.rm=T))
#######
#ANALYSIS
#######

#Create REF TTWA scores
ref.outcomes_by.ttwa <- ref.outcomes %>% group_by(ttwa.name,
                                                  subject,measure) %>%
        summarise(fte.4star_ttwa=sum(fte.4star,na.rm=T),
                  fte.2star_ttwa=sum(fte.2star,na.rm=T),
                  weighted.score.norm=mean(staff*weighted.score)/sum(staff,na.rm=T)) %>%
        ungroup()

#Whowins
# ref.outcomes_by.ttwa %>% 
#         filter(measure=="Impact" & subject=="computer science") %>%
#         arrange(desc(weighted.score.norm))

#Merge with all.ttwa.staff to create lqs
ref.outcomes_df <- merge(ref.outcomes_by.ttwa,all.ttwa.staff,
                          by.x="ttwa.name",
                          by.y="ttwa.name",
                          all.x=T,
                         all.y=T)

#Create lqs (by ttwa,subject/code) - 
#based on split/apply/combine over combinations of subjects and
        #disciplines
ref.outcomes_with.lq <- ldply(
        split(ref.outcomes_df,
              list(ref.outcomes_df$subject,
                   ref.outcomes_df$measure)),
        function(x) {
                x$fte.4star_lq <- LQ(x$fte.4star_ttwa,
                                     x$academic.fte_sum)
                x$fte.2star_lq <- LQ(x$fte.2star_ttwa,
                                     x$academic.fte_sum)
                return(x)
        }) %>% tbl_df()

#Check rankings of TTWAs
subject.rankings <- ref.outcomes_with.lq %>% filter(measure=="Overall" & 
                                        subject=="computer science") %>%
        select(ttwa.name,contains("lq")) %>%
        arrange(desc(fte.4star_lq)) %>% as.data.frame()

