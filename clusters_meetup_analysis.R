#Meetup analysis
setwd("Desktop/Creative Clusters/")
source("Rcode/genericPackages-copy.R")
source("Rcode/myUtilityFunctions-copy.R")
source("Rcode/cluster_meetup_functions.R")
require("tm")
require("topicmodels")

######
#Read data
######

#Tech groups
#NB I had to fix one of the json files (there was a break in
     #one of the JSON files)
con <- file("Meetup data/tech_groups.json",'r')
json_read <- readLines(con)
tech_groups <- lapply(json_read,fromJSON)

#Business groups
con2 <- file("Meetup data/bus_groups.json",'r')
json_read2 <- readLines(con2)
biz_groups <- lapply(json_read2,fromJSON)

#Business event attendees
con4 <- file("Meetup data/bus_event_attendance.json",'r')
json_read4 <- readLines(con4)
bus_event_info <- lapply(json_read4,fromJSON)

#####
#CLEANING + PRE-PROCESSING
####
#1. Extract necessary data for topic modelling.
tech_groups_tm <- ldply(tech_groups,GetTmData)
biz_groups_tm <- ldply(biz_groups,GetTmData)

#Bind both kinds of groups
groups_tm <- rbind(tech_groups_tm,biz_groups_tm)
#nrow(groups_tm)

#2. Remove those whose last events happened in 2014.
     #Extract year from latest event date
     groups_tm$year_last_event <- year(groups_tm$latest_date)
     
     #subset groups with latest date in 2015 or 2016
     groups_tm_recent <- groups_tm %>%
          filter(!is.na(year_last_event),
                 year_last_event>=2015) %>% droplevels()

#nrow(groups_tm_recent)
#Additional preprocessing drawing on:
     #https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/
     
     #Corpus returns an object of the class corpus, ready for text mining
     #VectorSource takes a vector x where every element is a document
     group_docs <- Corpus(VectorSource(groups_tm_recent$topics))
     
     #tm_map transforms documents in the corpus.
     #In this case transform to lower case
     group_docs <- tm_map(group_docs,content_transformer(tolower))
     
     #Remove hyphens (they can create problems) using the tm_map function.
     group_docs <- tm_map(group_docs,content_transformer(function(x){
          gsub("-"," ",x)
     }))
     
     #Remove punctuation
     group_docs <- tm_map(group_docs,removePunctuation)
     
     #Remove numbers
     group_docs <- tm_map(group_docs,removeNumbers)
     
     #Remove stopwords
     group_docs <- tm_map(group_docs,removeWords,stopwords("english"))
     
     #Remove whitespace
     group_docs <- tm_map(group_docs,stripWhitespace)

#Document term matrix operations
#Create document-term matrix
dtm <- DocumentTermMatrix(group_docs)     

#Name its rows with the group ids
rownames(dtm) <- groups_tm_recent$id
    
#Collapse matrix
freq <- colSums(as.matrix(dtm))

#length(freq) = number of terms.
#index 
ord <- order(freq,decreasing=T)

#Write out word frequency
write.csv(freq[ord],"Meetup data/word_freq.csv")

#######
#TOPIC MODELLING
#######
#Again based on
     #https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/

# #What should we use for K
# best.model <- lapply(seq(2,50, by=1), function(k){LDA(dtm, k)})
# 
# #Extract the log-likelihood of the best model
# best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
# #And add the ks for plotting
# best.model.logLik.df <- data.frame(topics=c(2:50), LL=as.numeric(as.matrix(best.model.logLik)))
# 
# mod_perf_k <- ggplot(best.model.logLik.df, aes(x=topics, y=LL)) + 
#      xlab("Number of topics") + ylab("Log likelihood of the model") + 
#      geom_line() + 
#      theme_bw()  + 
#      theme(axis.title.x = element_text(vjust = -0.25, size = 14)) + 
#      theme(axis.title.y = element_text(size = 14, angle=90))
# 
# #The log likelihood plateaus after 30. Let's do 30.

#Set parameters
#Burnin (the initial iterations don't reflect the distribution)
burnin <- 4000
#Number of iterations of the algorithm
iter <- 2000
#Iterations for further use
thin <- 500
#Set seed
seed <- list(2003,5,63,100001,765)
#Number of starting points (independent runs)
nstart <- 5
#Return results with highest posterior probability
best <- TRUE
#Number of topics
k <- 30

#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

#Write out results
#Docs (groups) to topics
ldaOut.topics <- as.matrix(topics(ldaOut))

#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,10))

#Probabilities associated with each topic assignement
#Estimated probabilities for each group
topicProbabilities <- as.data.frame(ldaOut@gamma)

#Relative importance of top 2 topics
#Gives us a sense of robustness in the classification
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
     sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])

#Relative importance of 2nd/3rd most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
     sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])

#Further checking
#Draw 5 random groups for each label
#Create DF to split
df_labels <- data.frame(index=row.names(ldaOut.topics),
                        group = ldaOut.topics,
                        row.names = NULL)

check_descriptions <- ldply(split(df_labels,
                                  df_labels$group),
                            function(x){
                                 
                                 #Sample 5 group ids
                                 ids <- sample(x$index,5)
                                 
                                 #Extract indices
                                 indices <- as.factor(groups_tm$id) %in% 
                                      as.factor(ids)
                                 
                                 #Get those
                                 desc <- groups_tm[indices,
                                                          c("id","name","topics",
                                                            "description")]
                            })

WriteOut(check_descriptions,"final-report-data/")
topic_label_lookup <- read.csv("Meetup data/meetup_group_labels.csv")     


#Now: explore robustness of classification for different groups
#Extract probs of 1st label vs. 2 label
labelling_robustness_df <- 
     ldply(topic1ToTopic2,
           function(x){
                return(melt(x))
           }) %>%
     mutate(label=as.numeric(gsub("V","",variable)))

#Label variables
labelling_robustness_df2 <-
     merge(labelling_robustness_df,
           topic_label_lookup[,c(1,2)],by.x="label",
           by.y="label",sort=FALSE)

#Plot
rob_lab_plot <- ggplot(data=labelling_robustness_df2,
                       aes(value))+
     geom_histogram()+
     facet_wrap(~name,nrow=6,ncol=5)

#Write out plot
WriteChart(rob_lab_plot,"final-report-figures/",w=9,h=6)

#Some of the labels - specially those related to entrepreneurship and 
     #SMEs are less well-cut.
#Correlation matrix between labels.
#Create relabelling function
label_string_list <- as.list(1:30)
names(label_string_list) <- topic_label_lookup$name

#Corr_matrix
colnames(topicProbabilities) <- names(label_string_list)
topic_correlations <- cor(topicProbabilities,
                          method="pearson")
cor_palette <- colorRampPalette(c("darkblue","white","coral"))(n=200)

pdf("final-report-figures/topic_correlation.pdf",w=9,h=6)
heatmap(topic_correlations,col=cor_palette,cexRow = 0.6,cexCol=0.75,
        margins = c(11,11))
dev.off()

#Print creative labels
writeLines(paste0(sort(capitalize(as.character(
     topic_label_lookup$name[topic_label_lookup$creative==1]))),
       collapse=", "))

#Get number of creative groups.
lda_outputs_labels <- data.frame(label=ldaOut.topics) %>%
     merge(topic_label_lookup,by.x="label",by.y="label") %>%
     filter(creative==1) %>% droplevels()
sort(table(lda_outputs_labels$name))

#######
#4. Additional processing
#######

#We want to allocate meetups into TTWAs using the
     #point in polygon method.
#First we need to select meetups we are interested in.

#Those that are creative
groups_creative <- tbl_df(data.frame(rownames(ldaOut.topics),
                       ldaOut.topics)) %>% 
     merge(topic_label_lookup,by.x="ldaOut.topics",by.y="label") %>%
     rename(group_id=rownames.ldaOut.topics.,
            label=ldaOut.topics) %>%
     filter(creative==1) %>% merge(groups_tm,
                                   by.x="group_id",
                                   by.y="id") %>%
     
#Convert lon and lat in those groups into spatial points.
cord_groups <- SpatialPoints(groups_creative[,c("lon","lat")],
                             proj4string = CRS("+proj=longlat"))
#Convert to Eastings and Northings
coord_groups_en <- spTransform(cord_groups, CRS("+init=epsg:27700"))

#readOGR and do the point in polygon thing.
ttwa_polis <- readOGR("metadata/Travel_to_Work_Areas_(UK)_2011_boundaries_(super_generalised_clipped)_V3/",
                      layer="TTWA_2011_UK_BSC_V3")

proj4string(coord_groups_en) <- proj4string(ttwa_polis)

#Check that we are placing the groups right
inside.uk <- !is.na(over(coord_groups_en, as(ttwa_polis, "SpatialPolygons")))

#Run point of polygon and return the TTWA name
coord_groups_en$ttwa <- over(coord_groups_en, 
                             ttwa_polis)$TTWA11NM

coord_groups_en$group_id <- groups_creative$group_id

#Put in a dataframe with other relevant data
groups_creative_geotg <- as.data.frame(coord_groups_en) %>%
     merge(groups_creative,
           by.x="group_id",
           by.y="group_id") %>%
     select(-lon.y,-lat.y,-creative,-label)

#####
#5. Pre-processing for network analysis
#####
#Tasks
#Load and parse user data. Filter only active users and users in
     #creative groups.
#Loop over list of creative group ids
     #Get members ids from groups json.
     #Get other user info from the user df.
     #In particular: their country, created and modified dates,topics.
     #This give us a df with colnames.
          #group_ttwa group_name group_topic user_id user_id country created modified
#We can do several things with this group. We can:
     #Subset it by group TTWA, deduplicate and calculate...
          #total unique members by TTWA.
          #Split over m_groups and generate combinations of nodes
               #to plot a network - estimate some of its stats.
                    #clustering coefficient
                    #average path length
                    #density...
          #split over pairs of topics and generate a 
               #matrix of shared overlaps.
                    #Calculate average.
#Loop over TTWAs in the DFA and generate counts of user overlaps and use that... 
     #to produce a network map of cross-TTWA connectivity.
#Plot TTWAs vs. countries (will need to get a centroid for each country)

#1. Load and filter user data.          
#Process
#Ldply over file names. This includes loading, parsing and 
     #extracting relevant fields.

user_files <- list("tech"="tech_users.json",
                   "biz"="bus_users.json")

users_parsed_list <- lapply(user_files,
                  function(x){
                       #Read and parse data
                       file_name <- paste0("Meetup data/",x)
                       con <- file(file_name,'r')
                       file_read <- readLines(con)
                       file_parsed <- lapply(file_read,fromJSON)
                       return(file_parsed)})

users_parsed_unlist <- unlist(users_parsed_list,recursive = FALSE)

#Do it for all
users_extracted2 <- ldply(users_parsed_unlist,
                          GetUserData,.id=NULL,.progress="text")



# users_extracted <- GetUserData(users_parsed_unlist[[1]])
# write.csv(users_extracted,"final-report-data/users_first_65000.csv")
# 
# #Extract with a for loop (euch)
# for (i in 2:length(users_parsed_unlist)){
#      print(i)
#      users_extracted <- rbind(users_extracted,
#                               GetUserData(users_parsed_unlist[[i]]))
# }
# 
# #Do it with a loop over ldplys
# 
# for (i in 0:390){
#      print(i)
#      start <-65000 + i*1000 + 1
#      end <- 65000 + (i+1)*1000
#      extract <- ldply(users_parsed_unlist[start:end],GetUserData,
#                       .id=NULL)
#      users_extracted <- rbind(users_extracted,extract)
# }
# 
# 
# 
# 
# 
# 
