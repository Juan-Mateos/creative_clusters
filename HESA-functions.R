# #HESA data functions
# 
# #Get_HE_Address function:
# #Input: a string (HE name)
# #Process: Gets and parses data from GTR API
# #http://gtr.rcuk.ac.uk/resources/api.html
# #It also uses the GetOrganisationAddress from the
# #gtr script
# #Returns: the address of the HE
# Get_HE_Address <- function(x) {
#         #Delay
#         InsertDelay(1)
#         
#         #Create URL
#         cleanTerm <- tolower(gsub(" ","+",x))
#         gtrUrl <- paste0("http://gtr.rcuk.ac.uk/search/organisation.json?term=",
#                          cleanTerm)
#         #Get and Parse Data
#         gtrRequest <- GET(gtrUrl)
#         if (gtrRequest$status_code == 200) {
#                 gtrResults <- content(gtrRequest)[["results"]]
#                 
#                 #Extract the Address
#                 address <- GetOrganisationAddress(gtrResults[[1]]$organisation$url)
#                 return(data.frame(name.returned=gtrResults[[1]]$organisation$name,address))
#         }
# }


#GetQualStats function to get total qualifiers by subject and 
#location
#Input: x a year
        #y (optional = a vector of "deep dive" subjects for ranking)
        #z a data table with the data
        #place: the geography we want to use (defaults to ttwa.name)
#Returns a df with the total number of qualifiers by subject,
#as well as the qualifiers as % of residents, 
# and qualifiers by discipline as a % of all graduates
#Process: Create a long dataframe, split it by ttwa and calculate
#the other statistics.
#To create the denominator, we will do a similar split by
#subject and merge.

GetQualStats <- function(x,y=NULL,z) {
        #Select the year
        qualifiers.selected <- z %>% filter(year==x)
        
        #Create data table with ttwas and subjects
        area.subject.totals <- qualifiers.selected %>%
                group_by(ttwa.name,subject.short) %>%
                summarise(qual.totals = sum(total,na.rm=T))
        
        #Create data table with subjects nationally (also to get
        #the denominator for the LQs)
        subject.totals <- qualifiers.selected %>%
                group_by(subject.short) %>%
                summarise(subject.totals=sum(total,na.rm=T)) %>%
                mutate(subject.weight=subject.totals/sum(subject.totals))
        
        #Merge both
        area.subjects <- tbl_df(merge(area.subject.totals,
                                      subject.totals[,c(1,3)],
                                      by.x="subject.short",
                                      by.y="subject.short",sort=F)) %>% droplevels()
        
        
        #Split by TTWA, generate proportions and LQs, combine and apply
        subj.area.split <- split(area.subjects,area.subjects$ttwa.name)
        output.df <- tbl_df(ldply(subj.area.split,
                           function(x){
                                   x$qual.prop <- x$qual.totals/
                                           sum(x$qual.totals,na.rm=T)
                                   x$lq <- x$qual.prop/x$subject.weight
                                   return(x)
                           }))
        output.df <- output.df %>% select(-.id)
                           
        output.df$qual.perc <- Percentify(output.df$qual.prop)
        
        if (is.null(y)==FALSE) {
                subject.rankings <- lapply(
                        y,function(x){
                                myDf <- output.df %>%
                                        filter(subject.short==x) %>%
                                        arrange(desc(lq))
                                myDf2 <- myDf %>% select(subject.short,
                                                         ttwa.name,qual.totals,
                                                         lq)
                                myDf2$nat.prop <- Percentify(myDf2$qual.totals/
                                                                     sum(myDf2$qual.totals,
                                                                         na.rm=T))
                                return(myDf2)
                        })
                return(list(output.df,subject.rankings))
        } else {
                return(output.df)
        }
}
        

#Get JACS stats provides data about qualifiers in a location.
        #Inputs: x=year (defaults to 2013/14)
        #y= vector with 4 digitJACS codes of interest
        #z = the data table we are wowrking with
        #top = how many subjects to represent (sorted by popularity)

GetJacsStats <- function(x="2013/14",y,z,top) {
        #Select the year
        qualifiers.selected <- z %>% filter(year==x & jacs.new %in% y)
        
        #Find top sectors
        topSectors <- tapply(qualifiers.selected$total,
                             qualifiers.selected$labels.clean,sum,na.rm=T) %>% 
                sort(decreasing=T) %>% names %>% extract(1:top)
        
        #Relabel sectors not in top as "other" (i.e. focus on the top sectors)
        notInTop <- !(qualifiers.selected$labels.clean %in% 
                              topSectors)
        
        qualifiers.selected$labels.clean[notInTop] <- "Other"
        
        #Create data table with area summaries
        area.jacs.totals <- qualifiers.selected %>%
                group_by(ttwa.name,labels.clean) %>%
                summarise(qual.totals = sum(total,na.rm=T))
        
        #Create national sector summaries
        jacs.totals <- qualifiers.selected %>%
                group_by(labels.clean) %>%
                summarise(jacs.totals=sum(total,na.rm=T)) %>%
                ungroup() %>% arrange(desc(jacs.totals)) %>%
                mutate(subject.weight=jacs.totals/sum(jacs.totals))
        
        #Merge both
        area.jacs <- tbl_df(merge(area.jacs.totals,
                                      jacs.totals[,c(1,3)],
                                      by.x="labels.clean",
                                      by.y="labels.clean",sort=F))
        
        #Split by TTWA, generate proportions and LQs, combine and apply
        jacs.area.split <- split(area.jacs,area.jacs$ttwa.name)
        output.df <- tbl_df(ldply(jacs.area.split,
                                  function(x){
                                          x$qual.prop <- x$qual.totals/
                                                  sum(x$qual.totals,na.rm=t)
                                          x$lq <- x$qual.prop/x$subject.weight
                                          return(x)
                                  }))
        output.df <- output.df %>% select(-.id) %>%
                mutate(location.perc = Percentify(qual.prop))
        
        #Create an additional list where elements = ranking by sector
        jacs.rankings <- lapply(topSectors,
                                function(x) {
                                        myDf <- output.df %>%
                                                filter(labels.clean==x) %>%
                                                arrange(desc(qual.totals))
                                        myDf2 <- myDf %>% select(labels.clean,
                                                                 ttwa.name,qual.totals,
                                                                 lq)
                                        myDf2$nat.prop <- Percentify(myDf2$qual.totals/
                                                                             sum(myDf2$qual.totals,
                                                                                 na.rm=T))
                                        return(myDf2)
                                })
        #Return outputs
        return(list(output.df,jacs.rankings))
}

#Df for treemap
library(treemap)
#GetSubjectTreemap: Create a function that generates treemaps for the right subject areas
#Inputs:x is a subject
#       y is the label for the plot
#       z is the output dir
#It exports a pdf of the treemap

GetTreeMap <- function(x,y,z) {
        ttwa.relevant.subjects.14 <- GetQualStats(x="2013/14",z=qualifiers_tb2) %>%
                filter(subject.short==x) %>% 
                mutate(ttwa.values= paste0(ttwa.name,"\n",round(qual.totals,0)))
        pdf(paste0(z,x,"-treemap-",Sys.Date(),".pdf"))
        treemap(as.data.frame(ttwa.relevant.subjects.14),
                index="ttwa.values",
                vSize="qual.totals",
                vColor="lq",
                type="value",
                palette="Spectral",
                title=paste0(y,":Number of qualifiers and specialisation, 2013/14 (HESA)"),
                title.legend="location quotient"
        )
        dev.off()
}



#Table with activity in top 15 disciplines and top 20 TTWAs
#Create GetLevePlot function: gives a leveplot of activity in
#a vector of JACS codes of interest (including ranking)
#Inputs:x: a vector of subjects of interest
#       y: top number of subjects to display
#       z: top number of ttwas to display
#       name for the plot
GetLevelPlot <- function(x,y,z,name) {
        #Function that returns the table
        ttwa.jacs <- GetJacsStats(x="2013/14",
                                  y=x,z=qualifiers_tb2,
                                  top=y)
        
        #Get top z TTWAs
        topZttwas <- ttwa.jacs[[1]] %>% group_by(ttwa.name) %>% 
                summarise(totals=sum(qual.totals)) %>% ungroup() %>% arrange(desc(totals)) %>%
                extract(1:z,1)
        
        #Filter by those
        jacs.in.selected.ttwas.subjects <- ttwa.jacs[[1]] %>% 
                filter(ttwa.name %in% topZttwas$ttwa.name) %>% 
                select(labels.clean,ttwa.name,lq)
        
        jacs.ttwas.df <- as.data.frame(jacs.in.selected.ttwas.subjects)
        
        #Reorder ttwa.name by total number of qualifiers (using 
        #the vector topZttwas)
        jacs.ttwas.df <- droplevels(jacs.ttwas.df)
        jacs.ttwas.df$ttwa.name <- ReorderFactor(jacs.ttwas.df$ttwa.name,
                                                 y=topZttwas$ttwa.name,z=F)
        
        #Reorder the subject vector by popularity (number of
        #vectors with at least activity)
        jacs.rank <- jacs.in.selected.ttwas.subjects %>% 
                group_by(labels.clean) %>% 
                summarise(tots=n_distinct(ttwa.name)) %>%
                ungroup() %>% arrange(tots) %>% extract(,1)
        
        jacs.for.ordering <- jacs.rank$labels.clean
        
        #Reorder subject vector in ttwadf
        jacs.ttwas.df$labels.clean <- ReorderFactor(jacs.ttwas.df$labels.clean,
                                                    y=jacs.for.ordering,
                                                    z=T)
        
        #Plot
        ttwa.subjects.plot <- ggplot(data=jacs.ttwas.df,
                                     aes(x=ttwa.name,y=labels.clean))+
                geom_tile(aes(fill=log(lq+0.01)),colour="black")+
                labs(x=NULL,y=NULL,title="Level-plot with relevant subjects, top 25 TTWAs",
                     fill="location quotient (logged)")+
                scale_fill_gradient(high="darkblue",low="lightblue",na.value="white")+
                theme(axis.text.x=element_text(angle=45,hjust=1,size=9),
                      #legend.position=c("bottom"),
                      axis.ticks.x=element_blank(),
                      axis.ticks.y=element_blank(),
                      panel.grid=element_blank())
        
        pdf(paste0("Analysis/Figures/Levelplot-",name,"-",Sys.Date(),".pdf"),
            width=9,height=6)
        print(ttwa.subjects.plot)
        dev.off()
}

#Use a GetBarChart function
#Inputs:subjects: a vector of subjects of interest
#       top: total number of ttwas to display, by popularity
GetBarChart <-function(subjects,top) {
        subjects.for.barchart <- GetQualStats(x="2013/14",y=subjects,qualifiers_tb2)
        
        mySubjects <- subjects.for.barchart[[2]]
        #Rbind subjects
        subject.barchart_tbl <- do.call(rbind,mySubjects)
        
        #Get subject ranking
        rankedTtwas <- subject.barchart_tbl %>% group_by(ttwa.name) %>%
                summarise(tots=sum(qual.totals)) %>% ungroup() %>% arrange(desc(tots)) %>%
                extract(1:top,1)
        subject.barchart_tbl$ttwa.name <- ReorderFactor(subject.barchart_tbl$ttwa.name,
                                                        rankedTtwas$ttwa.name,z=T)
        
        subject.barchart_tbl <- subject.barchart_tbl %>% filter(!is.na(subject.barchart_tbl$ttwa.name))
        
        ttwa.subject.bar <- ggplot(data=subject.barchart_tbl,
                                   aes(x=ttwa.name,y=qual.totals,fill=lq))+
                geom_bar(stat="identity")+
                scale_fill_gradient(high="darkblue",low="lightblue")+
                facet_grid(.~subject.short)+coord_flip()+
                labs(y="Total qualifiers",x=NULL,fill="Location quotient",
                     title=paste0("Number of qualifiers and specialisation, 2013/14 (HESA)"))
}




