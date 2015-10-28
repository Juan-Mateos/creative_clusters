#R Script to read in data for the creative clusters update project.
######
#0. PREPARATION
######

#Set working directory
setwd("/Users/juanmateos-garcia/Desktop/2015 core/Creative Clusters")

#Install generic packages
source("/Users/juanmateos-garcia/Desktop/Tools/R-general_purpose_scripts/genericPackages.R")

#Read function scripts
source("/Users/juanmateos-garcia/Desktop/Tools/R-general_purpose_scripts/myUtilityFunctions-6Oct2015.R")
source("Rcode/CClustersVMLfunctions-6October2015.R")

#Create data outputs and figures folders.
CreateDirs("/Users/juanmateos-garcia/Desktop/2015 core/Creative Clusters/",
           "data-outputs")
CreateDirs("/Users/juanmateos-garcia/Desktop/2015 core/Creative Clusters/",
           "figures-outputs")
CreateDirs("/Users/juanmateos-garcia/Desktop/2015 core/Creative Clusters/",
           "metadata")

#Some parameters for exporting results with the Write functions
outputdir <- "figures-outputs/"
width <- 20
height <- 11

######
#1.READ
######

#Read data from Excel spreadsheet which contains the information I need.
#Load worksheet to get sheetNames
enterpriseData <- loadWorkbook("vml-data-6october2015/spd - data for creative clusters - enterprise unit - v2 - 06-10-15.xls")
sheetNames <- names(getSheets(enterpriseData))

#Focus on sheet names with content (excluding those with ">>")
sheetNames.data <- sheetNames[!grepl(">>",sheetNames)]

#Read the data in with the function ReadWorkSheet
enterprise.data <- lapply(sheetNames.data,
                         ReadWorkSheet,
                         x="/Users/juanmateos-garcia/Desktop/2015 core/Creative Clusters/vml-data-6october2015/spd - data for creative clusters - enterprise unit - v2 - 06-10-15.xls",
                         z=3)
names(enterprise.data) <- gsub("-",".",sheetNames.data)


#The sheet names are not sufficiently informative on their own.
#We create a better vector of names for the datasets, nicely formatted
datasetNames <- tolower(paste0(names(enterprise.data),
                    c(rep("",2),
                    rep(".all.creative",3),
                    rep(".subsector",2),
                    rep(".all.industries",3))))

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
        
#         #Add a industry shorter name if we have that variable
#         if ("industry" %in% names(myDataFrame)) {
#                 myDataFrame$industry.short <- myDataFrame$industry
#                 
#                 levels(myDataFrame$industry.short) <- 
#                         c("advertising","architecture","design",
#                           "film.TV.video","software",
#                           "museums.galleries.libraries",
#                           "music","other","publishing")
#         }
#         
        #Rename
        name <- datasetNames[i]
        assign(name,myDataFrame)
        remove(myDataFrame)
}

#Read GVA data
gva.country.data <- ReadWorkSheet(x="/Users/juanmateos-garcia/Desktop/2015 core/Creative Clusters/vml-data-6october2015/spd - data for creative clusters - GVA - 06-10-15.xls",
                                  y="country",3)

gva.region.data <- ReadWorkSheet(x="/Users/juanmateos-garcia/Desktop/2015 core/Creative Clusters/vml-data-6october2015/spd - data for creative clusters - GVA - 06-10-15.xls",
                                 y="region",3)

#####
#2.CLEAN
#####

#Before binding gor and region, remove Scotland,
        #Wales and NI from the GOR df (they are 
        #already in the country df)
ukCountries <- grep("^S|^W|^N",region.subsector$region.code)
region.subsector2 <- region.subsector[-ukCountries,]

#Bind dataframes for analysis and plotting
gor.subsector <- BindAreas(country.subsector,region.subsector2)
ttwa.all.cis <- BindAreas(ttwa.2007.all.creative,ttwa.2010.all.creative,
                          ttwa.2013.all.creative)
ttwa.subsector <- BindAreas(ttwa.2007.09.subsector,ttwa.2010.13.subsector)

levels(gor.subsector$industry)

#Rename industry levels for plotting etc.
gor.subsector$industry.short <-
        factor(gor.subsector$industry,
               labels=c("advertising","architecture","design",
                                                  "film.TV.video","software",
                                                  "museums.galleries.libraries",
                                                  "music","other","publishing"))

#Add regions/countries names
#We use a lookup from the ONS open Geography Portal
codeUrl <- "https://geoportal.statistics.gov.uk/Docs/Names%20and%20Codes/Government_office_regions_(Eng)_2010_Names_and_Codes.zip"
GetUnzip(codeUrl,"metadata")

#Read the codes
gor.codes <- read.csv("metadata/GOR_2010_EN_NC.csv")

#Merge
gor.subsector.coded <- merge(gor.subsector,gor.codes[,c(1,3)],
                             by.x="country.code",
                             by.y="GOR11CD",
                             all.x=TRUE)

#We need to add Scotland, Wales and NI by hand.

#First we add those as levels in the variable
gor.subsector.coded$GOR11NM <- AddLevels(gor.subsector.coded$GOR11NM,
                                         c("Scotland","Wales","Northern Ireland","England"))
for (i in c("Scotland","Wales","Northern Ireland")) {
        initial <- paste0("^",str_sub(i,1,1))
        gor.subsector.coded$GOR11NM[grep(initial,
                                         gor.subsector.coded$country.code)] <-
                i
}
#Add England
gor.subsector.coded$GOR11NM[grep("^E9",
                                 gor.subsector.coded$country.code)] <-"England"

#Rename the variable to something more readable
names(gor.subsector.coded)[length(names(gor.subsector.coded))] <- "area"

#Replace gor.subsector.
gor.subsector <- gor.subsector.coded

######
#ESTIMATE GVA PER TTWA
######

#####
#ANALYSE AND PLOT
#####

#####
#GOR
####

#Produce treemap of creative employment by region
        #in all the creative industries, 2013.
library(treemap)

gor.subsector.noEng <- subset(gor.subsector.coded,
                                 area !="England")
gor.subsector.noEng <- droplevels(gor.subsector.noEng)

#Create a dataframe for plotting treemap
gor.subsector.2013 <- subset(gor.subsector.noEng,
                             select=c("industry.short",
                                      "employment",
                                      "area"),
                             year==2013)

gor.subsector.2013 <- droplevels(gor.subsector.2013)

#Calculate location quotients requires making the df wide after
        #splitting it by variable (which requires melting)
gor.subsector.2013.melt.temp <- melt(gor.subsector.2013,
                                      id.vars=c("industry.short",
                                                "area"))
gor.subsector.2013.melt.temp2 <- subset(gor.subsector.2013.melt.temp,
                                        select=c("industry.short",
                                                 "area","value"))
                                     
emp.2013.totals <- rowSums(dcast(gor.subsector.2013.melt.temp2,
                         area~industry.short,value.var="value")[,-1])

gor.subsector.2013.lqs <- ldply(split(gor.subsector.2013.melt.temp,
                                      gor.subsector.2013.melt.temp$industry.short),
                                function(x) {
                                        x$lq <- LQ(x$value,emp.2013.totals)
                                        return(x)
                                })


pdf("figures-outputs/treemap.pdf",width=20,height=11)
treemap(gor.subsector.2013.lqs[
        gor.subsector.2013.lqs$industry.short != "other",],
        #Aesthetics
        index=c("area", "industry.short"),
        vSize="value",
        vColor="lq",
        type="value",
        
        #Formatting
        title="Employment in Creative Sub-sector by Region/Nation (BSD, 2013)",
        fontsize.title=24,
        palette="Spectral",
        border.lwds=c(3,1),
        fontsize.labels=c(18,12),
        align.labels=list(c("left","top"),c("center","center")),
        bg.labels=c("lightgrey")
)
dev.off()

#Barplot with concentration of employment in different sectors.
        
#Need to do split,apply,combine
gor.sector.table <- ldply(
        #Split
        split(gor.subsector.2013,
                                gor.subsector.2013$industry.short),
        #Apply
        function(x) {
                myDf <- subset(x,
                               select=c("employment","area"))
                
                #Create and clean the proportions 
                myDf$emp.prop <- myDf$employment/
                        sum(myDf$employment)
                myDf$emp.prop <- Percentify(myDf$emp.prop)
                return(myDf)
                          })

#Plot this.
#Ranking area levels according to top employment,
#And ranking sectors depending on geographical concentration
        #based on Herfindahl

gor.sector.table <- droplevels(gor.sector.table)
gor.subsector.2013 <- droplevels(gor.subsector.2013)

#Rank area levels by their total employment
areas.ordered <- names(sort(tapply(gor.sector.table$employment,
                        gor.sector.table$area,sum),decreasing=T))

#Calculate Herfindahl for sectors
sectors.herfindahl <- sapply(split(gor.subsector.2013,
                                   gor.subsector.2013$industry.short),
                             function(x) {
                                     return(
                                             sum((x$employment/sum(x$employment))^2))
                             })
sectors.ordered <- names(sort(sectors.herfindahl,decreasing=T))

#Reorder areas and sectors in gor.sector.table

gor.sector.table$.id <- ReorderFactor(gor.sector.table$.id,
                                      sectors.ordered,z=T)
gor.sector.table$area <- ReorderFactor(gor.sector.table$area,
                                       areas.ordered,z=F)

#Plot.
#Change the palette
myPalette <- brewer.pal(length(levels(gor.sector.table$area)),
                        "Paired")

sector.plot <- ggplot(data=gor.sector.table,
                      aes(x=.id,
                          y=emp.prop,fill=area,ordered=area))+
        geom_bar(stat="identity")+
        scale_fill_manual(values = myPalette)+
        labs(y="% employment",x=NULL,
             title="Distribution of sector by region/nation (BSD,2013)")+
        theme(legend.position=c("bottom"),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              title=element_text(size=18),
              axis.text.y=element_text(size=16),
              axis.text.x=element_text(size=16))+
        coord_flip()
WriteChart(sector.plot,y=outputdir,w=width,h=height)


#Faceted visualisation of changes in employment
        #and business counts by creative industries and
        #region
#Further subsetting of gor.subsector.plotting

gor.subsector.noEng.small <- subset(gor.subsector.noEng,
                                  select=c("year",
                                          "industry.short",
                                          "employment",
                                          "business.count",
                                          "turnover",
                                          "area"))

#Melt retaining industry, area and year as the ids
gor.subs.plotting.melt <- melt(gor.subsector.noEng.small,
                               id=c("industry.short","area",
                                    "year"))

#Now we need to normalise for each area-industry-variable pair.
#We do split, apply combine

#Split, pply and combine
gor.subs.fin.table <- ldply(split(gor.subs.plotting.melt,
                                  list(gor.subs.plotting.melt$area,
                                       gor.subs.plotting.melt$industry.short,
                                       gor.subs.plotting.melt$variable)),
                            function(x) {
                                    begin <- x$year==2007
                                    x$value.normalised <- 
                                            Percentify(x$value/x$value[begin])
                                    return(x)
                            })


                            
#Produce faceted plot with financials by sector/area

#Reorder levels and create palette
financials.ordered <- c("business.count","employment","turnover")
gor.subs.fin.table$variable <- ReorderFactor(gor.subs.fin.table$variable,
                                             financials.ordered,z=F)
#Palette
#Reorder sector levels for plot aesthetics (colour and thickness)
sectors.ordered <- c("advertising","architecture","design",
                     "film.tv.video","software",
                     "museums.galleries.libraries",
                     "music","publishing","other")
gor.subs.fin.table$industry.short <-
        ReorderFactor(gor.subs.fin.table$industry.short,
                      sectors.ordered,z=F)

myPalette2 <- c(brewer.pal(8,"Dark2"),"#000000")

#Create vector with line thickness (thicker for "other" category)
gor.subs.fin.table$lwd <- 1
gor.subs.fin.table$lwd[gor.subs.fin.table$industry.short=="other"] <-
        2

#Create plot
gor.subsector.plot <- ggplot(data=gor.subs.fin.table,
                             aes(x=year,y=value.normalised,
                                 color=industry.short,
                                 size=lwd
                                 ))+
        geom_line()+
        facet_grid(variable~area)+
        scale_color_manual(values=myPalette2)+
        
        #We want the lines to be thicker in the legend
        guides(colour = guide_legend(override.aes = list(size=2)))+
        
        #This is to control the scale of the line size
        scale_size(range=c(0.9,1.5),guide=FALSE)+
        labs(x=NULL,y="2007 = 100",
             title="Changes in business count,employment and turnover by area and sector (BSD,2013)",
             color="Creative sector")+
        theme(legend.text=element_text(size=16),
              legend.position=c("top"),
              title=element_text(size=18),
              strip.text=element_text(size=14))
        
#Output it
WriteChart(gor.subsector.plot,outputdir,w=width,h=height)

#In how many regions is a sector bigger in 2013 than in 2007?
#Cast: in the formula, the left hand side shows the
        #id vars, the right hand-side the variable we
        #want to cast, and the value variable is the 
        #value we want to cast
gor.subs.comparing <- dcast(gor.subs.fin.table,
                             industry.short+area+variable~year,
                            value.var="value.normalised")
#And we do a split apply combine over these
gor.sectors.changes <- ldply(split(gor.subs.comparing,
                 list(gor.subs.comparing$industry.short,
                      gor.subs.comparing$variable)),
                 function(x) {
                        pgrow <- Percentify(sum(x[,"2013"] > 
                                                        x[,"2007"])/nrow(x))
                        return(pgrow)
                 })
names(gor.sectors.changes) <- c("sector-variable","Pc.bigger.in.2013")

#Next step: compare labour productivity and
        #average company size at the regional level, by sector.

gor.subsector.noEng.small$lab.prod <- 
        gor.subsector.noEng.small$turnover/
        gor.subsector.noEng.small$employment
gor.subsector.noEng.small$avg.firmsize <- 
        gor.subsector.noEng.small$employment/
        gor.subsector.noEng.small$business.count

gor.subs.plotting.econ.melt <- melt(gor.subsector.noEng.small,
                                    id.var=c("year","industry.short",
                                             "area"))
gor.econ.table <- subset(gor.subs.plotting.econ.melt,
                                 variable == "lab.prod" |
                                 variable == "avg.firmsize")

#Reorder levels for the colour palette
gor.econ.table$industry.short <- ReorderFactor(gor.econ.table$industry.short,
                                               sectors.ordered,z=F)

#Now plot these two variables grouped by areas, without faceting
        #because their scales are different
#Use EconPlotFunction
#Input: x: variable of interest
#       y: dataframe
#Returns the plot we need

PlotEconVar <- function(x,y) {
        if (x=="lab.prod") {
                myvar <- "Turnover per worker"
                y.axis <- "£GBPTh/worker"
        } else {
                myvar <- "workers per company"
                y.axis <- "Workers per company"
        }
        
        selected <- y$variable==x
        myPlot <- ggplot(data=y[selected,],
                                     aes(x=year,y=value,color=industry.short))+
                geom_line(size=1)+
        scale_color_manual(values=myPalette2)+
        guides(colour = guide_legend(override.aes = list(size=2)))+        
        
        facet_grid(.~area)+
        labs(x=NULL,y=y.axis,
             title=paste("Change in",myvar,"2007-2013 (BSD)"),
             colour="Creative sector")+
        theme(title=element_text(size=18),
              legend.text=element_text(size=14),
              legend.position=c("top"))
        return(myPlot)
}
#Create the two plots
lab.prod.plot <- PlotEconVar("lab.prod",
                             gor.econ.table)
avg.firm.plot <- PlotEconVar("avg.firmsize",
                             gor.econ.table)

#Export them
WriteChart(multiplot(lab.prod.plot,avg.firm.plot),
           outputdir,w=width,h=height)

#The above will have to be tidied (especially the names)

#####
#TTWA
#####
#Identify creative clusters:
#Create a table for exporting with
        #TTWA names and their status.

creative.clusters <- data.frame(
        ttwa=ttwa.2013.all.creative$ttwa.name..2007.,
        employment = ttwa.2013.all.creative$employment,
        emp.lq = ttwa.2013.all.creative$location.quotient...employment,
        business.count = ttwa.2013.all.creative$business.count,
        bc.lq = ttwa.2013.all.creative$location.quotient...business.count)

#Create the two data.frame tests
#Employment
employment.tests <- Clusters.test(ttwa.2013.all.creative$location.quotient...employment,
                                  y=0.9,z=1)
names(employment.tests) <- paste0(names(employment.tests),"-emp")

#Business
business.tests <- Clusters.test(ttwa.2013.all.creative$location.quotient...business.count,
                                y=0.9,z=1)
names(business.tests) <- paste0(names(business.tests),"-bc")

#Combine them into a single df and export it
creative.clusters.tests <- cbind(creative.clusters,
                                 employment.tests,
                                 business.tests)
WriteOut(creative.clusters.tests,y="data-outputs/")

#CompareMeans at the sector level
#We need to split the ttwa.2010.13.subsector df into one
        #with employment data and another with busins.count data.
#Then we dcast it, compare means for each LQ and cbind to 
#the creative clusters test one. Any other outputs?
        #Sum how many sectors for clusters appear specialised?

#Modify industry levels
ttwa.2010.13.subsector$industry.short <- 
        factor(ttwa.2010.13.subsector$industry,
               labels=
                       c("advertising","all.creative",
                         "architecture","design","film.TV.radio",
                         "software","music","publishing"))

#How do we select the clusters? 
#More than 2 standard deviations over the mean in
        #at least one creative sector
#And some minimum level of activity 
        #(at least in the top quartile in employment and business counts)?
employment.clusters <- GetCreativeClusters(ttwa.2010.13.subsector,
                                           y="employment",z=2,p=0.75)
business.clusters <- GetCreativeClusters(ttwa.2010.13.subsector,
                                         y="business.count",z=2,p=0.75)

myCreative.clusters <- sort(unique(c(employment.clusters$cluster.names,
                                   business.clusters$cluster.names)))


#We need to test the robustness of all this.
#Look at the sensitivity of the numbers to selecting different
        #thresholds in terms of standard deviations?
#Create a table with 
        #number of selected locations per sd specification and variable,

cluster.robust.df <- ClusterRobustness()
cluster.robust.long <- melt(cluster.robust.df,
                               id.vars=c("sd","prop","all.clusters","new.clusters"))

#Plot robustness
#With all data
myRob.plot <- ggplot(data=cluster.robust.long,
                     aes(x=sd,y=value,color=variable))+
        geom_line()+facet_grid(.~prop)

#Only looking at clusters in the top quartile of scale by variable
rob.plot.topQ <- ggplot(data=subset(cluster.robust.long,
                                    prop=="0.75"),
                        aes(x=sd,y=value,color=variable))+
        geom_line(size=1.5)+
        labs(x="Threshold standard deviations above mean LQ",
             y="Identified clusters",
             colour="Variable",
             title="Number of creative clusters identified with different thresholds")+
        scale_colour_discrete(labels=c("Employment","Business",
                                       "Employment or business"))+
        geom_vline(aes(xintercept=2),linetype=2,size=1.6)+
        geom_vline(aes(xintercept=1.5),linetype=2,size=1)+
        geom_vline(aes(xintercept=2.5),linetype=2,size=1)+
        theme(axis.text=element_text(size=16),
              legend.title=element_text(size=16),
              legend.text=element_text(size=16),
              title=element_text(size=18))


WriteChart(rob.plot.topQ,y=outputdir,w=width,h=height-2)

#####
#Mapping creative clusters in the UK.
#####

require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")

GetUnzip("https://geoportal.statistics.gov.uk/Docs/Boundaries/Travel_to_work_areas_(UK)_2001_Boundaries_(Full_Extent).zip",
         "metadata/")
#Read the data
uk <- readOGR("metadata/Travel_to_work_areas_(UK)_2001_Boundaries_(Full_Extent)/",
              layer="TTWA_2001_UK_BFE")
uk@data$id <- row.names(uk@data)
uk.points <- fortify(uk,region="id")

uk.ttwa.df <- join(uk.points,uk@data,by="id")

#Now we want to merge this with df with the one that has the LQs
#Let's find it first.

#DF for plotting.
#Subset TTWA data
#Employment LQs by sector
ttwa.emp.lqs <- subset(employment.clusters$all.places, select=c(1:8))
names(ttwa.emp.lqs)[2:ncol(ttwa.emp.lqs)] <-
        paste(names(ttwa.emp.lqs)[2:ncol(ttwa.emp.lqs)],"emplq",sep="-")
#Business count LQs by sector
ttwa.bc.lqs <- subset(subset(
        business.clusters$all.places, select=c(1:8)))
names(ttwa.bc.lqs)[2:ncol(ttwa.bc.lqs)] <-
        paste(names(ttwa.bc.lqs)[2:ncol(ttwa.bc.lqs)],"bclq",sep="-")


#Combine lq dfs into a data.frame (will also use this for co-location
        #analysis)
ttwa.lq.df <- data.frame(ttwa.emp.lqs,ttwa.bc.lqs[,-1])

#Merge with the uk.ttwa.df
ttwa.mapping.df <- merge(uk.ttwa.df,ttwa.lq.df,
                         by.x="TTWA01NM",by.y="ttwa")


#Melt and plot...

#MapSubsectors function returns a map for a sector and 
        #variable (employment of business count), including 
        #A list of the locations below the map.
#Input: x: a dataframe
#       y: a sub-sector
#       z: a measure (emp or bc)
#Returns: The map object

MapSubsectors <- function(x,y,z) {
        #Subset DF
        varname <- paste0(y,".",z,"lq")
        myVariable <- grep(varname,names(x))
        myDf <- x[c(1:7,myVariable)]
        
        #GetNames
        topNames <-unique(myDf[order(myDf[,varname],decreasing=T),1])[1:5]
        topNames.together <- paste(
                c("TOP TTWAs",
                        as.character(topNames)),collapse="\n ")
                
        #Plot
        myMap <- ggplot(data=myDf,aes_string(x="long",y="lat",group="group",
                                      fill=varname))+
                geom_polygon()+
                #Add top cities for each cluster
                annotate("text",x=5.5e+05,y=800000,label=topNames.together,
                         size=3)+
                labs(x=NULL,y=NULL,
                     title=paste(y,z,"specialisation",sep=" "))+
                scale_fill_gradient(low="#0057E7",high="#FFA700")+
                theme(
                        panel.grid=element_blank(),
                        axis.ticks=element_blank(),
                        axis.text=element_blank(),
                        panel.background=element_rect(fill="white"))
        #assign(varname,myMap)
        return(myMap)
}

#Subsector names excluding "all industries" category
subsector.names <- levels(ttwa.2010.13.subsector$industry.short)[-2]

#Loop over all sector names
emp.maps <- lapply(subsector.names,
                      MapSubsectors,x=ttwa.mapping.df,z="emp")

#Create multiplot
WriteChart(multiplot(emp.maps[[1]],
                     emp.maps[[2]],
                     emp.maps[[3]],
                     emp.maps[[4]],
                     emp.maps[[5]],
                     emp.maps[[6]],
                     emp.maps[[7]],
                     layout=matrix(c(1,2,3,4,5,6,7,8),ncol=4,byrow=T)),
           outputdir,
           w=width, h=height)

#Business count maps (need to output as multiple plots)
bc.maps <- lapply(subsector.names,
                   MapSubsectors,x=ttwa.mapping.df,z="bc")
WriteChart(multiplot(bc.maps[[1]],
                     bc.maps[[2]],
                     bc.maps[[3]],
                     bc.maps[[4]],
                     bc.maps[[5]],
                     bc.maps[[6]],
                     bc.maps[[7]],
                     layout=matrix(c(1,2,3,4,5,6,7,8),ncol=4,byrow=T)),
           outputdir,
           w=width, h=height)

#Create a map with myCreative.clusters. 
#Use four colours (employment c, business c, both, none)
ttwa.mapping.df$cluster.type <- factor(rep("none",nrow(ttwa.mapping.df)),
                                       levels=c("none",
                                                "employment","business",
                                                "both"))
#Get names for each category
emp.cluster.names <- employment.clusters$cluster.names
business.cluster.names <- business.clusters$cluster.names

#Clusters in business and employment
both.cluster.names <- intersect(business.cluster.names,emp.cluster.names)

#Clusters only in employment
only.emp.clusters <- setdiff(emp.cluster.names,both.cluster.names)

#Clusters only in business counts
only.business.clusters <- setdiff(business.cluster.names,both.cluster.names)

#Assign labels to the cluster.type variable in the Df.
ttwa.mapping.df$cluster.type[ttwa.mapping.df$TTWA01NM %in% both.cluster.names] <-
        "both"
ttwa.mapping.df$cluster.type[ttwa.mapping.df$TTWA01NM %in% only.business.clusters] <-
        "business"
ttwa.mapping.df$cluster.type[ttwa.mapping.df$TTWA01NM %in% only.emp.clusters] <-
        "employment"

#We want to add name labels to the map. Create table with centroids.
centroidsDf <- data.frame(aggregate(ttwa.mapping.df$long,
                                    list(ttwa.mapping.df$TTWA01NM),
                                    mean),
                          aggregate(ttwa.mapping.df$lat,
                                    list(ttwa.mapping.df$TTWA01NM),
                                    mean)[,2])
names(centroidsDf) <- c("TTWA01NM","long","lat")
#And names for labelling the map
centroidsDf$cluster.name <- as.character(centroidsDf$TTWA01NM)
centroidsDf$cluster.name[!(centroidsDf$TTWA01NM %in% myCreative.clusters)] <- ""

#Map them.
clusterMap <- ggplot(data=ttwa.mapping.df,
                     aes(x=long,y=lat,group=group,
                         fill=cluster.type))+
        geom_polygon()+
        geom_text(data=centroidsDf,
                  aes(x=long,y=lat,group=NULL,fill=NULL,
                      cluster.type=NULL,
                              label=cluster.name),size=1.8,vjust=0)+
        labs(x=NULL,y=NULL,
             title="Creative clusters in the UK")+
        scale_fill_manual(values=c("#8b9dc3","#ffc425",
                                           "#00b159","#f37735"))+
        theme(
                panel.grid=element_blank(),
                axis.ticks=element_blank(),
                axis.text=element_blank(),
                panel.background=element_rect(fill="white"))

WriteChart(clusterMap,outputdir)


#Now we want to produce a barchart summarising the economic structure of these
#places.
#We need to combine the ttwa.2010-13.subsectors and the ttwa.2013 all creative

#NB we are removing the "all creative category".
cluster.composition.temp <- subset(ttwa.2010.13.subsector,
                                   ttwa.name..2007. %in% myCreative.clusters
                                           & industry.short != "all.creative",
                                   select=c(1:7,17)) 
cluster.composition.temp <- droplevels(cluster.composition.temp)

#Get proportions
cluster.emp.comp <- GetClusterComposition(cluster.composition.temp,"employment")
cluster.bc.comp <- GetClusterComposition(cluster.composition.temp,"business.count")

#Df with all the cluster composition information
cluster.comp <- rbind(cluster.emp.comp,cluster.bc.comp)

#Reorder factors for plotting
#Determine order for sector (total employment) and clusters (Herfindahl)?
sector.emp.table <- sort(tapply(cluster.emp.comp$absolute,
                               cluster.emp.comp$sector,sum),decreasing=T)
sector.emp.ordered <- names(sector.emp.table)
cluster.comp$sector <- ReorderFactor(cluster.comp$sector,sector.emp.ordered,z=T)

#Herfindahl for clusters
sector.emp.herfindahl <- ldply(split(cluster.emp.comp,
                                     cluster.emp.comp$ttwa),
                               function(x) {
                                       return(
                                               sum((x$absolute/sum(x$absolute))^2))
                               })
ttwa.emp.ordered <- sector.emp.herfindahl[order(sector.emp.herfindahl$V1,
                                                decreasing=T),1]

cluster.comp$ttwa <- ReorderFactor(cluster.comp$ttwa,
                                   ttwa.emp.ordered,z=T)

#Palette
myPalette3 <- brewer.pal(length(levels(cluster.comp$sector)),"Accent")

#Plot cluster composition
cluster.comp.plot <- ggplot(data=cluster.comp,
                            aes(x=ttwa,y=prop,fill=sector,ordered=rev(sector)))+
        geom_bar(stat="identity")+
        facet_grid(variable~.)+
        scale_fill_manual(values=myPalette3)+
        labs(title="Creative cluster composition by sector",
             y="Proportion of activity in sub-sector",x=NULL)+
        theme(axis.text.x=element_text(angle=45,hjust=1,size=14),
              strip.text=element_text(size=14),
              legend.text=element_text(size=14),
              title=element_text(size=16))

WriteChart(cluster.comp.plot,outputdir,w=width,h=height)

#Produce a couple of bar charts summarising the performance of these locations
#Including:
        #Labour productivity
        #Average company size

#We are working with ttwa.2013.all.creative

#First, what % of all employment do myCreativeClusters represent
myClusters <- ttwa.2013.all.creative$ttwa.name..2007. %in% myCreative.clusters

prop.employment <- Percentify(sum(ttwa.2013.all.creative$employment[myClusters],na.rm=T)/
                                      sum(ttwa.2013.all.creative$employment,na.rm=T))
prop.business <- Percentify(sum(ttwa.2013.all.creative$business.count[myClusters],na.rm=T)/
                                      sum(ttwa.2013.all.creative$business.count,na.rm=T))
prop.turnover <- Percentify(sum(ttwa.2013.all.creative$turnover[myClusters],na.rm=T)/
                                    sum(ttwa.2013.all.creative$turnover,na.rm=T))

#Create measures of sales/worker and average company size in our cluster list
#First, subset
ttwa.2013.all.c.subset <- subset(ttwa.2013.all.creative,
                                 ttwa.name..2007. %in% myCreative.clusters,
                                 select=c("ttwa.name..2007.",
                                          "business.count","turnover",
                                          "employment"))
ttwa.2013.all.c.subset$sales.per.worker <- ttwa.2013.all.c.subset$turnover/
        ttwa.2013.all.c.subset$employment
ttwa.2013.all.c.subset$avg.firmsize <- ttwa.2013.all.c.subset$employment/
        ttwa.2013.all.c.subset$business.count

ttwa.allcreative.melt <- melt(ttwa.2013.all.c.subset,measure.vars=
                                      c("sales.per.worker","avg.firmsize"))

ttwa.prod.ordered <- ttwa.2013.all.c.subset[order(ttwa.2013.all.c.subset$sales.per.worker,decreasing=T),1]

ttwa.allcreative.melt$ttwa.name..2007. <- ReorderFactor(ttwa.allcreative.melt$ttwa.name..2007.,
                                                        ttwa.prod.ordered,z=T)

#Plot this.
myProdPlot <- ggplot(data=subset(ttwa.allcreative.melt,
                                 variable=="sales.per.worker"),
                     aes(x=ttwa.name..2007.,y=value))+
        geom_bar(stat="identity",fill="#00b159")+
        labs(x=NULL,y="Sales per employee, thGBP")+coord_flip()+
        theme(axis.text.y=element_text(size=14),
              panel.grid.major.y=element_line(color="black"))
myFirmSize <- ggplot(data=subset(ttwa.allcreative.melt,
                                 variable=="avg.firmsize"),
                     aes(x=ttwa.name..2007.,y=value))+
        geom_bar(stat="identity",fill="#00b159")+
        labs(y="Average workers per business",x=NULL)+
        theme(axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.grid.major.y=element_line(color="black"))+coord_flip()

WriteChart(multiplot(myProdPlot,myFirmSize,cols=2),outputdir,w=width,h=height)

#Finally, a time series comparing 2007 and 2013
#We use ttwa.all.cis
#We want to arrange it in wide form with employment per year in the columns.

#Split, apply, combine over turnover and employment.
ttwa.allcis.timeseries <- subset(ttwa.all.cis,
                                 year!=2010 &
                                         ttwa.name..2007. %in% myCreative.clusters,
                                 select=c("year",
                                         "ttwa.name..2007.",
                                          "business.count",
                                          "turnover",
                                          "employment"))
ttwa.allcis.timeseries <- droplevels(ttwa.allcis.timeseries)
#Create list of variables to loop over
finVars <- list("business.count","turnover","employment")
names(finVars) <- c("business.count","turnover","employment")

#Create time.series DF
ci.timeseries.df <- ldply(finVars,
              function(x) {
                       myDf <- ttwa.allcis.timeseries[,c("year","ttwa.name..2007.",x)]
                       myDf.wide <- dcast(myDf,ttwa.name..2007.~year,
                                          value.var=x)
                       names(myDf.wide) <- c("ttwa","y2007","y2013")
                       return(myDf.wide)
              })

#Normalise by first year
ci.timeseries.df[,paste("norm",c("2007","2013"),sep=".")] <-
        Percentify(ci.timeseries.df[,c("y2007","y2013")]/
                           ci.timeseries.df[,c("y2007")])
#Melt for plotting
ci.timeseries.melt.df <- melt(subset(ci.timeseries.df,
                                     select=c(1:2,5:6)),
                              id.vars=c(".id","ttwa"))
ci.timeseries.melt.df$year <- as.numeric(gsub("norm.","",
                                                  ci.timeseries.melt.df$variable))
ci.timeseries.melt.df$is.cluster <- factor("no", levels=c("no","yes"))
ci.timeseries.melt.df$is.cluster[ci.timeseries.melt.df$ttwa %in% myCreative.clusters] <-
        "yes"
#Tried to produce some labels (I didn't do much with them)
text.position <- subset(ci.timeseries.melt.df,
                        year==2013,
                        select=c(".id","ttwa","value"))

#Order ttwa names by their average performance in the three variables
#Should probably remove business counts given that it
        #seems to be negatively correlated with changes in employment and sales
ttwas.avg.perf <- sapply(split(ci.timeseries.melt.df,
                               ci.timeseries.melt.df$ttwa),
                         function(x) {
                                 x2 <- subset(x,year==2013)
                                 return(mean(x2$value,na.rm=T))
                         })

order.ttwas.by.perf <- names(sort(ttwas.avg.perf))

ci.timeseries.melt.df$ttwa <- ReorderFactor(ci.timeseries.melt.df$ttwa,
                                            order.ttwas.by.perf,z=T)

#Plot the timeseries (slopegraph?)
time.series.plot <- ggplot(data=ci.timeseries.melt.df,
                           aes(x=year,y=value,colour=.id))+
        geom_line(stat="identity",size=1.1)+
#         geom_text(data=text.position,aes(x=2013.1,y=value,
#                                          label=ttwa),hjust=0,size=3)+
        labs(title="Changes in employment,business counts and turnover per cluster,2007-2013 (BSD,2013)",
          x=NULL,y="2007=100")+
        scale_x_continuous(breaks=c(2007,2013))+
        facet_wrap(~ttwa,ncol=9,nrow=3)+
        theme(axis.text=element_text(size=14),
              axis.text.x=element_text(angle=45,hjust=1),
              legend.text=element_text(size=14),
              legend.position=c("top"),
              title=element_text(size=16),
              strip.text=element_text(size=14),
              panel.margin=unit(0.5,"lines"))
        
WriteChart(time.series.plot,outputdir,h=height,w=width)

#K-means clusters.
#Plan: allocate creative clusters into K groups in 2013 using:
        #Their sectoral distribution (businesses and employment)
        #Their diversity
        #Compare their performances

#Use ttwa.2010.13.subsector
cluster.composition.km <- subset(ttwa.2010.13.subsector,
                                   industry.short != "all.creative",
                                 #& ttwa.2010.13.subsector$ttwa.name..2007. %in%
                                         #myCreative.clusters,
                                   select=c(1:7,17)) 
cluster.composition.km <- droplevels(cluster.composition.km)



#Get dfs for kmeans
cluster.emp.k.df <- GetClusterCompForKMeans(cluster.composition.km,"employment")
cluster.bc.k.df <- GetClusterCompForKMeans(cluster.composition.km,"business.count")


#Df with all the cluster composition information (including location quotients)
cluster.k.df <- cbind(cluster.emp.k.df,cluster.bc.k.df[,-1],
                      ttwa.emp.lqs[,-1],ttwa.bc.lqs[,-1])

#I would like to add the average performance of each cluster in the
        #three variables from ttwa all cis
ttwa.allcis.time.k <- subset(ttwa.all.cis,
                                 year!=2010
#                              & ttwa.all.cis$ttwa.name..2007. %in%
#                                      myCreative.clusters
                             ,
                                 select=c("year",
                                          "ttwa.name..2007.",
                                          "business.count",
                                          "turnover",
                                          "employment"))
ttwa.allcis.time.k <- droplevels(ttwa.allcis.time.k)

#Create time.series DF
ci.timeseries.k <- lapply(finVars,
                          function(x) {
                                  myDf <- ttwa.allcis.time.k[,c("year","ttwa.name..2007.",x)]
                                  myDf.wide <- dcast(myDf,ttwa.name..2007.~year,
                                                     value.var=x)
                                  #Get the normalised performance metric
                                  myDf.wide$perf <- Percentify((myDf.wide[,3]/myDf.wide[,2])
                                                               -1)
                                  names(myDf.wide) <- c("ttwa","2007","2013","perf")
                                  return(myDf.wide)
                          })

#cbind
ci.allvars.k <- do.call(cbind,ci.timeseries.k)
#Do performance averages
perfVars <- names(ci.allvars.k)[grep("perf",names(ci.allvars.k))]

#One of them includes business counts, the other one doesn't
ci.allvars.k$perf.av.all <- rowMeans(ci.allvars.k[,perfVars],na.rm=T)
ci.allvars.k$perf.av.some <-rowMeans(ci.allvars.k[,perfVars[-1]],na.rm=T)
 

#DF for kmeans analysis
ci.k.df <- cbind(cluster.k.df,ci.allvars.k[,-(grep("ttwa",names(ci.allvars.k)))])

#Remove the performance and composition variables and missing values
kVars <- c("ttwa.emp",names(ci.k.df)[grep("lq|herf",names(ci.k.df))])

#Focus on all sectors or only my clusters?
ci.k.df.final <- subset(ci.k.df,
                        ttwa.emp %in% myCreative.clusters
                        )


ci.k.matrix1 <- ci.k.df.final[,kVars]
ci.k.matrix2 <- ci.k.matrix1[complete.cases(ci.k.matrix1),]
#Scale and take out the variable name
ci.k.matrix.final <- scale(ci.k.matrix2[,-1])


#How many clusters?
wss <- (nrow(ci.k.matrix.final)-1)*
        sum(apply(ci.k.matrix.final,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(ci.k.matrix.final, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#Find clusters
myClusters <- kmeans(ci.k.matrix.final,4)

kCluster.plot <- data.frame(kcluster=row.names(myClusters$centers),
                            myClusters$centers)
kCluster.plot.melt <- melt(kCluster.plot,id.var=1)

#Reorder factors using their alphabetical order.
sector.vars.ordered <- sort(levels(kCluster.plot.melt$variable))
sector.vars.ordered <- sector.vars.ordered[c(1:6,9:16,7:8)]

#Reorder
kCluster.plot.melt$variable <- ReorderFactor(kCluster.plot.melt$variable,
                                             sector.vars.ordered,z=T)

#Give the clusters more readable names
kCluster.plot.melt$kcluster <- factor(kCluster.plot.melt$kcluster,
                                      labels=c("hotspot","diversified",
                                               "IT","specialised"))

#Plot
#Set theme options
theme.options <- theme(title=element_text(size=16),
                       axis.text=element_text(size=14),
                       legend.title=element_text(size=16),
                       legend.text=element_text(size=14),
                       legend.position=c("top"))

#Plot
kmeans.plot <- ggplot(data=kCluster.plot.melt,aes(x=variable,y=value,
                                                  fill=kcluster,width=0.75))+
        geom_bar(stat="identity",position="dodge")+coord_flip()+
        scale_fill_manual(values=brewer.pal(4,"Set1"))+
        labs(y=NULL,x="Score in clustering variable",
             title="Scores in clustering variables")+
        theme.options
        
#Export
WriteChart(kmeans.plot,outputdir)

#Write-out cluster-names
kcluster.table <- data.frame(ttwa=ci.k.df.final$ttwa.emp,
                             kcluster=as.factor(myClusters$cluster))


kCluster.allocations <- lapply(as.list(as.character(c(1:4))),
                              function(x) {
                                      myrows <- kcluster.table$kcluster == x
                                      return(paste(kcluster.table$ttwa[myrows],
                                                   collapse=", "))
                              })

#Is there a correlation between concentration and average performance?

conc.perf <- ci.k.df[,c("ttwa.emp","herf.bc","perf.av.some")]
names(conc.perf) <- c("ttwa","conc","perf")
#Merge with cluster names and rename them
conc.perf2 <- merge(conc.perf,data.frame(ttwa=ci.k.df[,1],
                    kcluster=myClusters$cluster),
                    by.x="ttwa",by.y="ttwa")
conc.perf2$kcluster <- factor(conc.perf2$kcluster,
                              labels=c("hotspot","diversified",
                                       "IT","specialised"))

#Model to plot a regression line
cor(conc.perf$perf,conc.perf2$conc)

simple.model <- lm(perf~conc,data=conc.perf2)
coefs <- round(coef(simple.model),2)

#Create the plot
perf.con.scatter <- ggplot(data=conc.perf2,aes(x=conc,y=perf,
                                              color=kcluster))+geom_point()+
        geom_text(aes(x=conc,y=perf,label=ttwa),size=5,vjust=-1)+
        geom_abline(intercept=coefs[1],slope=coefs[2],colour="darkblue",size=2,
                    linetype=2)+
#         xlim(c(0.1,0.85))+
#         ylim(c(75,150))+
        annotate("text",x = 0.465,y=0,colour="darkblue",size=6,
                 label=paste("performance=",coefs[1],coefs[2],"*concentration"))+
        labs(x="Concentration in employment (Herfindahl)",
             y="Average cluster performance \n (2010-2013)",
             title="Cluster performance and concentration in employment\n(BSD,2013)")+
        theme(axis.text=element_text(size=14),
              title=element_text(size=18),
              legend.text=element_text(size=16))


WriteChart(perf.con.scatter,outputdir,w=width,h=height)

#Finally: co-location analysis
library(corrplot)
#Use ci.k.df
#GetCorrelationPlot function outputs a correlation plot between lqs.
#input: x: a variable (emp or bc)
#Returns the corrplot
GetCorrelationPlot <- function(x) {
        myVars <- grep(paste0(x,"lq"),names(ci.k.df))
        myDf <- ci.k.df[,myVars]
        
        if (x=="emp") {
                title <- "Creative co-location matrix: employment"
        } else {title <- "Creative co-location matrix: business counts"}
        
        M <- cor(myDf,use = "pairwise.complete.obs")
        corrplot(M, method="circle",order ="hclust",
                 tl.cex=1,sig.level="0.05",
                 #title=title,
                 tl.col="black",insig="blank",addrect=3)
        return(corrplot)
}
pdf("figures-outputs/colocation.pdf")
GetCorrelationPlot("emp")
GetCorrelationPlot("bc")
dev.off()

#Output data for CE.
ttwa.subsector.10_13.tbl <- tbl_df(ttwa.2010.13.subsector)
myClusters.tbl <- filter(ttwa.subsector.10_13.tbl,
                         ttwa.name..2007. %in% myCreative.clusters &
                                 industry != "All creative industries")
myClusters2.tbl <- select(myClusters.tbl,
                          industry,ttwa.name..2007.,location.quotient...business.count)
names(myClusters2.tbl) <- c("industry","ttwa","business.lq")

myClusters2.tbl.wide <- dcast(myClusters2.tbl,ttwa~industry,
                              value.var="business.lq")

myClusters2.tbl.wide$specialisations <- 
        apply(myClusters2.tbl.wide,1,function(x) {
                indices <- which(x[-1]>1) + 1
                sector.names <- names(x)[indices]
                return(paste(sector.names,collapse=", "))
        })

clusters.for.CE <- myClusters2.tbl.wide[,c("ttwa","specialisations")]


WriteOut(clusters.for.CE,"data-outputs/")

save.image("clusters-October2015.RData")
