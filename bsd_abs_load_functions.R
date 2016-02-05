#Read Data

#ReadManySheets function to extract data 
#from an excel worksheet
#Inputs:x: an excel file
#       y: the name (not index) of the sheet we want
#       z: start row
#       NB: here we are removing the first column (used for notes)
#Returns: the worksheet
ReadWorkSheet <- function(x,y,z) {
        myFile <- read.xlsx(x,sheetName = y,startRow = z)
        return(myFile)
}

#GetTopTTWAs function returns top x TTWAs by a set variable,
#in a selected year.
GetTopCreativeTTWAs <- function(number,df,variable,year) {
     myDf <- df[df$year==year,c("ttwa.name","year",variable)] %>% as.data.frame()
     myTTWAs <- myDf[order(myDf[,variable],decreasing=T),] 
     return(myTTWAs$ttwa.name[1:number])
}

#GetClusterBarchart is a function that returns a ggplot object ready for 
#formatting. Inputs: variable, year,vector with TTWA names.
GetClusterBarchart <- function(df=ttwa.all.creative.inds,variable,my.year,ttwas=myTTWAs) {
     df$year <- as.character(df$year)
     df.2 <- subset(df,
                    year== my.year & ttwa.name %in% myTTWAs,
                    select=c("ttwa.name",variable))
     df.2$ttwa.name <- ReorderFactor(df.2$ttwa.name,myTTWAs,z=T)
     
     myPlot <- ggplot(data=df.2,aes_string(x="ttwa.name",y=variable))+
          geom_bar(stat="identity",fill="steelblue")+
          labs(x=NULL)+coord_flip()+
          scale_y_continuous(labels=comma)+
          theme(axis.text=element_text(size=10),
                axis.title=element_text(size=10))
     return(myPlot)
}


#BindAreas function.
#input: a list of data-frames
#Returns them nicely r-bound (after )

BindAreas <- function(...) {
        objects <- list(...)
        standardNames <- names(objects[[1]])
        objects2 <- lapply(objects,function(x) {
                names(x) <- standardNames
                return(x)
        })
        myDf <- do.call("rbind",objects2)
        return(myDf)
}

#Cluster.tests function generates logical vectors as
#a result to creative clustering tests
#Input: x: a vector to test.
#       y: a percentile to consider.
#       z: standard deviations from the mean
#Returns a data.frame with the results of the test.
Clusters.test <- function(x,y,z) {
        
        #Is the lq > 1
        above1 <- x >1      
        #>2?
        above2 <- x >2
        
        #>mean
        above.mean <- x > mean(x,na.rm=T)
        #>median
        above.median <- x > median(x,na.rm=T)
        
        #top quartile
        top.pctile <- x >quantile(x,p=c(y),na.rm=T)
        #z sdv higher than the mean
        sd.above.mean <- CompareToMean(x,y=z)
        
        return(as.data.frame(cbind(above1,above2,
                                   above.mean,above.median,top.pctile,
                                   sd.above.mean)))
}

#CompareToMean function to compare the value of a 
#variable to the mean for the variable.
#Inputs: x: a variable
#        y: threshold number of deviations
#Returns: a logical vector with the result of the test.
CompareToMean <- function(x,y) {
        logv <- log(x)
        mean.v <- mean(logv,na.rm=T)
        sd.v <- sd(logv,na.rm=T)
        test <- logv > mean.v + y*sd.v
        return(test)
}

#Get.subsector.tests Checks whether the LQs for a TTWA
        #we are interested in are above the mean by a given number
        #of standard deviations. It also tells us whether the
        #number of businesses are above a minimum threshold.
#input: x: the dataframe
#       y: the variable we are interested in
#       z: threshold of
#        standard deviations above the mean in the variable
#       p: threshold (percentile) in business counts for inclusion
#Returns the data.frame with the tests

Get.subsector.tests <- function(x,y,z,p) {
        #Locate the location quotient
        varname <- paste0("location.quotient...",y)
        
        #Subset (remove "all creative industries" and "other)
        x2 <- subset(x,industry.short!="all.creative" &
                             industry.short != "other")
        x2 <- droplevels(x2)
        
        #Get variable we want to check
        myVars <- grep(varname,
                       names(x2))
        myDf <- data.frame(
                ttwa=x2$ttwa.name..2007.,
                industry=x2$industry.short)
        myDf2 <- cbind(myDf, x2[,names(x2)[myVars]])
        names(myDf2)[ncol(myDf2)] <- varname
        #Need to widen the df to do the checks
        myDf.wide <- dcast(myDf2,ttwa~industry,
                           value.var=names(x2)[myVars])
        tests <- as.data.frame(
                sapply(myDf.wide[,-1],CompareToMean,y=z))
        names(tests) <- paste(names(tests),"lq-test",sep="-")
        tests$lq.tests.passed <- rowSums(tests,na.rm=T)
        
        #Then, is the TTWA above the lowest pctile of
        #the variable for each sector?
        myDf.scale <- subset(x2,select=c("industry.short",
                                      "ttwa.name..2007.",
                                      y))
        myDf.scale.wide <- dcast(myDf.scale,ttwa.name..2007.~
                                       industry.short,
                               value.var=y)
        myDf.scale.tests <- as.data.frame(
                sapply(myDf.scale.wide[,-1],function(x) {
                        test <- x > quantile(x,p=c(p),na.rm=T)
                        test
                }))
        
        names(myDf.scale.wide) <- paste(names(myDf.scale.wide),
                                        y,sep="-")
        names(myDf.scale.tests) <- paste(names(myDf.scale.tests),
                                               "scale-test",sep="-")
        #Bind all tests
        myDf.subsector.tests <- cbind(myDf.wide,tests,
                                      myDf.scale.wide,
                                      myDf.scale.tests)
        
        #Create new data-frame with joint evaluation of tests.
        sector.names <- levels(x2$industry.short)
        myDf.joint.evaluation <- 
                as.data.frame(sapply(sector.names,
                       function(x) {
                               pairs <- myDf.subsector.tests[,
                                       grep(x,names(myDf.subsector.tests))]
                               pass <- pairs[,2]==TRUE &
                                       pairs[,4] == TRUE
                               return(pass)
                       }))
        names(myDf.joint.evaluation) <- paste(names(myDf.joint.evaluation),
                                               "joint-test",sep="-")
        myDf.joint.evaluation$joint.tests.passed <- rowSums(myDf.joint.evaluation)
        myDf.out <- data.frame(myDf.subsector.tests,
                               myDf.joint.evaluation)
        return(myDf.out)
}

#GetCreativeClusters function: A function with the inputs above,
        #writes out a csv with all the data for the
        #selected clusters as well as a vector with
        #the names of all the clusters.
#Arguents: x: the dataframe
#       y: the variable we are interested in
#       z: threshold of
#        standard deviations above the mean in the variable
#       p: threshold (percentile) in business counts for inclusion
#writes out a dataframe and returns a list of names

GetCreativeClusters <- function(x,y,z,p) {
        outputDf <- Get.subsector.tests(x,y,z,p)
        
        cluster.index <- outputDf$joint.tests.passed>0 &
                                             !is.na(outputDf$joint.tests.passed)
        cluster.stats <- outputDf[cluster.index,]
        
        #Create field with names of sectors with high specialisation
        cluster.lqs <- cluster.stats[,grep("lq.test$",names(cluster.stats))]
        names(cluster.lqs) <- gsub(".lq.test","",names(cluster.lqs))
        
        cluster.stats$specialisations <- unlist(apply(cluster.lqs,1,
                                 function(x) {
                                         sp <- names(x)[which(x==TRUE)]
                                         return(paste0(sp,collapse=", "))}))
        
       #Get cluster names
        cluster.names <- outputDf$ttwa[cluster.index]
        results <- list(as.character(cluster.names),
                          cluster.stats,
                          outputDf)
        names(results) <- c("cluster.names","cluster.stats","all.places")
        return(results)
}

#ClusterRobustness function to check the robustness of the
        #cluster analysis.
#Returns a df for plotting robustness.
ClusterRobustness <- function() {
        #Create sequence of parameters
        sd.seq <- seq(0,2.5,0.1)
        scale.seq <- c(0.5,0.75,0.9)
        
        #DF with combinations of parameters
        combs <- expand.grid(sd.seq,scale.seq)
        
        #Run a loop to count number of clusters identified with
                #different combinations of parameters
        
        cluster.sel <- apply(combs,1, function(x) {
                sd <- as.numeric(x[1])
                prop <- as.numeric(x[2])
                
                #Extract the clusters
                emp <- GetCreativeClusters(ttwa.2010.13.subsector,
                                            "employment",
                                            z=sd,
                                            p=prop)
                bc <- GetCreativeClusters(ttwa.2010.13.subsector,
                                          "business.count",
                                          z=sd,
                                          p=prop)
                #Count number of employment and business clusters
                emp.clusters <- length(emp$cluster.names)
                bc.clusters <- length(bc$cluster.names)
                
                chosen.clusters <- unique(c(emp$cluster.names,
                                                   bc$cluster.names))
                chosen.clusters.pasted <- paste(chosen.clusters,
                                                collapse=", ")
                #Combinations of clusters
                both.clusters <- length(chosen.clusters)
                
                #What are the new clusters?
                
                new.clusters <- chosen.clusters[
                        !(chosen.clusters %in% creative.clusters)]
                new.clusters.pasted <- paste(new.clusters,
                                             collapse=", ")
                
                
                return(c(emp.clusters,
                         bc.clusters,
                         both.clusters,
                         chosen.clusters.pasted,
                         new.clusters.pasted))
        })
        #Get it out
        output <- data.frame(combs,t(cluster.sel))
        #Rename variables
        names(output) <- c("sd","prop","emp.clusters",
                           "bc.clusters","both",
                        "all.clusters","new.clusters")
        #Tidy up variable classes
        output$prop <- as.factor(output$prop)
        output[,c(3:5)] <- lapply(output[,c(3:5)],function(x){
                as.numeric(as.character(x))
        })
        
        return(output)
         
}

#GetClusterNames function returns cluster names for 
#different parametres (sd and prop)
#Inputs:sd: standard deviations from the mean used as LQ threshold
#       prop: quantile used to set a scale threshold
#Returns the names of clusters selected with those parameters.

GetClusterNames <- function(sd,prop) {
        names <- cluster.robust.df$all.clusters[cluster.robust.df$sd==sd
                                                &cluster.robust.df$prop==prop]
        number <- cluster.robust.df$both[cluster.robust.df$sd==sd
                                         &cluster.robust.df$prop==prop]
        return(list(number,sort(as.character(names))))
}

#GetClusterComposition function extracts a dataframe with
#sectoral composition of a cluster for each location. 
#Input: x: the dataframe
#       y: the variable we are interested in (employment or business count)
#Returns a data.frame where one of the variables is area, another sector
#and another proportion.

GetClusterComposition <- function(x,y) {
        myDf <- ldply(split(x,
                            x$ttwa.name..2007.),
                      function(x){
                              myT <- x[,c("industry.short",y)]
                              myT$prop <- Percentify(myT[,2]/
                                                             sum(myT[,2],na.rm=T))
                              return(myT)
                      })
        myDf2 <- data.frame(rep(y,nrow(myDf)),myDf)
        names(myDf2) <- c("variable","ttwa","sector","absolute","prop")
        return(myDf2)
}

#GetClusterCompForKmeans is the same function as above
        #but returning a data-frame for k-means analysis
        #In wide form and with a column with the Herfindahl index
GetClusterCompForKMeans <- function(x,y) {
        myDf <- ldply(split(x,
                            x$ttwa.name..2007.),
                      function(x){
                              myT <- x[,c("industry.short",y)]
                              myT$prop <- myT[,2]/
                                                             sum(myT[,2],na.rm=T)
                              return(myT)
                      })
        myDf2 <- data.frame(rep(y,nrow(myDf)),myDf)
        names(myDf2) <- c("variable","ttwa","sector","absolute","prop")
        
        myDf2.wide <- dcast(myDf2,ttwa~sector,
                                     value.var="prop")
        
        myDf2.wide$herf <- rowSums((myDf2.wide[,2:ncol(myDf2.wide)])^2,
                                   na.rm=T)
        #Name the variables 
        if (y=="employment") {
                names(myDf2.wide) <- paste(names(myDf2.wide),"emp",sep=".")
        } else {names(myDf2.wide) <- paste(names(myDf2.wide),"bc",sep=".")
        }
        return(myDf2.wide)
}

#GetColocation matrix takes a df, a year and a measure, and returns
#the colocation (correlation) matrix
GetColocation.matrix <- function(df=subsector.ttwa,my.year,var) {
     #Get the df to work with
     myDf <- tbl_df(df) %>% filter(year==my.year,
                                   industry.short !="all.creative") %>% 
          select_("ttwa.name","industry.short",var)
     #Create Df
     myDf.wide <- dcast(myDf,ttwa.name~industry.short,value.var=var)
     
     #Correlate
     cor.matrix <- cor(as.matrix(myDf.wide[,-1]),use="complete.obs")
     return(as.data.frame(cor.matrix))
}


#GetLevelPlot function
#Takes a correlation matrix and a title and returns a levelplot
GetLevePlot <- function(cormat, my.title) {
     mydf <- data.frame(sector=row.names(cormat),cormat)
     mydf2 <- melt(mydf,id.var="sector")
     
     orderedlevels <- rev(sort(as.character(levels(mydf2$sector))))
     
     mydf2$sector <- ReorderFactor(mydf2$sector,orderedlevels,z=T)
     
     mydf2$variable <- as.factor(mydf2$variable)
     mydf2$variable <- ReorderFactor(mydf2$variable,
                                     orderedlevels,z=F)
     
     plot <- ggplot(data=mydf2,aes(x=sector,y=variable,fill=value))+
          geom_tile()+
          scale_fill_gradient2(low="lightblue",high="coral2")+
          labs(title=my.title,x=NULL,y=NULL,fill="value")+
          theme(axis.text.x=element_text(angle=45,hjust=1),
                axis.text=element_text(size=14))
     return(plot)
}


GetLevePlot2 <- function(cormat, my.title) {
          mydf <- cormat
#      orderedlevels <- rev(sort(as.character(levels(mydf$sector))))
#      
#      mydf$sector <- ReorderFactor(mydf$sector,orderedlevels,z=T)
#      
#      mydf$variable <- as.factor(mydf$variable)
#      mydf$variable <- ReorderFactor(mydf$variable,
#                                      orderedlevels,z=F)
     
     plot <- ggplot(data=mydf,aes(x=sector,y=variable,fill=value))+
          geom_tile()+
          scale_fill_gradient2(low="lightblue",high="coral2")+
          labs(title=my.title,x=NULL,y=NULL,fill="value")+
          theme(axis.text.x=element_text(angle=45,hjust=1),
                axis.text=element_text(size=14))
     return(plot)
}




#GetSectorSpecialisationPlot function takes a year and an industry and
#retuns a faceted plot with all the specialisations by industry
GetSectorSpecialisationPlot <- function(df=subsector.ttwa_selected3, 
                                        my.year,my.industry) {
     myDf <- df %>% filter(industry.short== my.industry & year==my.year) %>%
          mutate(average.firm.size.norm=average.firm.size/average.firm.size.uk,
                 sales.per.worker.norm=sales.per.worker/sales.per.worker.uk) %>%
          select(ttwa.name,
                 business.lq, employment.lq,
                 average.firm.size.norm,sales.per.worker.norm,cluster) %>%
          melt(id.vars=c("ttwa.name","cluster"))
     
     myDf$ttwa.name <- ReorderFactor(myDf$ttwa.name,myTTWAs,z=T)
     
     sectorPlot <- ggplot(data=myDf,aes(x=ttwa.name,y=value-1,fill=cluster))+
          geom_bar(stat="identity")+facet_grid(.~variable)+
          coord_flip()+
          labs(x=NULL,y="Score vs. UK average in sector",
               title=paste("Activity in",my.industry,"by TTWA","(",my.year,")",sep=" "))
     
     return(sectorPlot)
}

#GetSectorChangePlot function that takes a sector and calculates changes in
     #activity between 2007-2009 and 2010-2013.
GetSectorChange_df <- function(df=subsector.ttwa_selected3,my.industry) {
     myDf <- df %>% filter(industry.short== my.industry) %>%
          select(ttwa.name,year,business.count,employment,turnover,
                 average.firm.size,sales.per.worker,cluster) %>%
          melt(id.vars=c("ttwa.name","year","cluster")) %>% droplevels()
     
     myDf$ttwa.name <- ReorderFactor(myDf$ttwa.name,myTTWAs,z=T)
     
     var.changes <- ldply(split(myDf,list(myDf$ttwa.name,myDf$variable)),
                          function(x) {
                               change <- x[2,]
                               change$year <- "Change"
                               change$value <- x[2,5]/x[1,5] - 1
                               return(change)
                          },.id=NULL)
     return(var.changes)
}


#Use function that takes a dataframe and two variables and returns
#a shaded barchart
GetShadedBarchart <- function(df=meetup.sector.ttwa_selected,
                              length.var,shade.var) {
     df$ttwa.name <- ReorderFactor(df$ttwa.name,top.ttwas_business,z=T)
     bar <- ggplot(data=df,aes_string(x="ttwa.name",y=length.var,fill=shade.var))+
          geom_bar(stat="identity")+
          scale_fill_gradient(low="lightblue",high="darkblue")+
          scale_y_continuous(labels=comma)+
          coord_flip()+
          labs(x=NULL)+
          theme(axis.text=element_text(size=7),
                axis.title=element_text(size=9))
     return(bar)
}


#GetShadedBarchart General Use function that takes a dataframe and two variables and returns
#a shaded barchart
GetShadedBarchart_general <- function(df,length.var,shade.var,cat.var,grid.var=NULL) {
     bar <- ggplot(data=df,aes_string(x=cat.var,y=length.var,fill=shade.var))+
          geom_bar(stat="identity")+
          scale_fill_gradient(low="lightblue",high="darkblue")+
          scale_y_continuous(labels=comma)+
          coord_flip()+
          labs(x=NULL)+
          theme(axis.text=element_text(size=7),
                axis.title=element_text(size=9))
     if (grid.var!=NULL) {
          bar <- bar+facet_grid(.~grid.var)
     }
     return(bar)
}



