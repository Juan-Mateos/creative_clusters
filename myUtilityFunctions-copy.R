######
#PREPARATION
######

#CreateDirs Utility function to create directories to store data
#Input: the name of the folder and the working directory
#Returns: creates the folder

CreateDirs <- function(x,y) {
        if(!file.exists(paste0(x,y)))
        {dir.create(paste0(x,y))}
}

######
#MANAGING THE WORKSPACE
######

#CleanWorkspace function removes objects from the workspace
#Input: a string or regex
#Returns: Nothing. Removes the objects from the workspace
#Process: uses the rm command and ls()
CleanWorkspace <- function(x) {
        rm(list=ls()[grep(x,ls())])
}



######
#CLEANING DATA
#####

#AddLevels function adds new levels to those a variable already has.
#Inputs:x: a variable
#       y: a vector with the levels to add
#       adds the levels to the variable
AddLevels <- function(x,y) {
        levels(x) <- c(levels(x),y)
        return(x)
}


#Percentify function to tidy up results from prop.table
#Inputs. x: a number
#Returns a rounded %
Percentify <- function(x) {
        return(round(x*100,2))
}


#ReorderFactor Function reorders a factor using the
#vector of levels provided
#Inputs:x: a factor
#       y: a vector of ordered levels
#       z: whether to reverse levels or not for plotting
#Returns the ordered factor

ReorderFactor <- function(x,y,z) {
        if (z == F) {
                x2 <- factor(x,ordered=T,levels=y)
        } else {x2 <- factor(x,ordered=T,levels=rev(y))
        }
        return(x2)
}

#Function to str_split and get a resulting vector
SplitGet <- function(my.string, my.pattern,my.index) {
     str <- unlist(str_split(my.string,my.pattern))
     return(str[[my.index]])
}     


#AddLevels function adds new levels to those a variable already has.
#Inputs:x: a variable
#       y: a vector with the levels to add
#       adds the levels to the variable
AddLevels <- function(x,y) {
        levels(x) <- c(levels(x),y)
        return(x)
}

#Reading data
#ReadSubset function takes a file name and an index of variables to read them
ReadSubset <- function(file,variables) {
     test.read <- read.delim(file,nrow=1)
     coll.v <- rep("NULL",ncol(test.read))
     coll.v[variables] <- NA
     real.read <- read.delim(file
                             ,colClasses = coll.v,
                             na.strings=c("-999","NA"),quote="")
     
     names(real.read) <- tolower(names(real.read))
     return(real.read)
}


#ReadSubset general function takes a file name and an index of variables to read them
ReadSubset_general <- function(file,variables) {
     test.read <- read.csv(file,nrow=1)
     coll.v <- rep("NULL",ncol(test.read))
     coll.v[variables] <- NA
     real.read <- read.csv(file
                           ,colClasses = coll.v)
     
     names(real.read) <- tolower(names(real.read))
     return(real.read)
}

######
#MEASURING ETC
######

#LQ function produces an lq from two variables
#Input: x and y: 2 variables.
#Returns their LQ
LQ <- function(x,y) {
        return((x/sum(x,na.rm=T))/(y/sum(y,na.rm=T)))
}

#GetSigTable function produces 2-sample t-test comparing a 
        #each group in a factor vs. the other.
#Inputs: x: a continuous variable name
#       y: a factor name.

GetSigTable <- function(df,x,y) {
        myDf <- as.data.frame((df))
        var <- myDf[,grepl(x,names(myDf))]
        fact <- myDf[,y]
        test <- sapply(levels(fact),function(level) {
                tst <- t.test(var~fact==level)
                est <- tst$estimate[2] - tst$estimate[1]
                sig <- tst$p.value
                if (est>0 & sig<0.05) {
                        return("positive")
                } else if (est<0 & sig<0.05) {
                        return("negative")
                } else {return("insignificant")
                }
                })
        output <- t(data.frame(test))
        row.names(output) <- x
        return(output)
        }

#Factor analysis function:
#Inputs: x name of a variable
#       y: a dataframe
#       returns the factor analysis of the variable

Factoranalyse <- function(x,df) {
        varindex <- grep(x, names(df))
        cor <- cor(df[,varindex], use="pairwise.complete.obs")
        scree(cor)
        eig <- eigen(cor)
        fact <- sum(eig$values>0.9)
        fit <- factanal(df[,varindex],factors=fact, rotation="varimax",
                        scores="regression")
        fit
}

#HighScore function returns TRUE if a score is above a threshold 
#(e.g. top quartile etc)
HighScore <- function(x,prob) {
     q <- quantile(x,probs = prob)
     y <- x>=q
     return(y)
}     


#GetHerfindahl function takes a distribution and returns its
#Herfindahl index.
GetHerfindahl <- function(vector) {
     h <- sum((vector/sum(vector,na.rm=T))^2,na.rm=T)
     return(h)
}


######
#GETTING WEB DATA
######

#GetUnzip function to download and unzip data
#input: x: a url
#       y: a destination folder
#downloads and unzips the file
GetUnzip <- function(x,y) {
        filename.split <- unlist(str_split(x,"/"))
        filename <- filename.split[length(filename.split)]
        dirname <- gsub(".zip","",filename)
        name <- paste(y,filename,sep="/")
        download.file(x,name,method="curl")
        unz <- unzip(name,exdir=paste(y,dirname,sep="/"))
        unz
}


#GetParseData function to get and parse JSON data from APIs
#Argument: a url and a delay
#Returns the parsed JSON object
GetParseData <- function(x,y=1) {
        insertDelay(y)
        return(fromJSON(getURL(x)))
}

#GetParseHtml function
#Arguments: a URL and the delay to introduce
#Returns a parsed html, after a delay
GetParseHtml <- function(x,y=1) {
        return(htmlParse(getURL(x)))
}

#insertDelay utility function.
#Arguments: the top length to delay for
#returns a delay.
InsertDelay <- function(x) {
        Sys.sleep(sample(seq(0,x,0.1),1))
}


#Utility function to check for nulls in JSON.
#Arguments: a JSON field
#Returns the value if it isn't NULL, NA otherwise.

CheckNulls <- function(x) {
        if(is.null(x)==TRUE) {
                return(NA)
        } else {
                return(x)
        }
}

#####
#Plotting
#####

#CreateMappingDf function outputs a ttwa df ready for plotting.
CreateTTWAmapping_df <- function(){
     #Read the Shapefiles
     uk <- readOGR("/Users/juanmateos-garcia/Desktop/2015 core/Technation/Analysis/Datasets/Travel_to_work_areas_(UK)_2001_Boundaries_(Full_Extent)/",
                   layer="TTWA_2001_UK_BFE")
     #Crete dfs.
     uk@data$id <- row.names(uk@data)
     uk.points <- fortify(uk,region="id")
     uk.ttwa_df <- join(uk.points,uk@data,by="id")
     return(uk.ttwa_df)
}

#CreateMappingDf function outputs a ttwa df ready for plotting.
CreateUK_df <- function(){
     #Read the Shapefiles
     uk <- readOGR("/Users/juanmateos-garcia/Desktop/2015 core/Technation/Analysis/Datasets/StatPlanet_UK/map/map.shp",
                   layer="map")
     uk@data$id <- row.names(uk@data)
     
     slot(uk, "polygons") <- lapply(slot(uk, "polygons"), checkPolygonsHoles)
     uk1 <- unionSpatialPolygons(uk, as.character(uk@data$MM_UID))
     
     ukbounds <- fortify(uk1,region="DIVISION")
     #Remove an extreme westward observation.
     west <- ukbounds$long < -10
     ukbounds <- ukbounds[!west,]

     return(ukbounds)
}


#GetBarchart returns a ggplot barchart. It
#Takes an x, a height, a facet and a fill.
GetBarchart <- function(my.df,xvar,height,
                        fill.var="NULL",facet.var="NULL",
                        my.title,
                        my.width=0.5) {
     
     b <- ggplot(data=my.df,
                 aes_string(x=xvar,
                            y=height))
     if (fill.var=="NULL") {
          b <- b+geom_bar(stat="identity",fill="steelblue",width=my.width
          )+coord_flip()
     } else if (fill.var!="NULL") {
          b <- b+geom_bar(stat="identity",
                          aes_string(fill=fill.var),position="dodge",width=my.width
                          )+coord_flip()
     }
     if (facet.var!="NULL") {
          b <- b+facet_grid(reformulate(facet.var))
     }
     b2 <- b+labs(x=NULL,title=my.title)
     return(b2)
}


#GetBarchart w Alpha returns a ggplot barchart. It
#Takes an x, a height, a facet and a fill.
GetBarchart_alpha <- function(my.df,xvar,height,
                        fill.var="NULL",facet.var="NULL",
                        my.title,
                        my.color=NULL,
                        my.alpha=1,
                        my.width=0.5) {
     
     b <- ggplot(data=my.df,
                 aes_string(x=xvar,
                            y=height))
     if (fill.var=="NULL") {
          b <- b+geom_bar(stat="identity",fill="steelblue",width=my.width
          )+coord_flip()
     } else if (fill.var!="NULL") {
          b <- b+geom_bar(stat="identity",
                          aes_string(fill=fill.var,
                                     alpha=my.alpha,
                                     color=my.color),position="dodge",width=my.width
          )+coord_flip()
     }
     if (facet.var!="NULL") {
          b <- b+facet_grid(reformulate(facet.var))
     }
     b2 <- b+labs(x=NULL,title=my.title)
     return(b2)
}



# Multiple plot function (from:http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/)
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}

#####
#Exporting
####

#WriteOut Function to write out files including the date

#Args:  x: file name
#       y: output dir
#Output: writes out the file
WriteOut <- function(x,y,rnames=F) {
        name <- deparse(substitute(x))
        today <- paste0(y,name,"-",Sys.Date(),".csv")
        write.csv(x,today,row.names=rnames)
}


#WriteChart to output charts and plots.
#Args:  x: a plot object, 
#       y: output dir, 
#       w: width
#       h: height
#Saves the plot in the charts folder
WriteChart <- function(x,y,w,h,...) {
        name <- deparse(substitute(x))
        pdf(paste0(y,name,Sys.Date(),".pdf"),width=w,height=h)
        print(x)
        dev.off()
}

#Some aesthetic stuff: map theme

map_theme <- theme(
     panel.grid=element_blank(),
     axis.ticks=element_blank(),
     axis.text=element_blank(),
     axis.title=element_blank(),
     panel.background=element_rect(fill="white"))

