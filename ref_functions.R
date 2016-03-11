#Script to scrape data about REF outcomes for UK universities

#Packages
library(httr)
library(stringr)
library(XML)
library(RCurl)


#Functions:
#insertDelay utility function.
#Arguments: the top length to delay for
#returns a delay.
insertDelay <- function(x) {
        Sys.sleep(sample(seq(0,x,0.1),1))
}

#GetParseHtml function
#Arguments: a URL and the delay to introduce
#Returns a parsed html, after a delay
GetParseHtml <- function(x,y=1) {
        insertDelay(y)
        return(htmlParse(getURL(x)))
}

#GetREFdata function to download and parse data.
#Input: a unit of assessment (can use the index for the subject or a keyword)
#returns a table with all results for that subject
GetREFData <- function(x) {
        if (is.numeric(x)==TRUE) {
                url <- paste0("http://results.ref.ac.uk/Results/ByUoa/",x)
        } else {
                matches <- grepl(x,uoaTable[,2],ignore.case=T)
                index <- which(matches,TRUE)
                if (length(index)>1) {
                        stop("You are trying to get data for more than one subject")
                }
                url <- paste0("http://results.ref.ac.uk/Results/ByUoa/",index)
        }
        #We want to scrape data from these 4 sources
        urlsToGet <- paste0(url,c("","/outputs","/impact","/environment"))
        
        #Get and parse the data        
        parsedUrls <- lapply(urlsToGet,GetParseHtml)
        
        #Extract data from each parsed element in the list
        #We use a loop because we need to replace the name 
        #of the variable we are interested in in each
        #of the elements
        dfContainer <- data.frame()
        refFieldNames <- c("overall","outputs","impact","environment")
        
        for (i in 1:length(parsedUrls)) {
                myData <- ExtractREFData(parsedUrls[[i]],
                                         name=refFieldNames[i])
                dfContainer <- rbind(dfContainer,myData)
        }     
        dfContainer[,c(2,4:8)] <- sapply(dfContainer[,c(2,4:8)],
                                         as.numeric)
        return(dfContainer)
}

#ExtractREFData function to extract REF data from the parsed code
#Arguments: a scraped, parsed page with REF code for a subject,
#and the name of the measure we are interested in
#Returns: a table with results


ExtractREFData <- function(x,name) {        
        university <- xpathSApply(x,"//div[@class='left']//h2",xmlValue)
        staff <- xpathSApply(x,"//div[@class='staff']"
                             ,xmlValue)
        staff.cleaned <- ldply(staff,function(x){
                str_trim(unlist(str_split(x,":|\r"))[3])
        })
        #NB we insert the right variable we are interested in.
        scores <- xpathSApply(x,
                              paste0("//table[@class='",
                                     name,
                                     "']"),
                              xmlValue)
        scores.cleaned <- ldply(scores,function(x) {
                str_trim(unlist(strsplit(x, "\r")))[1:6]
        })
        results <- data.frame(university,
                              staff.cleaned,
                              scores.cleaned)
        names(results) <- c("university","staff",
                            "measure",
                            "star4","star3","star2","star1","uc")
        return(results)
}

#Get data

#Units of assessment = there are 36 of them.
#Create a dictionary of them using the reference page here:
        #http://results.ref.ac.uk/Results/SelectUoa

uoaNumbers <- GetParseHtml("http://results.ref.ac.uk/Results/SelectUoa")
#Extract the list
uoaSubjects <- xpathSApply(uoaNumbers,
                         "//div[@id='content']//li/a",xmlValue)
#Create a table
uoaTable <- ldply(uoaSubjects,
                  function(x) {
                          myData <- unlist(strsplit(x,"-"))
                          return(data.frame(code=myData[1],
                                            subject = myData[2]))
                          }
                  )

# #Extract the data
# computerRefs <- GetREFData("computer science")
# econRefs <- GetREFData("economics")
# head(econRefs)
