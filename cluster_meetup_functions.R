#Clusters meetup functions

#GetTmData function to extract relevant information for topic modelling, viz:
#Date of most recent event
#Topic field
#Metadata (id and name)
#Params: a tech Group (parsed json)
#Output: a df with the fields above

GetTmData <- function(x){
     #Get meetup ID and name
     id <- x$id
     name <- x$name
     
     #Geo variables
     lon <- CheckNulls(x$lon)
     lat <- CheckNulls(x$lat)
     city <- x$city
     
     #Description
     desc <- CheckNulls(x$description)
     members <- CheckNulls(x$members)
     
     #Date of creation
     created <- as.POSIXct(as.numeric(x$created)/1000,origin="1970-01-01")
     
     #Get All event dates (as a vector)
     all_dates <- ldply(x$events_in_window,
                        function(y){
                             time <- y$time
                             return(time)
                        })
     
     if (length(all_dates)>0) {
          all_dates_sorted <- sort(all_dates[,1],decreasing = TRUE)
          latest_date_int <- as.numeric(all_dates_sorted[1])
          latest_event <- as.POSIXct(latest_date_int/1000,origin="1970-01-01")
     } else {latest_event <- NA}
     #Get all topics.
     topics <- ldply(x$topics,
                     function(x){
                          return(x$urlkey)
                     })
     
     topics_collapsed <- paste(topics$V1,collapse=" ")
     
     my_out <- data.frame(id=id,name=name,
                          city=city,
                          lon=lon,
                          lat=lat,
                          members=members,
                          created=created,
                          latest_date=latest_event,
                          topics = topics_collapsed,
                          description=desc)
     return(my_out)
}

#GetUserData function to get data about meetup users.
#Params:
     #An element in the tech user list (parsed from json)
#Outputs:
     #A df with the following variables:
          #ids, country, city, lat and lon, topics, joined, visited


GetUserData <- function(x){
     #Get user ID and name
     id <- CheckNulls(x$id)
     name <- CheckNulls(x$name)
     
     #Geo variables
     lon <- CheckNulls(x$lon)
     lat <- CheckNulls(x$lat)
     city <- CheckNulls(x$city)
     country <- CheckNulls(x$country)
     
     #Joining date and last visit
     joined <- CheckNulls(
          as.POSIXct(as.numeric(CheckNulls(x$joined))/1000,origin="1970-01-01"))
     visited <- CheckNulls(
          as.POSIXct(as.numeric(CheckNulls(x$visited))/1000,origin="1970-01-01"))
     
     #Get all topics.
     topics <- ldply(x$topics,
                     function(x){
                          return(x$urlkey)
                     })
     
     topics_collapsed1 <- paste(topics$V1,collapse=" ")
     topics_collapsed2 <- CheckNulls(topics_collapsed1)
     
     my_out <- data.frame(id=id,
                          name=name,
                          city=city,
                          country=country,
                          lon=lon,
                          lat=lat,
                          joined=joined,
                          visited=visited,
                          topics = topics_collapsed2)
     return(my_out)
}



