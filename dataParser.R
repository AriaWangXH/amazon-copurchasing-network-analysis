library(dplyr)

# Load data
setwd("~/Downloads")
rawDat <- read.delim("amazon-meta.txt")
cleanEdgeDat <- data.frame(objectId = numeric(), # Edge (review) data
                       customerId = character(),
                       date = character(),
                       rating = numeric(),
                       votes = numeric(),
                       helpful = numeric()
                       )
cleanNodeDat <- data.frame(objectId = numeric(), # Node (object/product) data
                             title = character(),
                             group = character(),
                             salesrank = numeric(),
                             categories = numeric())

# Include desired years
yearList <- c("2000","2001","2002","2003","2004")

# Object level variables
currID <- NA
currTitle <- NA
currGroup <- NA
currSalesRank <- NA
currCategories <- NA

# Review (edge) level variables
currDate <- ""
currCust <- ""
currRating <- 0
currVotes <- 0
currHelpful <- 0

# Loop for parsing data
for (i in 1:100000) {
  # Get new row
  currRow <- rawDat[i,]
  if (nchar(currRow) < 500) { # Filter out error rows
    # Update review ID
    if (substr(currRow,1,3) == "Id:"){
      # Get new review id
      currID <- as.numeric(substr(currRow,4,nchar(currRow)))
      print(currID)
    }
    # Update title
    if (substr(currRow,3,8) == "title:"){
      # Get new title
      if (nchar(currRow) >= 10) {currTitle <- substr(currRow,10,nchar(currRow))}
      if (nchar(currRow) < 10) {currTitle <- NA}
    }
    # Update group
    if (substr(currRow,3,8) == "group:"){
      # Get new title
      if (nchar(currRow) >= 10) {currGroup <- substr(currRow,10,nchar(currRow))}
      if (nchar(currRow) < 10) {currGroup <- NA}
    }
    # Update salesrank
    if (substr(currRow,3,12) == "salesrank:"){
      # Get new title
      if (nchar(currRow) >= 14) {currSalesRank <- as.numeric(substr(currRow,14,nchar(currRow)))}
      if (nchar(currRow) < 14) {currSalesRank <- NA}
    }
    # Update categories and, if applicable, store object data
    if (substr(currRow,3,13) == "categories:"){
      # Get new title
      if (nchar(currRow) >= 15) {currCategories <- as.numeric(substr(currRow,15,nchar(currRow)))}
      if (nchar(currRow) < 15) {currCategories <- NA}
      # Now, store the object/product (node) data
      cleanNodeDat <- cleanNodeDat %>% add_row(objectId = currID,
                                               title = currTitle,
                                               group = currGroup,
                                               salesrank = currSalesRank,
                                               categories = currCategories)
    }
    # Look for a review row
    if (substr(currRow,5,8) %in% yearList){
      # Store review data
      currRow <- strsplit(currRow,split=" {1,}")[[1]]
      currDate <- currRow[2]
      currCust <- currRow[4]
      currRating <- as.numeric(currRow[6])
      currVotes <- as.numeric(currRow[8])
      currHelpful <- as.numeric(currRow[10])
      
      cleanEdgeDat <- cleanEdgeDat %>% add_row(objectId = currID,
                                               customerId = currCust,
                                               date = currDate,
                                               rating = currRating,
                                               votes = currVotes,
                                               helpful = currHelpful)
    }
  }
} 

write.csv(cleanNodeDat,"cleanNodeDat.csv")
write.csv(cleanEdgeDat,"cleanEdgeDat.csv")
