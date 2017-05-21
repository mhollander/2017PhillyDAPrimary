
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(stringr)
library(reshape2)
library(data.table)
library(leaflet)
library(rmapshaper)
library(rgdal)
library(dplyr)

# read the CSV and get rid of the strange last column
votes <- read.csv("PRECINCT_2017517_H20_M51_S0.txt",sep="@")
votes$X <- NULL
# make the votes into integers
votes$Vote_Count <- as.integer(votes$Vote_Count)


# create a table of DA votes by making a tall table wide based on precinct and DA race candidates
DAvotes <- dcast(votes[votes$Office_Prop.Name %in% c("DISTRICT ATTORNEY-DEM", "DISTRICT ATTORNEY-REP"),], Precinct_Name ~ Tape_Text, value.var="Vote_Count", fun.aggregate=sum)

# tidy up the names of the columns to remove spaces, commas, etc...
names(DAvotes)<- make.names(names(DAvotes))

# create a totals column
DAvotes$vote_totals <- rowSums(DAvotes[-1])

#Now split the precinct name into ward/precinct and then create a combined column since we'll need that for the mapping
DAvotes$Ward <- NULL
DAvotes$Division <- NULL
DAvotes[,c("Ward","Division")] <- str_split_fixed(as.character(DAvotes$Precinct_Name),"-",2)
DAvotes$Precinct_Name <- paste(DAvotes$Ward,DAvotes$Division, sep="")

#make a parallel table with percentages rather than absolute numbers of votes
#I bet there is a better wy to do this, without having to have a parallel table, but fuck it.
DApercents <- round(DAvotes[,c(-1,-11,-12,-13)]/DAvotes[,11],3) * 100
DApercents$Precinct_Name <- DAvotes$Precinct_Name
DApercents$Division <- DAvotes$Division
DApercents$Ward <- DAvotes$Ward

DApercents <- DApercents[,c((ncol(DApercents)-2):ncol(DApercents),1:(ncol(DApercents)-3))]

# map it!
tmp <- tempdir()
unzip("Political_Divisions.zip", exdir = tmp)
precincts <- readOGR(dsn=tmp)
precincts <- rmapshaper::ms_simplify(precincts)

# use a left join b/c some districts have no data
precincts@data = precincts@data %>%
  left_join(DApercents, by=c("DIVISION_N"="Precinct_Name"), copy=TRUE)


votePopup <- paste0("<strong>Ward:</strong> ", 
                      precincts$Ward, 
                      "<br><strong>Division:</strong> ",
                      precincts$Division,
                      "<br><strong>Deni:</strong>", precincts$DENI..T, "%",
                      "<br><strong>Shabazz:</strong>", precincts$EL.SHABAZZ..T, "%",
                      "<br><strong>Grossman:</strong>", precincts$GROSSMAN..B, "%",
                      "<br><strong>Khan:</strong>", precincts$KHAN..J, "%",
                      "<br><strong>Krasner:</strong>", precincts$KRASNER..L, "%",
                      "<br><strong>Negrin:</strong>", precincts$NEGRIN..R, "%",
                      "<br><strong>O'Neill:</strong>", precincts$O.NEILL..J, "%",
                      "<br><strong>Untermeyer:</strong>", precincts$UNTERMEYER..M, "%",
                      "<br><strong>Write In:</strong>", precincts$Write_In, "%")
                    
pal <- colorNumeric("YlOrBr",domain=0:100)

daVoteMap <- leaflet(data = precincts) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(KRASNER..L), 
              fillOpacity = .65,
              color="#BDBDC3",
              weight = 1, 
              popup = votePopup) %>%
  setView(lng = -75.07, lat = 40, zoom=11)



