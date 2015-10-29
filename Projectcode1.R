# This is the web sraping code for extraction of dataset

# the libraries used
library(rvest)
library(plyr)
library(stringr)
library(RJSONIO)

# creating a corpus
if (!file.exists("MovieData")) {
  dir.create("MovieData")
}
setwd("MovieData")

# creating a vector for the year from which movies are required
year <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014)

# Scraping the top 100 highest crossing movies per year from boxofficemojo.com, 
# reference: http://www.boxofficemojo.com/yearly/

for( i in 1:length(year)){
  urlmov <- paste0("http://www.boxofficemojo.com/yearly/chart/?yr=", year[i], "&p=.html")
  movie_name <- read_html(urlmov)%>%               # The name of the movies
    html_nodes("td td b font a")%>%
    html_text()
  
  gross_earning <- read_html(urlmov)%>%            # The gross earning by boxofficemojo
    html_nodes("td td tr+ tr td+ td font b")%>%
    html_text()
  
  theater <- read_html(urlmov)%>%                  # The number of total number of theaters/screens, movie was shown
    html_nodes("tr+ tr td:nth-child(5) font")%>%
    html_text()
  
  moviedata <- data.frame(movie_names= movie_name, gross_earning= gross_earning, theatre_count= theater[3:102], yearofrelease = year[i])
  
  filename <- paste0(year[i], ".csv")
  sink(file = filename) %>% # open file to write
    cat(write.csv(moviedata))
  sink()
}

# combining all datasets together together in one dataframe
files_data <- list.files(getwd(), pattern = ".csv")
moviesDF <- do.call(rbind, lapply(files_data, read.csv))

# To extract other variable we have use restful API of imdb which can be found on the link below
# http://www.omdbapi.com/ 
# to extract data as per the movie name following operations are performed on the movie name column
# to make it more readable as per the url requirement of the "omdbapi"
#
moviesDF$new_name <- str_replace_all(moviesDF$movie_names, "[\\?!]", "")
moviesDF$new_name <- str_replace(moviesDF$new_name, "\\((.*?)\\)", "")
moviesDF$new_name <- str_trim(moviesDF$new_name, side = "both")
moviesDF$new_name <- str_replace_all(moviesDF$new_name, "2000$", "")
moviesDF$new_name <- str_trim(moviesDF$new_name, side = "right")
moviesDF$new_name <- str_replace_all(moviesDF$new_name,"[^a-zA-Z0-9\\-'.,]+" , "+")
moviesDF$new_name <- str_replace(moviesDF$new_name,"^The\\+" , "")
moviesDF$new_name <- str_replace(moviesDF$new_name, "^Tyler\\+Perry's\\+", "")

# while due to some differences in the movie names, following specific string transformation
# functions are performed on the movie names to make them more readable
#
moviesDF$new_name[433] <- str_to_lower(moviesDF$new_name[433])
str_sub(moviesDF$new_name[991], start = 1, end = -1) <- "9+"
moviesDF$new_name[c(229,1076, 1023)] <- str_replace(moviesDF$new_name[c(229,1076, 1023)], "\\-", "")
moviesDF$new_name[c(24, 153, 332, 426, 550, 552, 723, 814, 875, 917, 1090, 1131, 1364, 1370, 
                    1399)] <- str_replace(moviesDF$new_name[c(24, 153, 332, 426, 550, 552, 723, 814, 875, 917, 1090, 1131,
                                                              1364, 1370, 1399)], "and\\+", "")
str_sub(moviesDF$new_name[c(34, 1201)], start = 1, end = 9) <- ""
str_sub(moviesDF$new_name[59], start = 8, end = -1) <- ""
str_sub(moviesDF$new_name[698], start = 9, end = -1) <- "+2"
str_sub(moviesDF$new_name[c(436,965)], start = 9, end = -1) <- ""
str_sub(moviesDF$new_name[157], start = 5, end = 6) <- "13"
str_sub(moviesDF$yearofrelease[188], start = 1, end = -1) <- "2000"
str_sub(moviesDF$yearofrelease[c(365,366)], start = 1, end = -1) <- "2002"
str_sub(moviesDF$yearofrelease[489], start = 1, end = -1) <- "2003"
str_sub(moviesDF$yearofrelease[489], start = 1, end = -1) <- "2003"
str_sub(moviesDF$yearofrelease[c(510,930)], start = 1, end = -1) <- "2007"
str_sub(moviesDF$yearofrelease[668], start = 1, end = -1) <- "2005"
str_sub(moviesDF$yearofrelease[1096], start = 1, end = -1) <- "2009"
str_sub(moviesDF$yearofrelease[1164], start = 1, end = -1) <- "2010"
str_sub(moviesDF$yearofrelease[c(1265, 1273)], start = 1, end = -1) <- "2011"
moviesDF$new_name[231] <- str_replace(moviesDF$new_name[231], "The\\+", "")
str_sub(moviesDF$new_name[306], start = 1, end = -1) <- "X-Men+2"
str_sub(moviesDF$new_name[319], start = 10, end = 11) <- "3-D"
str_sub(moviesDF$new_name[570], start = -2, end = -1) <- "3-D"
moviesDF$new_name[366]<- str_c(moviesDF$new_name[366], "...", sep= "")
moviesDF$new_name[433]<- str_c("AVP+", moviesDF$new_name[433], sep= "")
moviesDF$new_name[570]<- str_c("The+", moviesDF$new_name[570], sep= "")
str_sub(moviesDF$new_name[510], start = 4, end = 4) <- "+and+"
str_sub(moviesDF$new_name[1045], start = -4, end = -4) <- "+and+"
str_sub(moviesDF$new_name[570], start = -6, end = -4) <- ""
str_sub(moviesDF$new_name[634], start = 9, end = -1) <- ""
str_sub(moviesDF$new_name[c(670,1256)], start = -3, end = -1) <- ""
str_sub(moviesDF$new_name[848], start = -5, end = -1) <- ""
str_sub(moviesDF$new_name[c(810,1211)], start = 1, end = 11) <- ""
str_sub(moviesDF$new_name[946], start = 1, end = -11) <- ""
str_sub(moviesDF$new_name[748], start = 2, end = 2) <- "%3A"
str_sub(moviesDF$new_name[961], start = -4, end = -1) <- ""
str_sub(moviesDF$new_name[1297], start = 8, end = 8) <- "!+"

# removing some of the invalid movie's names
moviesDF <- moviesDF[-c(61, 993, 551, 1133, 1278, 1282, 1294), ] # 1278(TV Episode), 1282(TV episode), 1294(TV episode)

# Scraping the data using 'omdbapi'
for(i in 1:nrow(moviesDF)){
  movie <- fromJSON(paste0("http://www.omdbapi.com/?t=", moviesDF$new_name[i], "&y=", moviesDF$yearofrelease[i], "&tomatoes=true&r=json"))
  moviesDF$IMDB_Rating[i] <- movie[[16]]        # the imdb rating of the each movie
  moviesDF$Genre[i] <- movie[[6]]               # the Genre to which the movie belongs
  moviesDF$Tomato_Meter[i] <- movie[[20]]       # the Rotten Tomatoes movie Meter
  moviesDF$Tomato_Rating[i] <- movie[[22]]      # The Rotten Tomatoe's movie rating
  moviesDF$Tomato_UserMeter[i] <- movie[[27]]   # The Rotten Tomatoe's User Meter
  moviesDF$Tomato_UserRating[i] <- movie[[28]]  # The Rotten Tomatoe's User Rating
  moviesDF$Rated[i] <- movie[[3]]               # Which rating the movie belonged to
  moviesDF$BoxOffice[i] <- movie[[31]]          # The box office gross extimated by rotten tomatoes
}
#
# Dropping the unnecessary columns
drops <- c("new_name", "X")
moviesDF <- moviesDF[ ,!(names(moviesDF) %in% drops)]

# save the R session as image so that we can reuse it again.
save.image("../LinearModelsProj1.RData")

# get out of the Moviedata folder and save the dataset
setwd("C:/UVa/Stat_6021_Linear Models/workspace")

# saving the dataset in a csv format
write.csv(moviesDF, file = "MovieData.csv")
