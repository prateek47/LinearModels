library(rvest)
library(plyr)

if (!file.exists("MovieData")) {
  dir.create("MovieData")
}
setwd("MovieData")

year <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014)

for( i in 1:length(year)){
  urlmov <- paste0("http://www.boxofficemojo.com/yearly/chart/?yr=", year[15], "&p=.html")
  movie_name <- read_html(urlmov)%>%
    html_nodes("td td b font a")%>%
    html_text()
  
  gross_earning <- read_html(urlmov)%>%
    html_nodes("td td tr+ tr td+ td font b")%>%
    html_text()
  
  theater <- read_html(urlmov)%>%
    html_nodes("tr+ tr td:nth-child(5) font")%>%
    html_text()
  
  moviedata <- data.frame(movie_names= movie_name, gross_earning= gross_earning, theatre_count= theater[3:102], yearofrelease = year[i])
                          
  filename <- paste0(year[i], ".csv")
  sink(file = filename) %>% # open file to write
    cat(write.csv(moviedata))
  sink()
}

files_data <- list.files(getwd(), pattern = ".csv")
moviesDF <- do.call(rbind, lapply(files_data, read.csv))

write.csv(moviesDF, file = "MovieData", row.names = F, col.names = F, sep = ".")
