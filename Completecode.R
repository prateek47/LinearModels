
# list of libraries
library(MASS)
library(car)

# Data for the files
movie_data = read.csv("MovieData.csv")
ticket_price = read.csv("TcktPrice.csv")
box_office_earning = read.csv("TotalBoxOffice.csv")
tot_screen_count = read.csv("TotalScreenCount.csv")

# Data Cleaning

drops <- c("X")
moviesDF <- movie_data[ ,!(names(movie_data) %in% drops)]

# removing the $ sign from the values
moviesDF$gross_earning <- as.integer(gsub("[$,]","",moviesDF$gross_earning))
moviesDF$theatre_count <- as.integer(gsub(",","",moviesDF$theatre_count))
moviesDF[moviesDF=="N/A"] <- NA
moviesDF <- transform(moviesDF, yearofrelease= as.numeric(yearofrelease), 
                      IMDB_Rating= as.numeric(IMDB_Rating), Tomato_UserRating= as.numeric(Tomato_UserRating),
                      Tomato_UserMeter= as.numeric(Tomato_UserMeter),Tomato_Rating= as.numeric(Tomato_Rating),
                      Tomato_Meter= as.numeric(Tomato_Meter), Genre = as.character(Genre))

# similarly for other dataframes
ticket_price$price <-  as.numeric(gsub("[$,]","",ticket_price$price))
box_office_earning$price_billions <- as.numeric(gsub("[$,]", "", box_office_earning$price))
tot_screen_count$indoor_screen <- as.numeric(gsub("[,]", "", tot_screen_count$indoor_screen))
tot_screen_count$total_screen <- as.numeric(gsub("[, ]", "", tot_screen_count$total_screen))
#tot_screen_count[tot_screen_count == "N/A"] <- NA

#---------------------------------------------------------------------------------------------------
# Creating dummy variables

#Split the genre by comma
genre <-strsplit(moviesDF$Genre,',')
maxLen <- max(sapply(genre, length))
genre1 <- rep(NA,dim(moviesDF)[1])
genre2 <- rep(NA,dim(moviesDF)[1])
genre3 <- rep(NA,dim(moviesDF)[1])
for(i in 1:dim(moviesDF)[1]){
  if(!is.na(genre[[i]][1])){
    genre1[i] <- genre[[i]][1]
  }
  if(!is.na(genre[[i]][2])){
    genre2[i] <- genre[[i]][2]
  }
  if(!is.na(genre[[i]][3])){
    genre3[i] <- genre[[i]][3]
  }
}

#get rid of white space
genre1 <- str_trim(genre1)
genre2 <- str_trim(genre2)
genre3 <- str_trim(genre3)

#Find the categories of genre
table(genre1)
table(genre2)
table(genre3)

#create dummy variables
Action <- rep(0,dim(moviesDF)[1])
Adult <- rep(0,dim(moviesDF)[1])
Adventure <- rep(0,dim(moviesDF)[1])
Animation <- rep(0,dim(moviesDF)[1])
Biography <- rep(0,dim(moviesDF)[1])
Comedy <- rep(0,dim(moviesDF)[1])
Crime <- rep(0,dim(moviesDF)[1])
Documentary <- rep(0,dim(moviesDF)[1])
Drama <- rep(0,dim(moviesDF)[1])
Family <- rep(0,dim(moviesDF)[1])
Fantasy <- rep(0,dim(moviesDF)[1])
History <- rep(0,dim(moviesDF)[1])
Horror <- rep(0,dim(moviesDF)[1])
Music <- rep(0,dim(moviesDF)[1])
Musical <- rep(0,dim(moviesDF)[1])
Mystery <- rep(0,dim(moviesDF)[1])
Romance <- rep(0,dim(moviesDF)[1])
SciFi <- rep(0,dim(moviesDF)[1])
Short <- rep(0,dim(moviesDF)[1])
Sport <- rep(0,dim(moviesDF)[1])
Thriller <- rep(0,dim(moviesDF)[1])
War <- rep(0,dim(moviesDF)[1])
Western <- rep(0,dim(moviesDF)[1])

# splitting the genre into factors
for(i in 1:dim(moviesDF)[1]){
  if(any(genre1[i]=='Action',genre2[i]=='Action',genre3[i]=='Action',na.rm =T)){
    Action[i] <- 1
  }
  if(any(genre1[i]=='Adult',genre2[i]=='Adult',genre3[i]=='Adult',na.rm =T)){
    Adult[i] <- 1
  }
  if(any(genre1[i]=='Adventure',genre2[i]=='Adventure',genre3[i]=='Adventure',na.rm =T)){
    Adventure[i] <- 1
  }
  if(any(genre1[i]=='Animation',genre2[i]=='Animation',genre3[i]=='Animation',na.rm =T)){
    Animation[i] <- 1
  }
  if(any(genre1[i]=='Biography',genre2[i]=='Biography',genre3[i]=='Biography',na.rm =T)){
    Biography[i] <- 1
  }
  if(any(genre1[i]=='Comedy',genre2[i]=='Comedy',genre3[i]=='Comedy',na.rm =T)){
    Comedy[i] <- 1
  }
  if(any(genre1[i]=='Crime',genre2[i]=='Crime',genre3[i]=='Crime',na.rm =T)){
    Crime[i] <- 1
  }
  if(any(genre1[i]=='Documentary',genre2[i]=='Documentary',genre3[i]=='Documentary',na.rm =T)){
    Documentary[i] <- 1
  }
  if(any(genre1[i]=='Drama',genre2[i]=='Drama',genre3[i]=='Drama',na.rm =T)){
    Drama[i] <- 1
  }
  if(any(genre1[i]=='Family',genre2[i]=='Family',genre3[i]=='Family',na.rm =T)){
    Family[i] <- 1
  }
  if(any(genre1[i]=='Fantasy',genre2[i]=='Fantasy',genre3[i]=='Fantasy',na.rm =T)){
    Fantasy[i] <- 1
  }
  if(any(genre1[i]=='History',genre2[i]=='History',genre3[i]=='History',na.rm =T)){
    History[i] <- 1
  }
  if(any(genre1[i]=='Horror',genre2[i]=='Horror',genre3[i]=='Horror',na.rm =T)){
    Horror[i] <- 1
  }
  if(any(genre1[i]=='Music',genre2[i]=='Music',genre3[i]=='Music',na.rm =T)){
    Music[i] <- 1
  }
  if(any(genre1[i]=='Musical',genre2[i]=='Musical',genre3[i]=='Musical',na.rm =T)){
    Musical[i] <- 1
  }
  if(any(genre1[i]=='Mystery',genre2[i]=='Mystery',genre3[i]=='Mystery',na.rm =T)){
    Mystery[i] <- 1
  }
  if(any(genre1[i]=='Romance',genre2[i]=='Romance',genre3[i]=='Romance',na.rm =T)){
    Romance[i] <- 1
  }
  if(any(genre1[i]=='Sci-Fi',genre2[i]=='Sci-Fi',genre3[i]=='Sci-Fi',na.rm =T)){
    SciFi[i] <- 1
  }
  if(any(genre1[i]=='Short',genre2[i]=='Short',genre3[i]=='Short',na.rm =T)){
    Short[i] <- 1
  }
  if(any(genre1[i]=='Sport',genre2[i]=='Sport',genre3[i]=='Sport',na.rm =T)){
    Sport[i] <- 1
  }
  if(any(genre1[i]=='Thriller',genre2[i]=='Thriller',genre3[i]=='Thriller',na.rm =T)){
    Thriller[i] <- 1
  }
  if(any(genre1[i]=='War',genre2[i]=='War',genre3[i]=='War',na.rm =T)){
    War[i] <- 1
  }
  if(any(genre1[i]=='Western',genre2[i]=='Western',genre3[i]=='Western',na.rm =T)){
    Western[i] <- 1
  }
}

#-------------------------------------------------------------------------------------------
# creating a new dataframe
film <- cbind(moviesDF,as.factor(Action),as.factor(Adult),as.factor(Adventure),as.factor(Animation),as.factor(Biography),as.factor(Comedy),
              as.factor(Crime), as.factor(Documentary),as.factor(Drama),as.factor(Family),as.factor(Fantasy),as.factor(History),as.factor(Horror),
              as.factor(Music),as.factor(Musical), as.factor(Mystery),as.factor(Romance),as.factor(SciFi), as.factor(Short),as.factor(Sport),as.factor(Thriller),
              as.factor(War),as.factor(Western) )

my_names<-c("Name","gross_earning","theatre_count","year","IMDB_Rating","Genre","Tomato_Meter","Tomato_Rating","Tomato_User_Meter",
            "Tomato_User_Rating","MPAA_Rating","Action","Adult","Adventure","Animation","Biography","Comedy","Crime",
            "Documentary","Drama","Family","Fantasy","History","Horror","Music","Musical","Mystery","Romance","SciFi","Short","Sport","Thriller","War","Western")
names(film) <- my_names
names(film)

#-----------------------------------------------------------------------------------------
# Storing the data file
write.csv(film, file = "Film_data", row.names = FALSE)

# --------------------------------------------------------------------------------------------

# adjusting for inflation
# adjusting the inflation value
ticket_price$inflation <- ticket_price$price/ticket_price$price[15]

# adding to the film Database
film <- merge(ticket_price,film,by='year')
# calculating the number of tickets sold
film$num_of_tickt_sold <- film$gross_earning/film$price
# calculating the gross earning for each movie after Inflation adjustment
film$gross_earning_after <- film$gross_earning/(film$inflation)

write.csv(film, "Film_Data2", row.names = FALSE)

# Creating a train and test split 
# creating a sample
s <- sample(1:nrow(film), 1000)
film.train <- film[s, ]
film.test <- film[-s, ]

# 1st Model
# Creating a full model
lm.full <- lm(gross_earning_after~theatre_count+IMDB_Rating+Tomato_Meter+Tomato_Rating+
                Tomato_User_Meter+Tomato_User_Rating+Action+Adventure+Animation+Biography+
                Comedy+Crime+Documentary+Drama+Family+Fantasy+History+Horror+Music+Musical+
                Mystery+Romance+SciFi+Short+Sport+Thriller+War+Western,data=film.train)

summary(lm.full)

pred <- predict(lm.full, newdata = film.test)
mse.full <- mean((film.test$gross_earning_after-pred)^2, na.rm = TRUE)

# 2nd Model
# based upon the p-values

lm2 <- lm(gross_earning_after~theatre_count+Tomato_Rating+Tomato_User_Meter+Tomato_User_Rating+
            Adventure+Animation+Crime+Drama+Family+Fantasy+Horror+Music+Short,data=film.train)

summary(lm2)

pred2 <- predict(lm2, newdata = film.test)
mse2 <- mean((film.test$gross_earning_after-pred2)^2, na.rm = TRUE)

# Stepwise AIC selection
stepAIC(lm.full, direction = "both")

# 3rd Model
# based upon AIC values
lm3 <- lm(formula = gross_earning_after ~ theatre_count + Tomato_Rating + Tomato_User_Meter + 
            Tomato_User_Rating + Adventure + Animation + Crime + Drama + Family + Fantasy + 
            Horror + Music + Short + Documentary, data = film.train)
summary(lm3)

pred3 <- predict(lm3, newdata =  film.test)
mse3 <- mean((film.test$gross_earning_after-pred3)^2, na.rm = TRUE)

# checking for multicollinearity in the model
vif(lm3)

# 4th Model
lm4 <- lm(formula = gross_earning_after ~ theatre_count + Tomato_Rating + Tomato_User_Meter + 
            Adventure + Animation + Crime + Drama + Family + Fantasy + 
            Horror + Music + Short + Documentary, data = film.train)
summary(lm4) # .5014

pred4 <- predict(lm4, newdata =  film.test)
mse4 <- mean((film.test$gross_earning_after-pred4)^2, na.rm = TRUE)
# Tomato user rating is removed based on the mse value

# checking the normality plot
# trying the normality plots
r_student1 <- rstudent(model = lm4)
qqnorm(r_student1, xlab = "external standardized residuals",
       ylab = "Probability", main = "Normal Probablity plot")
qqline(r_student1)

# the plot is positively skewed 

# calculating the box cox
bc01 <- boxcox(lm4,plotit=T, interp=T)
bc01   # Lists log-likelihoods and corresponding lambdas.

maxlambda01 <- bc01$x[bc01$y==max(bc01$y)]
maxlambda01 #[1] -0.303

# 5th Model
lm5 <- lm(formula = ((gross_earning_after)^(-0.303)) ~ theatre_count + Tomato_Rating + Tomato_User_Meter + 
            Adventure + Animation + Crime + Drama + Family + Fantasy + 
            Horror + Music + Short + Documentary, data = film.train)
summary(lm5) # .5203

pred5 <- predict(lm5, newdata =  film.test)
mse5 <- mean((film.test$gross_earning_after-(pred5)^(1/(-0.303)))^2, na.rm = TRUE)
# Tomato user rating is removed based on the mse value

# checking the normality plot
# trying the normality plots
r_student2 <- rstudent(model = lm5)
qqnorm(r_student2, xlab = "external standardized residuals",
       ylab = "Probability", main = "Normal Probablity plot with Box-Cox Transformation")
qqline(r_student2)

# 6th Model
lm6 <- lm(formula = (log(gross_earning_after)) ~ theatre_count + Tomato_Rating + Tomato_User_Meter + 
            Adventure + Animation + Crime + Drama + Family + Fantasy + 
            Horror + Music + Short + Documentary, data = film.train)
summary(lm6) # .5388

pred6 <- predict(lm6, newdata =  film.test)
mse6 <- mean((film.test$gross_earning_after-exp(pred6))^2, na.rm = TRUE)
# Tomato user rating is removed based on the mse value

# checking the normality plot
# trying the normality plots
r_student3 <- rstudent(model = lm6)
qqnorm(r_student3, xlab = "external standardized residuals",
       ylab = "Probability", main = "Normal Probablity plot with Log Transformation")
qqline(r_student3)


# Performing ANOVA to calculate feature contributions
anova.lm6 <- aov(formula = (log(gross_earning_after)) ~ theatre_count + Tomato_Rating + Tomato_User_Meter + 
                   Adventure + Animation + Crime + Drama + Family + Fantasy + 
                   Horror + Music + Short + Documentary, data = film.train)
summary(anova.lm6)

# Interesting Findings

max(film$Tomato_Rating)


colMax <- function(data) sapply(data, max, na.rm = TRUE)
