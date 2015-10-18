# Team Member's Name & Computing ID's: Ferris, Kyle, kcf5bw
#                                      Hays, Matthew, mch9qs
#                                      Tong, Wenting, wt4yg
#                                      Agrawal, Prateek, pa7sb
#
#******************************************************************************************
#                             TEAM ASSIGNMENT- 6
#*****************************************************************************************
# 
# Read the dataset in R
trees <- read.csv("trees.csv", header = TRUE)
# Plotting the trees.
plot(trees$x, trees$y)  

#------------
# Functions:-
#------------
#------------------------
# 1. Masuyama's Method
#------------------------
# Function for Basal Area of the sample for Masuyama method
BA.Masuyama <- function(df, min.x, max.x, min.y, max.y, radius = 37){
  total.ba <- 0
  area.of.square <- (750+ 2*radius)^2
  area.of.sample <- pi*(radius)^2
  prob.tree.sample <- area.of.sample/area.of.square
  x <- runif(10^5, min.x, max.x)
  y <- runif(10^5, min.y, max.y)
  value <- 0
  total.ba <- vapply(1:10^5, function(j){
    value <- ((df$x - x[j])^2 + (df$y - y[j])^2 - radius^2 < 0)
    tree.sample.ba <- df$ba[value]
    return(sum(tree.sample.ba)/prob.tree.sample)
  }, FUN.VALUE = numeric(1))
  return(total.ba)
}
#
#----------------------
# 2.Measure Pi Method
#----------------------
# function to find the area of the sample
overlap.area <- function(xt,yt,rl) {  
  dx <- min(xt, 750-xt)
  dy <- min(yt, 750-yt)
  if (dx >= rl & dy >= rl) {
    area <- pi*rl^2
  } else {
    if (dx < rl & dy >= rl) {
      if (dx >= 0) {
        area <- (pi - acos(dx/rl))*rl^2 + dx*sqrt(rl^2 - dx^2)
      } else {
        ndx <- -dx
        area <- acos(ndx/rl)*rl^2 - ndx*sqrt(rl^2 - ndx^2)
      }
    }
    if (dx >= rl & dy < rl) {
      if (dy >= 0) {
        area <- (pi - acos(dy/rl))*rl^2 + dy*sqrt(rl^2 - dy^2)
      } else {
        ndy <- -dy
        area <- acos(ndy/rl)*rl^2 - ndy*sqrt(rl^2 - ndy^2)
      }
    }
    if (dx < rl & dy < rl & (dx^2 + dy^2) >= rl^2) {
      if (dx >= 0 & dy >= 0) {
        area <- (pi-acos(dx/rl)-acos(dy/rl))*rl^2 + dx*sqrt(rl^2-dx^2)+dy*sqrt(rl^2-dy^2)
      }
      if (dx >= 0 & dy < 0) {
        ndy <- -dy
        area <- acos(ndy/rl)*rl^2 - ndy*sqrt(rl^2 - ndy^2)
      }
      if (dx < 0 & dy >= 0) {
        ndx <- -dx
        area <- acos(ndx/rl)*rl^2 - ndx*sqrt(rl^2 - ndx^2)
      }
      if (dx < 0 & dy < 0) {
        area <- 0
      }
    }
    if (dx < rl & dy < rl & (dx^2 + dy^2) < rl^2) {
      if (dx >= 0 & dy >= 0) {
        theta <- (3/2)*pi - acos(dx/rl) - acos(dy/rl)
        area <- (theta/2)*rl^2 + 0.5*(dx*sqrt(rl^2-dx^2)+dy*sqrt(rl^2-dy^2)) + dx*dy
      }
      if (dx >= 0 & dy < 0) {
        area1 <- acos(dx/rl)*rl^2 - dx*sqrt(rl^2 - dx^2)
        ndy <- -dy
        theta <- (3/2)*pi - acos(dx/rl) - acos(ndy/rl)
        area2 <- (theta/2)*rl^2 + 0.5*(dx*sqrt(rl^2-dx^2)+ndy*sqrt(rl^2-ndy^2)) + dx*ndy
        area <- pi*rl^2 - (area1 + area2)
      }
      if (dx < 0 & dy >= 0) {
        area1 <- acos(dy/rl)*rl^2 - dy*sqrt(rl^2 - dy^2)
        ndx <- -dx
        theta <- (3/2)*pi - acos(ndx/rl) - acos(dy/rl)
        area2 <- (theta/2)*rl^2 + 0.5*(ndx*sqrt(rl^2-ndx^2)+dy*sqrt(rl^2-dy^2)) + ndx*dy
        area <- pi*rl^2 - (area1 + area2)
      }
      if (dx < 0 & dy < 0) {
        ndx <- -dx
        ndy <- -dy
        theta <- (3/2)*pi + asin(ndx/rl) + asin(ndy/rl)
        area <- pi*rl^2 - ((theta/2)*rl^2 + 0.5*(ndx*sqrt(rl^2-ndx^2)+ndy*sqrt(rl^2-ndy^2)) - ndx*ndy)
      }
    }
  }
  return(area)
}
# function for Basal Area of the sample for PI method
simulation.pi <- function(df, r){
  total.ba02 <- 0
  xt <- runif(10^5, 0, 750)
  yt <- runif(10^5, 0, 750)
  Area.of.square02 <- (750)^2
  value <- 0
  total.ba02 <- vapply(1:10^5, function(j){
    area.of.sample02 <- overlap.area(xt[j], yt[j], 37)
    prob.tree.sample02 <- area.of.sample02/Area.of.square02
    value <- ((df$x - xt[j])^2 + (df$y - yt[j])^2 - r^2 < 0)
    tree.sample.ba02 <- df$ba[value]
    return(sum(tree.sample.ba02)/prob.tree.sample02)
  }, FUN.VALUE = numeric(1))
  return(total.ba02)
}
#
#----------------------------
# 3.  Repeated Masuyama
#----------------------------
rep.masuyama <- function(df, min.x, max.x, min.y, max.y, radius = 37){
  total.ba <- 0
  area.of.square <- 750^2
  area.of.sample <- pi*(radius)^2
  x <- runif(10^5, min.x, max.x)
  y <- runif(10^5, min.y, max.y)
  value <- 0
  total.ba <- vapply(1:10^5, function(j){
    area <- overlap.area(x[j], y[j], 37)
    value <- ((df$x - x[j])^2 + (df$y - y[j])^2 - radius^2 < 0)
    tree.sample.ba <- df$ba[value]
    while(area < (pi*(radius)^2)){
      xt <- runif(1,min.x,max.x)
      yt <- runif(1,min.y,max.y)
      rl <- sqrt((area.of.sample - area)/pi)
      area <- area + overlap.area(xt,yt,rl)
      value1 <- 0
      value1 <- ((df$x - xt)^2 + (df$y - yt)^2 - rl^2 < 0)
      tree.sample.ba <- c(tree.sample.ba , df$ba[value1])
    }
    prob.tree.sample <- area/area.of.square
    return(sum(tree.sample.ba)/prob.tree.sample)
  }, FUN.VALUE = numeric(1))
  return(total.ba)
}	
#
#*******************************************************************************
#                              MASUYAMA'S METHOD
#*******************************************************************************

start <- proc.time()
sample.ba <- BA.Masuyama(trees, min.x = min(trees$x)-37, max.x = max(trees$x)+37,
                               min.y = min(trees$y)-37, max.y = max(trees$y)+37)
percentage.bias.m <- ((mean(sample.ba)- 311.906)/311.906)*100
percentage.RMSE.m <- 100*(sqrt(var(sample.ba))/311.906)

#*******************************************************************************
#                              PI METHOD
#*******************************************************************************

sample.ba02 <- simulation.pi(trees,37)
percentage.bias.pi <- ((mean(sample.ba02)- 311.906)/311.906)
percentage.RMSE.pi <- 100*(sqrt(var(sample.ba02))/311.906)

#*******************************************************************************
#                              REPEATED MASUYAMA
#*******************************************************************************

sample.ba03 <- rep.masuyama(trees,min.x = min(trees$x), max.x = max(trees$x),
                            min.y = min(trees$y), max.y = max(trees$y))
percentage.bias.rm <- ((mean(sample.ba03)- 311.906)/311.906)
percentage.RMSE.rm <- 100*(sqrt(var(sample.ba03))/311.906)
#
# Total time elasped
proc.time()-start

#*********************************************************************************