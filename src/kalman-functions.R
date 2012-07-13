########################################################
## Author: Diego Valle-Jones
## Website: www.diegovalle.net
## Date Created: Wed Jul  4 21:45:47 2012
## Email: diegovalle at gmail.com
## Purpose: 
## Copyright (c) Diego Valle-Jones. All rights reserved


getIndex <- function(candidate) {
  if(candidate == "epn")
    index <- 1
  else if(candidate == "amlo")
    index <- 2
  else if(candidate == "jvm")
    index <- 3
  else if(candidate == "gqt")
    index <- 4
  return(index)
}

getColor <- function(candidate) {
  if(candidate == "epn")
    color <- "red"
  else if(candidate == "amlo")
    color <- "#ffd217"
  else if(candidate == "jvm")
    color <- "#00448b"
  else if(candidate == "gqt")
    color <- "lightblue"
  return(color)
}

mexVar <- function(data, candidate) {
  ## Description: 
  ## Calculate the variance of a pollwhen there are four candidates
  ## Args:
  ##   data - the data frame with the polls
  ##   candidate - the name of the candidate to calculate the variance
  ## Returns: 
  ##  a scalar with the variance
  
  alpha <- data.frame(data$epn,
    data$amlo,
                      data$jvm,
                      data$gqt)
  alpha <- sapply(alpha, function(x) x * data$size+0.005)
  
  index <- getIndex(candidate)
  var <- apply(alpha, 1, function(x) var(rdirichlet(10000, x)[,index]))
  return(var)
}

jaggit <- function(data, candidate = "epn") {
# Description: 
#   Prepares 
# Args:
#   data - a data frame with the series of polls
#   candidate - the name of the candidate
#
# Returns: 
#   NADA, but the files outputed by jags are left in the jags directory
# Example:
#   jaggit(data)
#   yields: a couple of files in the jags subdirectory
#
  var <- mexVar(data, candidate)
  foo <- list()

 
  foo$y <- data[[candidate]]
  foo$y[foo$y == 0]  <- 0.005
  foo$y[is.na(foo$y)] <- 0.005
  ##var <- foo$y*(1-foo$y) / data$size
  foo$prec <- 1 / var
  foo$date <- data$date - min(data$date) + 1
  foo$org <- data$pollster
  foo$NPOLLS <- length(foo$y) 
  foo$NPERIODS <- length(min(data$date):max(data$date)) + 7
  foo$alpha <- c(rep(NA,length(min(data$date):max(data$date))))  ## 
  foo$alpha <- c(foo$alpha, rep(NA, 6), final.result[getIndex(candidate)])
  ## write content of object foo back to top level directory
  for(i in 1:length(foo))
    assign(x=names(foo)[i],
           value=foo[[i]])
  dump(list=names(foo), file = str_c("jags/dumpdata-", candidate, ".R"))   ## dump
  system.str <- str_c("cd jags;jags jags-", candidate, ".cmd")
  ## run jags job in batch mode
  system(system.str)
}

readJagsOutput <- function(candidate) {
  # Description: 
#   Read the files created by JAGS
# Args:
#  
#
# Returns: 
#   a list with the posterior estimates of the true voting intention levels in alpha
  ##and the house effect
  candidate <- toupper(candidate)
  alpha <- read.coda(str_c("jags/", candidate, "chain1.txt"),
                     str_c("jags/", candidate, "index.txt"),
                     quiet = TRUE)
  total.days <- length(seq(start.date, end.date, by ="day")) + 7
  house <- alpha[,(total.days + 2):(total.days + 14)]
  sigma <- alpha[,(total.days)]
  alpha <- alpha[,1:total.days]
  return(list(alpha = alpha,
              house = house,
              sigma = sigma
  ))
}

convertToDataFrame <- function(list) {
# Description: 
#  Convert the posterior estimates of voting intention into a data.frame
# Args:
#   list - the list returned by readJagsOutput
#
# Returns: 
#  A data frame containing
# Example:
#   covertToDataFrame(list)
#   yields:
#               bar     lower     upper       date candidate
#alpha[1] 0.4649942 0.4506959 0.4795549 2012-02-18       epn
#...........................................................

  alpha.bar <- apply(list$alpha, 2, mean)
  alpha.ci <- apply(list$alpha, 2, quantile, c(.025, .975))
  alpha.ci <- as.data.frame(t(alpha.ci))
  alpha.ci$date <- seq(start.date, end.date + 7, by ="day") 
  alpha.ci <- cbind(alpha.bar, alpha.ci)
  names(alpha.ci) <- c("bar", "lower", "upper", "date")
  
  return(alpha.ci)
}


houseDataFrame <- function(ll) {
  ## Description: 
  ##  Convert the list of house effects from JAGS to a data frame
  ## Args:
  ##   ll - a linked list with the values from JAGS
  ##

  house <- as.data.frame(ll$house)  
  names(house) <- levels(data$pollster)
  house <- melt(house)
  house$variable <- reorder(house$variable, house$value, function(x) abs(mean(x)))
  return(house)
}

plotPosteriorHouseDensity <- function(df, title) {
  ## Description: 
  ##  Pretty plot of the posterior house effects
  ## Args:
  ##   df : data frame fomr house Data Frame
  ##   title : plot title
  ##   candidate : the name of the candidate (useful for color

  ggplot(df, aes(value, group = candidate, fill = candidate)) +
    geom_density(alpha = .4) +
    facet_wrap(~variable) +
    scale_x_continuous(label = percent, limits = c(-.13, .13)) +
    geom_vline(xintercept = 0) +
    opts(title = title) +
    scale_fill_manual("candidate", values = c("#ffd217","red", "#00448b"))
}
