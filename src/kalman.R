########################################################
# Author: Diego Valle-Jones
# Website: www.diegovalle.net
# Date Created: Wed Jun 20 17:39:01 2012
# Email: diegovalle at gmail.com
# Purpose: Implement a poll of polls based on the paper Pooling of the polls by Simon Jackman and its associated replication code 
# Copyright (c) Diego Valle-Jones. All rights reserved

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
  
  if(candidate == "epn")
    index <- 1
  else if(candidate == "amlo")
    index <- 2
  else if(candidate == "jvm")
    index <- 3
  else if(candidate == "gqt")
    index <- 4
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
  foo$NPERIODS <- length(min(data$date):max(data$date))
  foo$alpha <- c(rep(NA,length((min(data$date)):max(data$date))))  ## actual 2PP on last day
  
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
  total.days <- length(seq(start.date, end.date, by ="day"))
  house <- alpha[,(total.days + 2):(total.days+ 2 + 11)]
  sigma <- alpha[,(total.days + 1)]
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
  alpha.ci$date <- seq(start.date, end.date, by ="day") 
  alpha.ci <- cbind(alpha.bar, alpha.ci)
  names(alpha.ci) <- c("bar", "lower", "upper", "date")
  
  return(alpha.ci)
}


#Read the file with the weekly gea-isa poll
data <- read.csv("clean-data/polldb-weeklygea.csv")
data$date.posix <- as.Date(data$date)
data <- subset(data, date.posix >= as.Date("2012-02-01"))
data <- data[!is.na(data$epn), ]
start.date <- min(as.Date(data$date))
end.date <- max(as.Date(data$date))
data$date <- julian(as.Date(data$date),
                    origin=as.Date("2004-01-01"))
data <- arrange(data, date)
day.of.election <- julian(as.Date("2012-07-01"),
                          origin=as.Date("2004-01-01"))



##length(sim.epn$alpha[,1])

#Since JAGS is not multithreaded we can parallelize it manually
foreach(i = c("epn", "amlo", "jvm", "gqt")) %dopar% {
  jaggit(data, i)
  NULL
}

#Read the output and convert to data.frame for each of the 4 candidates
sim.epn <- readJagsOutput("epn")
alpha.epn <- convertToDataFrame(sim.epn)
alpha.epn$candidate <- "epn"

sim.amlo <- readJagsOutput("amlo")
alpha.amlo <- convertToDataFrame(sim.amlo)
alpha.amlo$candidate <- "amlo"


sim.jvm <- readJagsOutput("jvm")
alpha.jvm <- convertToDataFrame(sim.jvm)
alpha.jvm$candidate <- "jvm"

sim.gqt <- readJagsOutput("gqt")
alpha.gqt <- convertToDataFrame(sim.gqt)
alpha.gqt$candidate <- "gqt"

#bind the data for all the candidates
alpha <- rbind(alpha.epn, alpha.amlo)
alpha <- rbind(alpha, alpha.jvm)
alpha <- rbind(alpha, alpha.gqt)



#Summary statistics
lastcol <- ncol(sim.amlo$alpha)
#Amlo > JVM
sum(sim.amlo$alpha[,lastcol] > sim.jvm$alpha[,lastcol]) / length(sim.jvm$alpha[,lastcol])
mean(sim.epn$alpha[,lastcol])
mean(sim.amlo$alpha[,lastcol])
mean(sim.jvm$alpha[,lastcol])
mean(sim.gqt$alpha[,lastcol])
quantile(sim.epn$alpha[,lastcol], c(.0025, .975))
quantile(sim.amlo$alpha[,lastcol], c(.0025, .975))
quantile(sim.jvm$alpha[,lastcol], c(.0025, .975))
quantile(sim.gqt$alpha[,lastcol], c(.0025, .975))

sum(sim.gqt$alpha[,120] > .02)

#Long form for the plots
mdata <- melt(data[, c("date.posix", "pollster",
                       "epn", "amlo", "jvm", "gqt")],
              id = c("date.posix", "pollster"))
names(mdata) <- c("date", "pollster", "candidate", "value")


p <- ggplot(alpha, aes(date, bar, group = candidate)) +
  geom_line(aes(color = candidate), size = 1.2) +
  #geom_vline(xintercept = as.numeric(as.Date("2012-05-06"))) +
  geom_point(data = mdata, size = 2, alpha = .7,
             aes(date, value, group = candidate, color = candidate )) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = .2)+
  scale_y_continuous(labels = percent) +
  ylab("") + xlab("date") +
  scale_color_manual("candidate",
                     values = c("#ffd217", "red", "lightblue", "#00448b"),
                     labels = c("EPN", "AMLO", "JVM", "GQT"),
                     breaks = c("epn", "amlo", "jvm", "gqt")) +
  opts(title = "Net Voting Intention with Trendline (Kalman Filter)\n2012 Mexican Presidential Election") 
ggsave(file.path("graphs","kalmanreg.svg"), plot = p, dpi = 100, w = 6, h=6)


#House Effects
#Order the pollster by MSE from the trendline
mpolls.pred <- merge(alpha, mdata, by = c("date", "candidate"),
                     all.x = TRUE)
mpolls.pred <- ddply(mpolls.pred, .(pollster), transform,
      order = mean((bar - value)^2, na.rm = TRUE))
mpolls.pred$pollster <- reorder(mpolls.pred$pollster, mpolls.pred$order)
mpolls.pred <- na.omit(mpolls.pred)

p <- ggplot(alpha, aes(date, bar, group = candidate)) +
  geom_line(aes(color = candidate), size = 1.2) +
  #geom_vline(xintercept = as.numeric(as.Date("2012-05-06"))) +
  geom_point(data = mpolls.pred, size = 2, alpha = .7,
             aes(date, value, group = candidate, color = candidate )) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = .2)+
  scale_y_continuous(labels = percent) +
  ylab("") + xlab("date") +
  scale_color_manual("candidate",
                     values = c("#ffd217", "red", "lightblue", "#00448b"),
                     labels = c("EPN", "AMLO", "JVM", "GQT"),
                     breaks = c("epn", "amlo", "jvm", "gqt"))+
  facet_wrap(~pollster) +
  opts(title = "Net Voting Intention with Trendline (Kalman Filter), by Polling Firm")
ggsave(file.path("graphs","house-effect.svg"), plot = p, dpi = 100, w = 9.6, h=6)


##Chart with alphabetical order
##p <- p+
##  facet_wrap(~pollster) +
##  opts(title = "Net Voting Intention with Trendline (Kalman Filter), by Polling Firm")


##Suport for JVM dropped with the start of her campaing
ggplot(alpha.jvm, aes(date, bar)) +
  geom_line(aes(color = candidate), size = 1.2, color = "#00448b") +
  geom_vline(xintercept = as.numeric(as.Date("2012-03-30"))) +
  annotate("text", x = as.Date("2012-03-30"), y = .34, hjust = -0.05,
           label="Campaign Start")+
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = .2)+
  #geom_vline(xintercept = as.numeric(as.Date("2012-05-06"))) +
  geom_point(data = subset(mdata, candidate == "jvm"), size = 4, alpha = 1,
             aes(date, value, group = pollster, color = pollster)) +
  scale_y_continuous(labels = percent) +
  scale_color_hue("pollster") +
  ylab("") + xlab("date") +
  opts(title = "Vázquez Mota Voting Intention with Trendline (Kalman Filter with House Effects)")
ggsave(file.path("graphs","josefina.svg"), dpi = 100, w = 9.6, h=5)

##Support for Quadri went up after the first presidential debate
ggplot(alpha.gqt, aes(date, bar, group = candidate)) +
  geom_point(data = subset(mdata, candidate == "gqt"), size = 4, alpha = 1,
             aes(date, value, group = pollster)) +
  geom_line(aes(color = candidate), size = 1.2, color = "lightblue") +
  #geom_vline(xintercept = as.numeric(as.Date("2012-04-01"))) +
  geom_vline(xintercept = as.numeric(as.Date("2012-05-06"))) +
  annotate("text", x = as.Date("2012-05-06"), y = 0.01, hjust = -0.05,
           label="Presidential Debate") +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = .2)+
  scale_y_continuous(labels = percent) +
  ylab("") + xlab("date") +
  scale_color_hue("pollster") +
  opts(title = "Quadri Voting Intention with Trendline (Kalman Filter) - 2012 Mexican Presidential Election")
ggsave(file.path("graphs","quadri.svg"), dpi = 100, w = 9.6, h=5)
