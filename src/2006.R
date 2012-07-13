


jaggit06 <- function(data, candidate = "epn") {
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
  
  foo$alpha <- c(rep(NA,length(min(data$date):max(data$date))))  ## 
  foo$alpha <- c(foo$alpha, rep(NA, 8), final.2006[getIndex(candidate)])
  foo$NPERIODS <- length(foo$alpha) 
  ## write content of object foo back to top level directory
  for(i in 1:length(foo))
    assign(x=names(foo)[i],
           value=foo[[i]])
  dump(list=names(foo), file = str_c("jags2006/dumpdata-", candidate, ".R"))   ## dump
  system.str <- str_c("cd jags2006;jags jags-", candidate, ".cmd")
  ## run jags job in batch mode
  system(system.str)
}

readJagsOutput06 <- function(candidate) {
  # Description: 
#   Read the files created by JAGS
# Args:
#  
#
# Returns: 
#   a list with the posterior estimates of the true voting intention levels in alpha
  ##and the house effect
  candidate <- toupper(candidate)
  alpha <- read.coda(str_c("jags2006/", candidate, "chain1.txt"),
                     str_c("jags2006/", candidate, "index.txt"),
                     quiet = TRUE)
  total.days <- length(seq(start.date, end.date, by ="day")) + 8
  house <- alpha[,(total.days + 3):(total.days+ 2 + 9)]
  sigma <- alpha[,(total.days + 2)]
  alpha <- alpha[,1:total.days]
  return(list(alpha = alpha,
              house = house,
              sigma = sigma
  ))
}

convertToDataFrame06 <- function(list) {
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
  alpha.ci$date <- seq(start.date, end.date + 8, by ="day") 
  alpha.ci <- cbind(alpha.bar, alpha.ci)
  names(alpha.ci) <- c("bar", "lower", "upper", "date")
  
  return(alpha.ci)
}

houseDataFrame06 <- function(ll) {
  house <- as.data.frame(ll$house)
  names(house) <- levels(data$pollster)
  house <- melt(house)
  house$variable <- reorder(house$variable, house$value, function(x) abs(mean(x)))
  return(house)
}

six <- read.csv("data/polls-2006.csv")

six$Fuente <- NULL
six <- transform(six, calderon = calderon / 100)
six <- transform(six, madrazo = madrazo / 100)
six <- transform(six, amlo = amlo / 100)
six$other <- 1 - apply(six[,3:5], 1, sum) 
six <- six[,c(1:5,10)]
six$Date <- as.Date(six$Date, format = "%d de %B de %Y")


final.2006 <- c(.2291, .3635, .3695, .037711)
names(six) <- c("pollster", "date", "jvm", "epn", "amlo","gqt")
data <- subset(six, gqt <= .1)
data <- data[,c("epn", "jvm", "amlo", "gqt", "pollster", "date")]
data$size <- 1000
data$date.posix <- as.Date(data$date)
data <- subset(data, date.posix >= as.Date("2006-02-01"))
data <- data[!is.na(data$epn), ]
start.date <- min(as.Date(data$date))
end.date <- max(as.Date(data$date))
data$date <- julian(as.Date(data$date),
                    origin=as.Date("2004-01-01"))
data <- arrange(data, date)
day.of.election <- julian(as.Date("2006-07-02"),
                          origin=as.Date("2004-01-01"))


##jaggit06(data, "gqt")

foreach(i = c("epn", "amlo", "jvm", "gqt")) %dopar% {
  jaggit06(data, i)
  NULL
}

#Read the output and convert to data.frame for each of the 4 candidates
sim.epn.2006 <- readJagsOutput06("epn")
alpha.epn.2006 <- convertToDataFrame06(sim.epn.2006)
alpha.epn.2006$candidate <- "epn"

sim.amlo.2006 <- readJagsOutput06("amlo")
alpha.amlo.2006 <- convertToDataFrame06(sim.amlo.2006)
alpha.amlo.2006$candidate <- "amlo"


sim.jvm.2006 <- readJagsOutput06("jvm")
alpha.jvm.2006 <- convertToDataFrame06(sim.jvm.2006)
alpha.jvm.2006$candidate <- "jvm"

sim.gqt.2006 <- readJagsOutput06("gqt")
alpha.gqt.2006 <- convertToDataFrame06(sim.gqt.2006)
alpha.gqt.2006$candidate <- "gqt"

#bind the data for all the candidates
alpha.2006 <- rbind(alpha.epn.2006, alpha.amlo.2006)
alpha.2006 <- rbind(alpha.2006, alpha.jvm.2006)
alpha.2006 <- rbind(alpha.2006, alpha.gqt.2006)

##House effects
house.epn.2006 <- houseDataFrame06(sim.epn.2006)
house.epn.2006$candidate <- "epn"

house.amlo.2006 <- houseDataFrame06(sim.amlo.2006)
house.amlo.2006$candidate <- "amlo"

house.jvm.2006 <- houseDataFrame06(sim.jvm.2006)
house.jvm.2006$candidate <- "jvm"

house2006 <- rbind(house.jvm.2006, house.amlo.2006)
house2006 <- rbind(house2006, house.epn.2006)

house2006 <- ddply(house2006,
                   .(variable, candidate),
                   transform,
                   order = abs(mean(value)))
house2006 <- ddply(house2006,
                   .(variable),
                   transform,
                   order = abs(mean(order)))
house2006$variable <- with(house2006, reorder(variable, order, ))

plotPosteriorHouseDensity(house2006, "Posterior Distribution of House Effects, Mexican 2006 Presidential Election") +
    scale_fill_manual("candidate",
                      values = c("#ffd217","red", "#00448b"),
                      labels = c("AMLO", "Calderón", "Madrazo"))
ggsave(file.path("graphs","house2006.svg"), dpi = 100, w = 9.6, h=6)

##The priors for the 2012 election
ddply(house.epn.2006, .(variable), summarise,
      mean = round(mean(value), 4), prec = round(1 / var(value)))
##The overall mean and precision
ddply(house.epn.2006, .(), summarise,
      overallmean = mean(value),
      overallprec = 1 / var(value))



##The priors for the 2012 election
ddply(house.amlo.2006, .(variable), summarise,
      mean = round(mean(value), 4), prec = round(1 / var(value)))
##The overall mean and precision
ddply(house.amlo.2006, .(), summarise,
      overallmean = mean(value),
      overallprec = 1 / var(value))



##The priors for the 2012 election
ddply(house.jvm.2006, .(variable), summarise,
      mean = round(mean(value), 4), prec = round(1 / var(value)))
##The overall mean and precision
ddply(house.jvm.2006, .(), summarise,
      overallmean = mean(value),
      overallprec = 1 / var(value))



mdata.2006 <- melt(data[, c("date.posix", "pollster",
                       "epn", "amlo", "jvm", "gqt")],
              id = c("date.posix", "pollster"))
names(mdata.2006) <- c("date", "pollster", "candidate", "value")


p <- ggplot(alpha.2006, aes(date, bar, group = candidate)) +
  geom_line(aes(color = candidate), size = 1.2) +
  #geom_vline(xintercept = as.numeric(as.Date("2012-05-06"))) +
  geom_point(data = mdata.2006, size = 2, alpha = .7,
             aes(date, value, group = candidate, color = candidate )) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = .2)+
  scale_y_continuous(labels = percent) +
  ylab("") + xlab("date") +
  scale_color_manual("candidate",
                     values = c("#ffd217", "red", "lightblue", "#00448b"),
                     labels = c("Madrazo", "AMLO", "Calderón", "Other"),
                     breaks = c("epn", "amlo", "jvm", "gqt")) +
  opts(title = "Net Voting Intention with Trendline (Kalman Filter)\n2006 Mexican Presidential Election and Final Result") +
  geom_point(data = data.frame(res = final.2006,
               date = as.Date("2006-07-02"),
               candidate = "cat"),
               aes(date, res, shape = "Final Result")) +
  scale_shape_manual("", values = c(15))
ggsave(file.path("graphs","kalmanreg-2006.svg"), plot = p, dpi = 100, w = 9.6, h=6)


mpolls.pred.2006 <- merge(alpha.2006, mdata.2006, by = c("date", "candidate"),
                     all.x = TRUE)
mpolls.pred.2006 <- ddply(mpolls.pred.2006, .(pollster), transform,
      order = mean((bar - value)^2, na.rm = TRUE))
mpolls.pred.2006$pollster <- reorder(mpolls.pred.2006$pollster,
                                     mpolls.pred.2006$order)
mpolls.pred.2006 <- na.omit(mpolls.pred.2006)

p <- ggplot(alpha.2006, aes(date, bar, group = candidate)) +
  geom_line(aes(color = candidate), size = .8) +
  #geom_vline(xintercept = as.numeric(as.Date("2012-05-06"))) +
  geom_point(data = mpolls.pred.2006, size = 2.5, alpha = 1,
             aes(date, value, group = candidate, color = candidate )) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = .1)+
  scale_y_continuous(labels = percent) +
  ylab("") + xlab("date") +
  scale_color_manual("candidate",
                     values = c("#ffd217", "red", "lightblue", "#00448b"),
                     labels = c("Madrazo", "AMLO", "Calderón", "Other"),
                     breaks = c("epn", "amlo", "jvm", "gqt"))+
  facet_wrap(~pollster) +
  opts(title = "Net Voting Intention with Trendline (Kalman Filter) 2006 Election, by Polling Firm")+
  geom_point(data = data.frame(res = final.2006,
               date = as.Date("2006-07-02"),
               candidate = "cat"),
               aes(date, res, shape = "Final Result")) +
  scale_shape_manual("", values = c(15))
ggsave(file.path("graphs","house-effect-2006.svg"), plot = p, dpi = 100, w = 9.6, h=6)
