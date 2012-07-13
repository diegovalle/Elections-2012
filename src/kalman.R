########################################################
# Author: Diego Valle-Jones
# Website: www.diegovalle.net
# Date Created: Wed Jun 20 17:39:01 2012
# Email: diegovalle at gmail.com
# Purpose: Implement a poll of polls based on the paper Pooling of the polls by Simon Jackman and its associated replication code 
# Copyright (c) Diego Valle-Jones. All rights reserved

#Read the file with the weekly gea-isa poll
data <- read.csv("clean-data/polldb-weeklygea.csv")
data$date.posix <- as.Date(data$date)
data <- subset(data, date.posix >= as.Date("2012-03-28"))
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

##sum(sim.gqt$alpha[,120] > .02)


##House Effects
house.epn <- houseDataFrame(sim.epn)
house.epn$candidate <- "epn"

house.amlo <- houseDataFrame(sim.amlo)
house.amlo$candidate <- "amlo"

house.jvm <- houseDataFrame(sim.jvm)
house.jvm$candidate <- "jvm"

house <- rbind(house.jvm, house.amlo)
house <- rbind(house, house.epn)

house <- ddply(house,
                   .(variable, candidate),
                   transform,
                   order = abs(mean(value)))
house <- ddply(house,
                   .(variable),
                   transform,
                   order = abs(mean(order)))
house$variable <- with(house, reorder(variable, order))


plotPosteriorHouseDensity(house, "Posterior Distribution of House Effects, Mexican 2012 Presidential Election")
ggsave(file.path("graphs","house.svg"), dpi = 100, w = 9.6, h=6)






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
  opts(title = "Net Voting Intention with Trendline (Kalman Filter)\n2012 Mexican Presidential Election 'Quick Count'") +
  geom_point(data = data.frame(res = final.result,
               date = as.Date("2012-07-01"),
               candidate = "Cat"),
               aes(date, res, shape = "Official Count")) +
  scale_shape_manual("", values = c(15))
ggsave(file.path("graphs","kalmanreg.svg"), plot = p, dpi = 100, w = 6, h=6)

ma <- function(x,n=3){filter(x,rep(1/n,n), sides=2)}

#House Effects
#Order the pollster by MSE from the trendline
mpolls.pred <- merge(alpha, mdata, by = c("date", "candidate"),
                     all.x = TRUE)

mpolls.pred <- ddply(mpolls.pred, .(pollster, candidate), transform,
      order = ((((bar[length(bar)] - value[length(value)])^2))))

mpolls.pred <- ddply(mpolls.pred, .(pollster), transform,
      order = sqrt(sum(order)))

mpolls.pred$pollster <- reorder(mpolls.pred$pollster, mpolls.pred$order)
mpolls.pred <- na.omit(mpolls.pred)

mpolls.pred <- merge(mpolls.pred, final.polls[,c("pollster",
                                       "euclidian"
                                       )],
       all.x = TRUE)
mpolls.pred$pollster <- reorder(mpolls.pred$pollster, mpolls.pred$euclidian)

p <- ggplot(alpha, aes(date, bar, group = candidate)) +
  geom_line(aes(color = candidate), size = .8) +
  #geom_vline(xintercept = as.numeric(as.Date("2012-05-06"))) +
  geom_point(data = mpolls.pred, size = 2.5, alpha = .7,
             aes(date, value, group = candidate, color = candidate )) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = .2)+
  scale_y_continuous(labels = percent) +
  ylab("") + xlab("date") +
  scale_color_manual("candidate",
                     values = c("#ffd217", "red", "lightblue", "#00448b"),
                     labels = c("EPN", "AMLO", "JVM", "GQT"),
                     breaks = c("epn", "amlo", "jvm", "gqt")) +
  geom_point(data = data.frame(res = final.result,
               date = as.Date("2012-07-01"),
               candidate = "Cat"),
               aes(date, res, shape = "Official Count")) +
  facet_wrap(~pollster) +
  opts(title = "Net Voting Intention with Trendline (Kalman Filter), by Polling Firm")+
  scale_shape_manual("", values = c(15)) +
  scale_x_date(breaks = date_breaks("months"),
               labels = date_format("%b"))
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
  opts(title = "Vázquez Mota Voting Intention with Trendline (Kalman Filter with House Effects)")+
  geom_point(data = data.frame(res = final.result[3],
               date = as.Date("2012-07-01"),
               candidate = "Cat"),
               aes(date, res, shape = "Official Count")) +
  scale_shape_manual("", values = c(15))
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
  opts(title = "Quadri Voting Intention with Trendline (Kalman Filter) - 2012 Mexican Presidential Election")+
  geom_point(data = data.frame(res = final.result[4],
               date = as.Date("2012-07-01"),
               candidate = "Cat"),
               aes(date, res, shape = "Official Count")) +
  scale_shape_manual("", values = c(15))
ggsave(file.path("graphs","quadri.svg"), dpi = 100, w = 9.6, h=5)
