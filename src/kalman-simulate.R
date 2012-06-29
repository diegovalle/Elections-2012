########################################################
# Author: Diego Valle-Jones
# Website: www.diegovalle.net
# Date Created: Wed Jun 20 18:50:02 2012
# Email: diegovalle at gmail.com
# Purpose: Forecast the 2012 election
# Copyright (c) Diego Valle-Jones. All rights reserved




#Simulate the election for each candidate from the posterior distribution of
#voter support
pena <- predElection(t(sim.epn$alpha), days.to.election, auto.arima)
amlo <- predElection(t(sim.amlo$alpha), days.to.election, auto.arima)
vaz <- predElection(t(sim.jvm$alpha), days.to.election, auto.arima)
quadri <- predElection(t(sim.gqt$alpha), days.to.election, auto.arima)


#Get the mean of the simulation for each candidate on election day
candidates <- c(epn = getMean(pena, days.to.election),
                jvm = getMean(vaz, days.to.election),
                amlo = getMean(amlo, days.to.election), 
                quadri = getMean(quadri, days.to.election))
message("Vote Forecast:")
print(sapply(candidates, function(x) x/ sum(candidates)))
sum(sapply(candidates, function(x) x/ (sum(candidates))))
sum(candidates)

#Get the forecasts of each simulation for each candidate on election day
pred4 <- data.frame(epn = getElectionVal(pena, days.to.election),
                    jvm = getElectionVal(vaz, days.to.election), 
                    amlo = getElectionVal(amlo, days.to.election), 
                    gqt = getElectionVal(quadri, days.to.election))
message("Percentage of simulations won by:")
print(prop.table(table(apply(pred4, 1, function(x) which(x == max(x))))))
message("Close Race:")
print(sum(apply(pred4, 1, function(x) x[order(-x)][1]-x[order(-x)][2]) < .01) / nSimulations)
sum(apply(pred4, 1, function(x) x[order(-x)][1]-x[order(-x)][2]) < .03) / nSimulations
message("AMLO to beat JVM:")
print(sum(apply(pred4, 1, function(x) x[3] > x[2])) / nSimulations)
message("Panal to retain registration:")
print(sum(pred4$gqt > .02) / nSimulations)
message("Peña to obtain mayority congress:")
print(sum(pred4$epn > .422) / nSimulations)


#Confidence intervals for the vote each candidate will receive on election day
message("Peña range:")
print(getQuantile(pena, days.to.election))
message("AMLO range:")
print(getQuantile(amlo, days.to.election))
message("JVM range:")
print(getQuantile(vaz, days.to.election))
message("Quadri range:")
print(getQuantile(quadri, days.to.election))


#Create confidence intervals from all the simulations
Xnew <- data.frame(date= as.numeric(seq(min(polls$date), max(polls$date),
                                        by = "day")))
mean.prediction <- data.frame(epn = getMeanVec(pena, days.to.election),
                jvm = getMeanVec(vaz, days.to.election),
                amlo = getMeanVec(amlo, days.to.election), 
                gqt = getMeanVec(quadri, days.to.election))
mean.prediction$date <- seq(max(polls$date)+1, kelection.day+1,
                        by = "day")

#For d3.js the confidence intervals and mean

quants <- data.frame(epn = getQuantile(pena, days.to.election),
                     jvm = getQuantile(vaz, days.to.election),
                     amlo = getQuantile(amlo, days.to.election),
                     gqt = getQuantile(quadri, days.to.election))
quants <- rbind(quants, mean.prediction[nrow(mean.prediction), 1:4])
normalized <- sapply(quants[3,], function(x) x/ (sum(quants[3,])))
quants[1,] <- quants[1,] + normalized - quants[3,]
quants[2,] <- quants[2,] + quants[3,] - normalized
quants[3,] <- normalized
test_that(sum(quants[3,]), equals(1))


mmean.prediction <- melt(mean.prediction,id = "date")


mean.intervals <- rbind(
      getQuantileVec(pena, days.to.election),
      getQuantileVec(vaz, days.to.election),
      getQuantileVec(amlo, days.to.election),
      getQuantileVec(quadri, days.to.election))
mean.intervals$date <- seq(max(polls$date)+1, kelection.day+1,
                        by = "day")
mean.intervals$variable <- rep(c(
                                  "epn",
                                  "jvm",
                                  "amlo",
                                  "gqt"),
                               each = days.to.election)
mean.intervals$value <- 0
mean.intervals$bar <- 0



ggplot(alpha, aes(date, bar, group = candidate)) +
  geom_line(aes(color = candidate), size = 1.2) +
   scale_x_date(limits = c(as.Date("2012-03-15"), as.Date("2012-07-02"))) +
   geom_point(data = mpolls, size = 2, alpha = .7,
              aes(date, value, group = variable, color = variable), alpha = .7) +
   geom_line(data = mmean.prediction, size = 1.2, linetype = 3,
             aes(date, value, group = variable, color = variable), alpha = 1,
             show_guide = FALSE) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              alpha = .2)+
  geom_ribbon(data = mean.intervals,
              aes(ymin = q1, ymax= q2, group = variable),
              color = "transparent", alpha = .2) +
  scale_color_manual("candidate",
                     values = c("#ffd217", "red", "lightblue", "#00448b"),
                     labels = c("EPN", "AMLO", "JVM", "GQT"),
                     breaks = c("epn", "amlo", "jvm", "gqt")) +
   opts(title = "Simulated 2012 Mexican Presidential Race Outcomes") +
   ylab("") +
   scale_y_continuous(labels = percent)
ggsave(file.path("graphs","simulation-kalman2.svg"), dpi = 100, w = 6, h=7)



write.csv(dcast(formula =  date ~ candidate,
      data = alpha[,c("date", "bar", "candidate")],
      value.var = "bar"), "clean-data/kalman.csv", row.names = FALSE)

##toJSON(list(dcast(formula =  date ~ candidate,
  ##    data = alpha[,c("date", "bar", "candidate")],
    ##  value.var = "bar")))


formatJSONDate <- function(date) {
   format(date, "%a, %d %b %Y, %H:%M:%S")
}

head(alpha)
temp <- dcast(formula =  date ~ candidate,
      data = alpha[,c("date", "bar", "candidate")],
      value.var = "bar")
temp$date <- formatJSONDate(temp$date)

temppolls <- polls
temppolls$date <- formatJSONDate(temppolls$date)
temppolls$start <- formatJSONDate(temppolls$start)
temppolls$end <- formatJSONDate(temppolls$end)
temppolls$pollster <- as.character(temppolls$pollster)
head(polls)

t.mean.prediction <- mean.prediction
t.mean.prediction$date <- formatJSONDate(t.mean.prediction$date)

sink("clean-data/json.txt")
cat(toJSON(list(kalman = temp,
                polls = temppolls[!is.na(temppolls$epn),],
                forecast = t.mean.prediction,
                quants = quants)))
sink()
