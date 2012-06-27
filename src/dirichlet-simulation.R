polls <- polls[!is.na(polls$epn),]
polls <- arrange(polls, date)
drpolls <- DR_data(polls[, c("epn", "jvm", "amlo", "gqt")])
rand.polls <- c()
for(i in nrow(polls):1) {
  t <- rdirichlet(nSimulations, unlist(drpolls$Y[i,] * polls$size[i] ))
  rand.polls <- rbind(rand.polls, matrix(t(t), ncol = nSimulations*4, nrow = 1))
}

#head(t)
#head(matrix(t, ncol = 4000, nrow = 1))
#tail(rand.polls, 1)

#dr.list <- list()

sim.dr <- data.frame(date = seq(min(polls$date), max(polls$date),
                                by = "day"))
##create progress bar
pb <- txtProgressBar(min = 1, max = ncol(rand.polls), style = 3)
message("simulating dirichlet regressions:")
for(i in seq(1, ncol(rand.polls), by = 4)) {
  drdata <- DR_data(rand.polls[, i:(i+3)])
  date1 <- rev(polls$date)
  poll.fit <- DirichReg(drdata ~ ns(date1, 3))
  
  Xnew <- data.frame(date1 = seq(min(polls$date), max(polls$date),
                                 by = "day"))
  pred1 <- predict(poll.fit, Xnew)
  ##update progress bar
  setTxtProgressBar(pb, i)
    
  #names(pred) <- c("epn", "jvm", "amlo", "gqt")
  sim.dr <- cbind(sim.dr, pred1)
}
close(pb)



##create progress bar
pb <- txtProgressBar(min = 1, max = ncol(rand.polls), style = 3)
message("simulating dirichlet regressions:")
for(i in seq(1, ncol(rand.polls), by = 4)) {

 
  drpolls <- DR_data(rand.polls[, i:(i+3)])
  x <- as.data.frame(model.matrix(~pollster-1, data = polls))
  names(x) <- str_c("a", 1:13)
  polls2 <- cbind(polls, x)

  date1 <- rev(polls$date)
#f <- as.formula(str_c("drpolls~ ns(date, 3) + ", str_c("a", 1:13)))
  poll.fit <- DirichReg(drpolls~ bs(date1, 3) +a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13, polls2)
##  confint(poll.fit, exp=TRUE)
  Xnew <- data.frame(date1= seq(min(polls$date), max(polls$date),
                       by = "day"),
                     #debate = FALSE,
                     a1 = 0, a2 =0, a3 = 1, a4 = 1, a5 =0,
                     a6 =0, a7 = 1, a8=0, a9=0, a10=0,
                     a11=0, a12=0, a13=0)
  pred1 <- cbind(predict(poll.fit, Xnew))
  ##update progress bar
  setTxtProgressBar(pb, i)
    
  #names(pred) <- c("epn", "jvm", "amlo", "gqt")
  sim.dr <- cbind(sim.dr, pred1)
}
close(pb)



##create progress bar
## message("simulating dirichlet regressions:")
## pb <- txtProgressBar(min = 1, max = ncol(rand.polls), style = 3)  
## r <- foreach(i = seq(1, ncol(rand.polls), by = 4),
##              .combine = cbind) %dopar% {
##   drdata <- DR_data(rand.polls[, i:(i+3)])
##   date1 <- rev(polls$date)
##   poll.fit <- DirichReg(drdata ~ bs(date1, 4))
  
##   Xnew <- data.frame(date1 = seq(min(polls$date), max(polls$date),
##                                  by = "day"))
##   pred.sim <- predict(poll.fit, Xnew)
##   ##update progress bar
##   setTxtProgressBar(pb, i)
##   return(pred.sim)
## }
## close(pb)
## head(r)
## head(sim.dr)
## all.equal(r, sim.dr)
##sim.dr <- sim.dr[,-seq(6, 501, by = 5)]

names(sim.dr) <- c("date", 1:(nSimulations * 4))
msim.dr2 <- melt(sim.dr, id = "date")
msim.dr2$group <- rep(c("epn", "jvm", "amlo", "gqt"), 
                      each = as.numeric(max(polls$date) - min(polls$date))+1)
msim.dr2$variable <- rep(1:(nSimulations * 4), 
                      each = as.numeric(max(polls$date) - min(polls$date))+1) 
p <- ggplot(msim.dr2, aes(date, value, group = variable, color = group)) +
  geom_line(alpha = .06, size = .5)
ggsave("graphs/dirichlet-sim-check.png", plot = p,
       dpi = 100, w = 9, h = 6)

sim.dr$date <- NULL
pena <- predElection(sim.dr[,seq(1, (ncol(rand.polls)-4), by = 4)], days.to.election)
amlo <- predElection(sim.dr[,seq(3, (ncol(rand.polls)-4), by = 4)], days.to.election)
vaz <- predElection(sim.dr[,seq(2, (ncol(rand.polls)-4), by = 4)], days.to.election)
quadri <- predElection(sim.dr[,seq(4, (ncol(rand.polls)-4), by = 4)], days.to.election)


candidates <- c(epn = getMean(pena, days.to.election),
                jvm = getMean(vaz, days.to.election),
                amlo = getMean(amlo, days.to.election), 
                quadri = getMean(quadri, days.to.election))
message("Vote Forecast:")
print(sapply(candidates, function(x) x/ sum(candidates)))
sapply(candidates, function(x) x/ (sum(candidates) ))
sum(candidates)


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
print(sum(apply(pred4, 1, function(x) x[2] > x[3])) / nSimulations)
message("Panal to retain registration:")
print(sum(pred4$gqt > .02) / nSimulations)
message("Peña to obtain mayority congress:")
print(sum(pred4$epn > .422) / nSimulations)


getQuantile(pena, days.to.election)
getQuantile(amlo, days.to.election)
getQuantile(vaz, days.to.election)
getQuantile(quadri, days.to.election)

Xnew <- data.frame(date= as.numeric(seq(min(polls$date), max(polls$date),
                                        by = "day")))

mean.prediction <- data.frame(epn = getMeanVec(pena, days.to.election),
                jvm = getMeanVec(vaz, days.to.election),
                amlo = getMeanVec(amlo, days.to.election), 
                gqt = getMeanVec(quadri, days.to.election))
mean.prediction$date <- seq(max(polls$date)+1, kelection.day,
                        by = "day")

mmean.prediction <- melt(mean.prediction,id = "date")


mean.intervals <- rbind(
      getQuantileVec(pena, days.to.election),
      getQuantileVec(vaz, days.to.election),
      getQuantileVec(amlo, days.to.election),
      getQuantileVec(quadri, days.to.election))
#names(mean.intervals) <- 
mean.intervals$date <- seq(max(polls$date)+1, kelection.day,
                        by = "day")
mean.intervals$variable <- rep(c(
                                  "epn",
                                  "jvm",
                                  "amlo",
                                  "gqt"),
                               each = days.to.election)
mean.intervals$value <- 0



drpolls <- DR_data(polls[, c("epn", "jvm", "amlo", "gqt")])
poll.fit <- DirichReg(drpolls~ ns(date, 3), polls)
Xnew <- data.frame(date= seq(min(polls$date), max(polls$date),
                             by = "day"))
pred <- cbind(predict(poll.fit, Xnew), Xnew)
names(pred) <- c("epn", "jvm", "amlo", "gqt", "date")

mpred <- melt(pred, id = "date")


ggplot(mpred, aes(date, value, 
                                         group = variable, color = variable)) +
   geom_line(size = 1.2)+
   scale_x_date(limits = c(as.Date("2012-03-15"), as.Date("2012-07-02"))) +
   geom_point(data = mpolls,
              aes(date, value, group = variable, color = variable), alpha = .7) +
   geom_line(data = mmean.prediction, size = 1.2, 
             aes(date, value, group = variable, color = variable), alpha = 1,
             show_guide = FALSE) +
  geom_ribbon(data = mean.intervals,
              aes(ymin = q1, ymax= q2, group = variable),
              fill = "gray", color = "transparent", alpha = .3) +
   scale_color_manual("candidate",
                      values = c("red", "#00448b", "#ffd217", "lightblue"),
                      labels = c("EPN","AMLO","JVM",  "GQT"),
                      breaks = c("epn", "amlo", "jvm", "gqt")) +
   opts(title = "Simulated 2012 Mexican Presidential Race Outcomes") +
   ylab("") +
   scale_y_continuous(labels = percent)
ggsave(file.path("graphs","simulation-dirichlet2.svg"), dpi = 100, w = 9, h=6)







## samp <- 1:nSimulations         #sample(1:nSimulations, 100)
## forecast.df <- cbind(as.data.frame(pena), 
##                      as.data.frame(vaz),
##                      as.data.frame(amlo),
##                      as.data.frame(quadri))
## forecast.df$date <- seq(max(polls$date)+1, kelection.day,
##                         by = "day")
## forecast.df <- melt(forecast.df, id = "date")
## forecast.df$variable <- rep(c("epn", "jvm", "amlo", "gqt"), each = 28) 
## forecast.df$group <- rep(c(1:(nSimulations-1)), each = days.to.election)
## forecast.df$group <- str_c(forecast.df$variable, forecast.df$group)







## p <- ggplot(mpred, aes(date, value, group = variable, color = variable)) +
##   geom_line(size = 1.2)+
##   scale_x_date(limits = c(as.Date("2012-03-15"), max(polls$date)+days.to.election)) +
##   geom_point(data = mpolls, size = 2, alpha = .7,
##              aes(date, value, group = variable, color = variable))+
##   ylab("") +
##   scale_y_continuous(labels = percent, limits = c(-0.04,.60))+
##   scale_color_manual("candidate",
##                      values = c("red", "#00448b", "#ffd217", "lightblue"),
##                      labels = c("EPN", "JVM", "AMLO", "GQT")) +
##   opts(title = "Voting Intention with Trendline (Dirichlet Regression) - 2012 Mexican Presidential Election") +
##   geom_line(data = forecast.df, size = .5, 
##             aes(date, value, group = group, color = variable), alpha = .05,
##             show_guide = FALSE)
## ggsave("graphs/dirichlet-sim.svg", plot = p,
##        dpi = 100, w = 9, h = 6)
