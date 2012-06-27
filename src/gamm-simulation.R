
#Compare three models to see which one is best
m1 <- gamm(amlo ~ s(as.numeric(date), k = 10),
           random = list(pollster = ~ 1 | pollster ),
           data = polls)
m2 <- gamm(amlo ~ s(as.numeric(date), k = 10), data = polls)
m3 <- gamm(amlo ~ s(as.numeric(date)), data = polls,
           correlation = corARMA(p = 1))
#plot(m2$gam, residuals = TRUE, pch = 19, cex = 0.75)
anova(m1$lme, m2$lme, m3$lme)
x <- as.data.frame(model.matrix(~pollster-1, data = polls))
x <- contrasts(polls$pollster)
x <- C(polls$pollster, cont.dev)
  names(x) <- str_c("a", 1:13)
  polls2 <- cbind(polls, x)
  ##f <- as.formula(str_c("drpolls~ ns(date, 3) + ", str_c("a", 1:13)))
##  poll.fit <- DirichReg(drpolls~ bs(date, 3)+a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13,
  ##                      polls2)
  formula.gamm <- as.formula(str_c("epn", " ~ s(as.numeric(date), k = 5)",
                              "+a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13"))
  m <- gamm(formula.gamm,
            data = polls2)#,
            #correlation = corARMA(p = 1))
  sim(lm(epn ~ date, data = polls))
## m1$gam$Vp
## m1 <- glm(epn ~ bs(as.numeric(date), 3), data = polls)
## Rbeta <- rmnorm(n = nSimulations, mean = coef(m1$gam), varcov = vcov(m1$gam))
## Rbeta <- mvrnorm(n = nSimulations, coef(m1), vcov(m1))
## rmvn
##   #predict(m2$gam, newdata = Xnew)
##   Xp <- predict(m1$gam, newdata = Xnew, type = "lpmatrix")
##   #predict(m2$gam, newdata = Xnew)
##   sim <- Xp %*% t(Rbeta)
##   return(sim)



mpena<-gammReg("epn", polls)
simpena <- simulate(mpena, polls)
mamlo<-gammReg("amlo", polls)
simamlo <- simulate(mamlo, polls)
mvaz<-gammReg("jvm", polls)
simvaz <- simulate(mvaz, polls)
mquadri<-gammReg("gqt", polls)
simquadri <- simulate(mquadri, polls)


#mpena<-glmReg("epn", polls)
#simpena <- glmSimulate(mpena, polls)



if(sum(file.exists(str_c("cache/", c("epn.rdata",
                                     "amlo.rdata",
                                     "jvm.rdata",
                                     "gqt.rdata"))))){
  pena <- predElection(simpena, days.to.election)
  save(pena, file = "cache/epn.rdata")
  amlo <- predElection(simamlo, days.to.election)
  save(amlo, file = "cache/amlo.rdata")
  vaz <- predElection(simvaz, days.to.election)
  save(vaz, file = "cache/jvm.rdata")
  quadri <- predElection(simquadri, days.to.election)
  save(quadri, file = "cache/gqt.rdata")
} else {
  load("cache/epn.rdata")
  load("cache/amlo.rdata")
  load("cache/jvm.rdata")
  load("cache/gqt.rdata")
}


getQuantile(pena, days.to.election)
getQuantile(amlo, days.to.election)
getQuantile(vaz, days.to.election)
getQuantile(quadri, days.to.election)

candidates <- c(epn = getMean(pena, days.to.election),
                amlo = getMean(amlo, days.to.election), 
                jvm = getMean(vaz, days.to.election),
                quadri = getMean(quadri, days.to.election))
message("Vote Forecast:")
print(sapply(candidates, function(x) x/sum(candidates)))

pred4 <- data.frame(epn = getElectionVal(pena, days.to.election), 
                    amlo = getElectionVal(amlo, days.to.election),
                    jvm = getElectionVal(vaz, days.to.election), 
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

Xnew <- data.frame(date= as.numeric(seq(min(polls$date), max(polls$date),
                                        by = "day")))

mean.prediction <- data.frame(epn = getMeanVec(pena, days.to.election),
                amlo = getMeanVec(amlo, days.to.election), 
                jvm = getMeanVec(vaz, days.to.election),
                gqt = getMeanVec(quadri, days.to.election))
mean.prediction$date <- seq(max(polls$date)+1, kelection.day,
                        by = "day")

mmean.prediction <- melt(mean.prediction,id = "date")


mean.intervals <- rbind(
      getQuantileVec(pena, days.to.election),
      getQuantileVec(amlo, days.to.election),
      getQuantileVec(vaz, days.to.election),
      getQuantileVec(quadri, days.to.election))

mean.intervals$date <- seq(max(polls$date)+1, kelection.day,
                        by = "day")
mean.intervals$variable <- rep(c(
                                  "epn",
                                  "amlo",
                                  "jvm",
                                  "gqt"),
                               each = days.to.election)
mean.intervals$value <- 0



pred.gamm <- data.frame(date = Xnew,
                        epn = predict(mpena$gam, newdata = Xnew),
                        amlo = predict(mamlo$gam, newdata = Xnew),
                        jvm = predict(mvaz$gam, newdata = Xnew),
                        gqt = predict(mquadri$gam, newdata = Xnew))



ggplot(melt(pred.gamm, id = "date"), aes(date, value, 
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
                      values = c("red", "#ffd217", "#00448b", "lightblue"),
                      labels = c("EPN","AMLO","JVM",  "GQT"),
                      breaks = c("epn", "jvm", "amlo", "gqt")) +
   opts(title = "Simulated 2012 Mexican Presidential Race Outcomes") +
   ylab("") +
   scale_y_continuous(labels = percent)
ggsave(file.path("graphs","simulation-gamm2.svg"), dpi = 100, w = 9, h=6)


## samp <- 1:nSimulations         #sample(1:nSimulations, 100)
## forecast.df <- cbind(as.data.frame(pena)[,samp], 
##                      as.data.frame(vaz)[,samp],
##                      as.data.frame(amlo)[,samp],
##                      as.data.frame(quadri)[,samp])
## forecast.df$date <- seq(max(polls$date)+1, kelection.day,
##                         by = "day")
## forecast.df <- melt(forecast.df, id = "date")
## forecast.df$variable <- rep(c("epn", "jvm", "amlo", "gqt"), each = length(samp)* days.to.election) 
## forecast.df$group <- rep(c(1:length(samp)), each = days.to.election)
## forecast.df$group <- str_c(forecast.df$variable, forecast.df$group)



## ggplot(melt(pred.gamm, id = "date"), aes(date, value, 
##                                          group = variable, color = variable)) +
##    geom_line(size = 1.2)+
##    scale_x_date(limits = c(as.Date("2012-03-15"), as.Date("2012-07-02"))) +
##    geom_point(data = mpolls,
##               aes(date, value, group = variable, color = variable)) +
##    geom_line(data = forecast.df, size = .5, 
##              aes(date, value, group = group, color = variable), alpha = .05,
##              show_guide = FALSE)+
##    scale_color_manual("candidate",
##                       values = c("red", "#ffd217", "#00448b", "lightblue"),
##                       labels = c("EPN","AMLO","JVM",  "GQT"),
##                       breaks = c("epn", "jvm", "amlo", "gqt")) +
##    opts(title = "Simulated 2012 Mexican Presidential Race Outcomes") +
##    ylab("") +
##    scale_y_continuous(labels = percent)
## ggsave(file.path("graphs","simulation.svg"), dpi = 100, w = 9, h=6)
