gammReg <- function(var, polls){
  x <- as.data.frame(model.matrix(~pollster-1, data = polls))
  names(x) <- str_c("a", 1:13)
  polls2 <- cbind(polls, x)
  ##f <- as.formula(str_c("drpolls~ ns(date, 3) + ", str_c("a", 1:13)))
##  poll.fit <- DirichReg(drpolls~ bs(date, 3)+a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13,
  ##                      polls2)
  formula <- as.formula(str_c(var, " ~ s(as.numeric(date), k = 10)",
                              "+ pollster"))
                              #"+a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13"))
  m <- gamm(formula,
            data = polls)#,
            #correlation = corARMA(p = 1))
  return(m)
}

glmReg <- function(var, polls){
  formula <- as.formula(str_c(var, " ~ bs(as.numeric(date))"))
  m <- glm(formula, data = polls)#,
            #correlation = corARMA(p = 1))
  return(m)
}


simulate <- function(model, polls){
  ##get variance estimates for quantities derived from the model
  Xnew <- data.frame(date= as.numeric(seq(min(polls$date), max(polls$date),
                                          by = "day")))
  ##get variance of sum of predictions using lpmatrix
  randCoef <- rmnorm(n = nSimulations,
                  mean = coef(model$gam),
                  varcov = vcov(model$gam))
  Xp <- predict(model$gam, newdata = Xnew, type = "lpmatrix")
  sim <- Xp %*% t(randCoef)
  return(sim)
}


glmSimulate <- function(model, polls){
  ##get variance estimates for quantities derived from the model
  Xnew <- data.frame(date= as.numeric(seq(min(polls$date), max(polls$date),
                                          by = "day")))
  ##get variance of sum of predictions using lpmatrix
  randCoef <- mvrnorm(n = nSimulations,
                  coef(model),
                  vcov(model))
  Xp <- predict(model, newdata = Xnew, type = "terms")
  sim <- Xp %*% t(randCoef)
  return(sim)
}


predElection <- function(sim, days.to.election, forecast.fun = ets){
  if(ncol(sim) > nSimulations)
    sim <- sim[,sample(1:ncol(sim), nSimulations)]
  message("forecasting each simulation...")
  ##create progress bar
  pb <- txtProgressBar(min = 1, max = ncol(sim), style = 3)
  r <- foreach(i = 1:ncol(sim)) %dopar% {
    prediction <- forecast(forecast.fun(ts(log1p(sim[,i]))),
                           h = days.to.election)
    ##update progress bar
    setTxtProgressBar(pb, i)
    return(expm1(prediction$mean))
  }
  ##print(sim[,ncol(sim)])
  ##print(nrow(sim))
  close(pb)
  return(r)
}


getMean <- function(vec, days.to.election) {
  last <- c()
  for(i in 1:length(vec)) {
    last[i] <- vec[[i]][days.to.election]
  }
  mean(last)
}

getMeanVec <- function(vec, days.to.election) {
  meanvec <- c()
  last <- c()
  for(i in 1:days.to.election){
    for(j in 1:length(vec)) {
      last[j] <- vec[[j]][i]
    }
    meanvec[i] <- mean(last)
  }
  meanvec
}

getElectionVal <- function(vec, days.to.election) {
  last <- c()
  for(i in 1:length(vec)) {
    last[i] <- vec[[i]][days.to.election]
  }
  last
}

getQuantile <- function(vec, days.to.election, q1 = .025, q2 = .975) {
  last <- c()
  for(i in 1:length(vec)) {
    last[i] <- vec[[i]][days.to.election]
  }
  quantile(last, c(q1, q2))
}

getQuantileVec <- function(vec, days.to.election, q1 = .025, q2 = .975) {
  meanvec <- data.frame(q1 = numeric(days.to.election),
                        q2 = numeric(days.to.election))
  last <- c()
  for(i in 1:days.to.election){
    for(j in 1:length(vec)) {
      last[j] <- vec[[j]][i]
    }
    meanvec$q1[i] <- quantile(last, c(q1, q2))[1]
    meanvec$q2[i] <- quantile(last, c(q1, q2))[2]
  }
  return(meanvec)
}
