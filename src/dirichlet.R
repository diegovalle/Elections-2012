
polls$debate <- FALSE
polls$debate[polls$date > as.Date("2012-05-06") &  polls$date <= as.Date("2012-05-21")] <- TRUE
drpolls <- DR_data(polls[, c("epn", "jvm", "amlo", "gqt")])
x <- as.data.frame(model.matrix(~pollster-1, data = polls))
names(x) <- str_c("a", 1:13)
polls2 <- cbind(polls, x)
#f <- as.formula(str_c("drpolls~ ns(date, 3) + ", str_c("a", 1:13)))
poll.fit <- DirichReg(drpolls~ ns(date, 3) +a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13, polls2)
##confint(poll.fit)
Xnew <- data.frame(date= seq(min(polls$date), max(polls$date),
                             by = "day"),
#                   debate = FALSE,
                   a1 = 0, a2 =0, a3 = 1, a4 = 1, a5 =0,
                   a6 =0, a7 = 1, a8=0, a9=0, a10=0,
                   a11=0, a12=0, a13=0)
pred <- cbind(predict(poll.fit, Xnew), as.data.frame(Xnew[,1]))
names(pred) <- c("epn", "jvm", "amlo", "gqt", "date")
write.csv(pred, "chart/dirichlet.csv", row.names = FALSE)


mpred <- melt(pred, id = "date")

ggplot(mpred, aes(date, value, group = variable, color = variable)) +
  geom_line(size = 1.2)+
  scale_x_date(limits = c(as.Date("2012-03-15"), max(polls$date))) +
  geom_point(data = mpolls, size = 2, alpha = .7,
             aes(date, value, group = variable, color = variable))+
  ylab("") +
  scale_y_continuous(labels = percent, limits = c(0,.60))+
  scale_color_manual("candidate",
                                  values = c("red", "#00448b", "#ffd217", "lightblue"),
                                  labels = c("EPN", "JVM", "AMLO", "GQT")) +
  opts(title = "Voting Intention with Trendline - 2012 Mexican Presidential Election")
ggsave(file.path("graphs","dirichletreg.svg"), dpi = 100, w = 9, h=6)


mpolls.pred <- merge(mpolls, mpred, by = c("date", "variable"))
mpolls.pred <- ddply(mpolls.pred, .(pollster), transform,
      order = mean((value.x - value.y)^2))
mpolls.pred$pollster <- reorder(mpolls.pred$pollster, mpolls.pred$order)
ggplot(mpolls.pred, size = 2,
       aes(date, value.x, group = variable, color = variable)) +
  geom_line(data = mpred, 
            aes(date, value, group = variable, color = variable),
            size = 1.2)+
  scale_x_date(limits = c(as.Date("2012-03-15"), max(polls$date))) +
  facet_wrap(~ pollster) +
  geom_point(alpha = .7)+
  ylab("") +
  scale_y_continuous(labels = percent)+
  scale_color_manual("candidate",
                                  values = c("red", "#00448b", "#ffd217", "lightblue"),
                                  labels = c("EPN", "JVM", "AMLO", "GQT")) +
 opts(title = "Voting Intention with Trendline, by Pollster (ordered by difference from trend)",
      axis.text.x  = theme_text(angle=60))
ggsave(file.path("graphs","dirichletreg-pollsters.svg"), dpi = 100, w = 9.6, h=6)



polls.gross <- na.omit(polls)
drpolls <- DR_data(polls.gross[, c("epn.gross", "jvm.gross",
                             "amlo.gross", "gqt.gross",
                             "no.answer")])
poll.fit <- DirichReg(drpolls~ ns(date, 3), polls.gross)
Xnew <- data.frame(date= seq(min(polls.gross$date), max(polls.gross$date),
                             by = "day"))
pred <- cbind(predict(poll.fit, Xnew), Xnew)
names(pred) <- c("epn.gross", "jvm.gross",
                 "amlo.gross", "gqt.gross", "no.answer", "date")

mpred <- melt(pred, id = "date")

ggplot(mpred, aes(date, value, group = variable, color = variable)) +
  geom_line(size = 1.2)+
  scale_x_date(limits = c(as.Date("2012-03-15"), max(polls$date))) +
  geom_point(data = mpolls.gross, size = 2, alpha = .7,
             aes(date, value, group = variable, color = variable))+
  ylab("") +
  scale_y_continuous(labels = percent, limits = c(0,.60))+
  scale_color_manual("candidate",
                                  values = c("red", "#00448b",
                                    "#ffd217", "lightblue", "gray"),
                                  labels = c("EPN", "JVM", "AMLO", "GQT", "No answer")) +
  opts(title = "Raw Voting Intention with Trendline - 2012 Mexican Presidential Election")
ggsave(file.path("graphs","dirichletreg-raw.svg"), dpi = 100, w = 9, h=6)


mpolls.pred <- merge(mpolls.gross, mpred, by = c("date", "variable"))
mpolls.pred <- ddply(mpolls.pred, .(pollster), transform,
      order = mean((value.x - value.y)^2))
mpolls.pred$pollster <- reorder(mpolls.pred$pollster, mpolls.pred$order)
ggplot(mpolls.pred, size = 2,
       aes(date, value.x, group = variable, color = variable)) +
  geom_line(data = mpred, 
            aes(date, value, group = variable, color = variable),
            size = 1.2)+
  scale_x_date(limits = c(as.Date("2012-03-15"), max(polls$date))) +
  facet_wrap(~ pollster) +
  geom_point(alpha = .7)+
  ylab("") +
  scale_y_continuous(labels = percent)+
  scale_color_manual("candidate",
                                  values = c("red", "#00448b", "#ffd217", "lightblue", "gray"),
                                  labels = c("EPN", "JVM", "AMLO", "GQT", "No answer")) +
 opts(title = "Voting Intention with Trendline, by Pollster (ordered by difference from trend)",
      axis.text.x  = theme_text(angle=60))
ggsave(file.path("graphs","dirichletreg-pollsters-raw.svg"), dpi = 100, w = 9.6, h=6)

# vcov(poll.fit)
# 
# mu <- c(coef(poll.fit)[[1]],coef(poll.fit)[[2]],coef(poll.fit)[[3]],coef(poll.fit)[[4]])
