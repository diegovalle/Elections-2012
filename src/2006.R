
six <- read.csv("data/polls-2006.csv")

head(six)
six$Fuente <- NULL
six <- transform(six, calderon = calderon / 100)
six <- transform(six, madrazo = madrazo / 100)
six <- transform(six, amlo = amlo / 100)
six$other <- 1 - apply(six[,3:5], 1, sum) 
six <- six[,c(1:5,10)]
six$Date <- as.Date(six$Date, format = "%d de %B de %Y")
#six <- subset(six, Date >= as.Date("2006-03-15"))
##six$Publisher <- factor(levels(six$Publisher))
#six <-  subset(six, Date >= as.Date("2006-03-15"))
drpolls6 <- DR_data(six[,3:6])

x <- as.data.frame(model.matrix(~ Publisher - 1, data = six))
names(x) <- str_c("a", 1:14)
six2 <- cbind(six, x)
#f <- as.formula(str_c("drpolls~ ns(date, 3) + ", str_c("a", 1:13)))
poll.fit6 <- DirichReg(drpolls6~ bs(Date, 3) +a1+a2+a3+a4+a5+a6+a7+a8+a9+a10+a11+a12+a13+a14, six2)




 poll.fit6 <- DirichReg(drpolls6~ ns(Date, 3), six)
 Xnew6 <- data.frame(Date= seq(min(six$Date), max(six$Date),
                               by = "day"))
 pred6 <- cbind(predict(poll.fit6, Xnew6), Xnew6)



## Xnew6 <- data.frame(Date= seq(min(polls$date), max(polls$date),
##                              by = "day"),
##                    a1 = 0, a2 =0, a3 = 1, a4 = 1, a5 =0,
##                    a6 =0, a7 = 1, a8=0, a9=0, a10=0,
##                    a11=0, a12=0, a13=0, a14 =0,
##                    a15 = 0, a16 =0, a17 =0,
##                    a18=0, a19=0)
## pred6 <- cbind(predict(poll.fit6, Xnew6), as.data.frame(Xnew6[,1]))

names(pred6) <- c("calderon","madrazo", "amlo", "other", "Date")

mpred6 <- melt(pred6, id = "Date")

ggplot(melt(six, id = c("Date", "Publisher")), 
       aes(Date, value, group = variable, color = variable)) +
  geom_point(size = 2.6) +
  geom_smooth(method = loess)

ggplot(mpred6, aes(Date, value, group = variable, color = variable)) +
  geom_line(size = 1.2)+
  scale_x_date(limits = c(as.Date("2006-03-15"), max(six$Date))) +
  geom_point(data = melt(six, id = c("Date", "Publisher")), size = 2, alpha = .7,
             aes(Date, value, group = variable, color = variable))+
  ylab("") +
  scale_y_continuous(labels = percent) +
  facet_wrap(~ Publisher)


final.2006 <- c(calderon = .3695, madrazo = .2291, amlo = .3635, other = .037711)
final.polls <- ddply(subset(six, Date >= as.Date("2006-06-13")),
        .(Publisher),
        function(df) ((head(df, 1)[3:6] - t(final.2006))))

final.polls$euclidian <- apply(final.polls[2:5], 1, function(x) sqrt(sum(x^2)))
arrange(final.polls, euclidian)
confint(poll.fit6)
