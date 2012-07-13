mpolls2 <- ddply(mpolls.gross, .(variable), transform, value = scale(value))
ggplot(subset(mpolls2, !pollster %in% c("Milenio-GEA ISA", "GEA-ISA")),         
       aes(no.answer, value, group = variable, color = pollster)) +
         geom_point() +
         geom_smooth(method = lm) +
         facet_wrap(~ variable)


fit <- lm(value ~ no.answer+variable, 
          mpolls.gross)
summary(fit)
coefplot(fit)

mpolls.net <- melt(polls[, c("pollster",
                            "date",
                            "epn",
                            "jvm",
                            "amlo",
                            "gqt",
                            "no.answer")],
                  id = c("date", "pollster", "no.answer"))
##mpolls.net <- ddply(mpolls.net, .(variable), transform, value = scale(value))
mpolls.net <- subset(mpolls.net, date >= as.Date("2012-06-01"))
mpolls.net <- subset(mpolls.net, variable != "gqt")
mpolls.net$group <- mpolls.net$pollster %in% subset(final.polls, euclidian < .04)$pollster


mpolls.net <- merge(mpolls.net,
                    data.frame(final = final.result,
                               variable = c("epn", "amlo", "jvm", "gqt")),
                    by = "variable")
mpolls.net <- ddply(mpolls.net,
                    .(pollster, variable),
                    transform, final = abs(final - value))


ggplot(mpolls.net, #!pollster %in% c("Milenio-GEA ISA", "GEA-ISA")),         
       aes(no.answer, final, group = variable, color = group)) +
  geom_point() +
  geom_smooth(aes(group = group), method = lm) +
  facet_wrap(~ variable) +
  scale_color_hue(labels = c("not close", "close"))+
  xlab("Non response / Not available / Refused") +
  ylab("Absolute difference between poll and final result") +
  ylim(0, .5) +
  scale_x_continuous(labels = percent) +
  scale_y_continuous(labels = percent) +
  opts(title = "June polls by candidate,\n grouped by how close the pollster was to the election results")
ggsave("graphs/no.answer.png", dpi = 100, width = 7, height = 5)

fit <- lm(final ~ -1+no.answer*variable, 
          mpolls.net)

dlply(mpolls.net, .(variable, group),
      function(df) summary(lm(final ~ no.answer, data = df)))
summary(fit)
coefplot(fit)


fit <- lm(final ~ no.answer,
               data = subset(mpolls.net, variable == "jvm" & group == FALSE))
quantile(coef(sim(fit))[,2], c(.025, .975))
summary(fit)
ggplot(as.data.frame(coef(sim(fit))), aes(V2)) +
  geom_density() +
  opts(title = "posterior probability of the slope lm(final ~ no.answer)")


## mraw <- melt(na.omit(polls), id = c("start", "end", "size", "pollster", "date",
##                           "no.answer"))
## mraw <- ddply(mraw, .(variable), transform, value = scale(value))

## ggplot(mraw, #!pollster %in% c("Milenio-GEA ISA", "GEA-ISA")),         
##        aes(no.answer, value, group = variable, color = pollster)) +
##   geom_point() +
##   geom_smooth(method = lm) +
##   facet_wrap(~ variable) +
##   scale_x_continuous("no answer", label = percent)+
##   scale_y_continuous("voting intention standarized") +
##   opts(title = "Raw and effective preferences")
## ggsave(file.path("graphs","no.answer.bias.svg"), dpi = 100, w = 9, h=6)

## fit <- lm(value ~ no.answer+variable, 
##          subset(mraw, str_detect(variable, "raw")))
## summary(fit)
## coefplot(fit)

## mraw <- subset(mraw, !pollster %in% c("Milenio-GEA ISA", "GEA-ISA"))
## ggplot(mraw, #!pollster %in% c("Milenio-GEA ISA", "GEA-ISA")),         
##        aes(no.answer, value, group = variable, color = pollster)) +
##   geom_point() +
##   geom_smooth(method = lm) +
##   facet_wrap(~ variable) +
##   scale_x_continuous("no answer", label = percent)+
##   scale_y_continuous("voting intention standarized") +
##   opts(title = "Raw and effective preferences - excluding GEA ISA and Covarrubias")
## ggsave(file.path("graphs","no.answer.bias-exclud-gea-covarr.svg"), dpi = 100, w = 9, h=6)

## fit <- lm(value ~ no.answer*variable, 
##          subset(mraw, !str_detect(variable, "raw")))
## summary(fit)
## coefplot.lm(fit)

## 0.44302 -.33*.17 + 0*-.16 + 1*-.17

## #write.csv(merge(raw, polls, by = c("start", "end", "pollster"), all.y = TRUE),
##  #         "temp.csv", row.names = FALSE)
