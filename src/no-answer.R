mpolls2 <- ddply(mpolls, .(variable), transform, value = scale(value))
ggplot(mpolls2, #!pollster %in% c("Milenio-GEA ISA", "GEA-ISA")),         
       aes(no.answer, value, group = variable, color = pollster)) +
         geom_point() +
         geom_smooth(method = lm) +
         facet_wrap(~ variable)


fit <- lm(value ~ no.answer*variable, 
          mpolls)
summary(fit)
coefplot(fit)





mraw <- melt(na.omit(polls), id = c("start", "end", "size", "pollster", "date",
                          "no.answer"))
mraw <- ddply(mraw, .(variable), transform, value = scale(value))

ggplot(mraw, #!pollster %in% c("Milenio-GEA ISA", "GEA-ISA")),         
       aes(no.answer, value, group = variable, color = pollster)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_wrap(~ variable) +
  scale_x_continuous("no answer", label = percent)+
  scale_y_continuous("voting intention standarized") +
  opts(title = "Raw and effective preferences")
ggsave(file.path("graphs","no.answer.bias.svg"), dpi = 100, w = 9, h=6)

fit <- lm(value ~ no.answer+variable, 
         subset(mraw, str_detect(variable, "raw")))
summary(fit)
coefplot(fit)

mraw <- subset(mraw, !pollster %in% c("Milenio-GEA ISA", "GEA-ISA"))
ggplot(mraw, #!pollster %in% c("Milenio-GEA ISA", "GEA-ISA")),         
       aes(no.answer, value, group = variable, color = pollster)) +
  geom_point() +
  geom_smooth(method = lm) +
  facet_wrap(~ variable) +
  scale_x_continuous("no answer", label = percent)+
  scale_y_continuous("voting intention standarized") +
  opts(title = "Raw and effective preferences - excluding GEA ISA and Covarrubias")
ggsave(file.path("graphs","no.answer.bias-exclud-gea-covarr.svg"), dpi = 100, w = 9, h=6)

fit <- lm(value ~ no.answer*variable, 
         subset(mraw, !str_detect(variable, "raw")))
summary(fit)
coefplot.lm(fit)

0.44302 -.33*.17 + 0*-.16 + 1*-.17

#write.csv(merge(raw, polls, by = c("start", "end", "pollster"), all.y = TRUE),
 #         "temp.csv", row.names = FALSE)
