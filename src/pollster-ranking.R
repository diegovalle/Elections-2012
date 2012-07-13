########################################################
## Author: Diego Valle-Jones
## Website: www.diegovalle.net
## Date Created: Mon Jul  2 16:45:40 2012
## Email: diegovalle at gmail.com
## Purpose: The best and worst pollsters of the 2012 elections 
## Copyright (c) Diego Valle-Jones. All rights reserved

##Get the values for the last poll by pollster
##only include polls conducted in June
final.polls <- ddply(subset(polls, date >= as.Date("2012-06-01")),
        .(pollster), summarise,
                     epn = epn[1],
                     amlo = amlo[1],
                     jvm = jvm[1],
                     gqt = gqt[1],
                     no = no.answer[1],
                     error = error[1])


##Replace the weekly value for GEA ISA with the last daily tracking poll
final.polls[final.polls$pollster == "Milenio-GEA ISA",2:5] <- c(.469, .285, .224, .022)
##Euclidian distance
final.polls$euclidian <- apply(final.polls[,2:5], 1 , function(x) sqrt(sum((x - final.result)^2)))

##Order the polls
final.polls <- arrange(final.polls, euclidian)
print(xtable(final.polls), include.rownames = FALSE, type = "html")
final.polls$pollster <- with(final.polls, reorder(pollster, -euclidian))
final.polls$group <- final.polls$euclidian < .04


ggplot(final.polls, aes(euclidian, pollster, color = group)) +
  geom_point() +
  xlab("euclidian distance") +
  opts(title = "Polling firms ordered by euclidian distance\nfrom official count") +
  scale_color_hue("group", labels = c("not close", "close"))
ggsave("graphs/pollster-ranking.png", dpi = 100, width = 7, height = 6)
