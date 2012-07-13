cleanGEA <- function(filename) {
# Description: 
#   clean the table dowloaded from ADN Político
# Args:
#   filename - the name of the fiel containing either the gross or net
#    table of voting intention 
#
# Returns: 
#   a data.frame
  
  df <- read.csv(file.path("data", filename), na.string = "ND",
                 fileEncoding = "UTF-8")
  names(df) <- c("start", "end", "pollster",
                 "size", "type", "error",
                 "epn", "jvm", "amlo", "gqt",
                 "no.answer", "winner")
  
  ##Convert to percentages
  df[,c("epn", "jvm",
        "amlo", "gqt",
        "no.answer")] <- apply(df[,c("epn", "jvm",
                                     "amlo", "gqt",
                                     "no.answer")], 2, 
                               function(x) x / 100)
  ##Make sure error is a string
  df$error <- as.character(df$error)
  df$start <- as.Date(df$start, format = "%d/%m/%y")
  df$end <- as.Date(df$end, format = "%d/%m/%y")
  ##Treat the quarterly poll by gea-isa as belonging to the daily tracking poll
  ##by milenio-gea-isa
  df$pollster <- car::recode(df$pollster, 'c("Milenio-GEA ISA", "GEA-ISA")= "Milenio-GEA ISA"')
  ##The polls are conducted over a period of several days
  ##take the midpoint as the date the poll was conducted
  df$date <- df$start + ((df$end - df$start) / 2)

  return(df)
}

#Polls in Mexico are usually reported as net and gross (with the percentage who didn't answer or were not home or didn't state a preference)
adn.raw <- cleanGEA("polls-adn-gross.csv")
adn <- cleanGEA("polls-adn-effective.csv")
#The poll of polls from ADN Político is missing some polls by Indemerc
adn <- rbind.fill(adn, read.csv("data/missing-adn.csv"))

#Merge the net and gross files
adn <- merge(adn, adn.raw, by = c("start", "end", "pollster"),
             all = TRUE)
adn$winner.x <- NULL
adn$winner.y <- NULL
adn$date.x <- NULL
adn$error.x <- str_replace(adn$error.x, "±", "")
adn$error.x <- as.numeric(str_replace(adn$error.x, "%", "")) / 100
adn$error.y <- NULL
adn$no.answer.y[!is.na(adn$no.answer.x)] <- adn$no.answer.x[!is.na(adn$no.answer.x)]
adn$no.answer.x <- NULL
adn$size.y <- NULL
adn$type.y <- NULL
names(adn) <- c("start", "end", "pollster", "size", "type", 
                "error", "epn", "jvm", "amlo", "gqt",
                "epn.gross", "jvm.gross", "amlo.gross", 
                "gqt.gross", "no.answer", "date")
adn$date <- adn$start + ((adn$end - adn$start) / 2)

#Correct an error in the adn político poll of polls
#The March Buendía y Laredo poll should really be April
adn[adn$date == as.Date("2012-03-14"),]$start <- as.Date("2012-04-12")
adn[adn$date == as.Date("2012-03-14"),]$end <- as.Date("2012-04-16")
adn[adn$date == as.Date("2012-03-14"),]$date <- as.Date("2012-04-14")

#There isn't much data before Februry 13
adn <- subset(adn, date >= as.Date("2012-02-13"))

adn <- arrange(adn, desc(date))
#adn <- subset(adn, !pollster %in% c("Milenio-GEA ISA", "GEA-ISA"))
write.csv(adn, "clean-data/polldb.csv", row.names = FALSE)



#There's a weird seasonal pattern in the GEA ISA poll
#correct this by taking the weekly average
adn.gea <- subset(adn, pollster %in% c("Milenio-GEA ISA", "GEA-ISA")) 
adn.no.gea <- subset(adn, !pollster %in% c("Milenio-GEA ISA", "GEA-ISA")) 

## ggplot(melt(adn.gea[, c("date", "epn", "jvm", "amlo", "gqt")],
##             id = c("date")),
##        aes(date, value, group = variable)) +
##   geom_line()
## ggplot(na.omit(adn.gea), aes(date, no.answer)) +
##   geom_line()
## ggplot(na.omit(adn.gea), aes(date, epn)) +
##   geom_line()
## plot(stl(ts(na.omit(adn.gea$epn), freq = 5
##             , start = (5)), s.win = "periodic"))


adn.gea$week <- week(adn.gea$date)
adn.gea <- ddply(adn.gea, .(week), summarise,
                 start = min(start),
                 end = max(end),
                 pollster = pollster[1],                 
                 size = mean(size, na.rm = TRUE),
                 error = mean(error, na.rm = TRUE),
                 type = type[1],
                 epn.gross = mean(epn.gross, na.rm = TRUE),
                 jvm.gross = mean(jvm.gross, na.rm = TRUE),
                 amlo.gross = mean(amlo.gross, na.rm = TRUE),
                 gqt.gross = mean(gqt.gross, na.rm = TRUE),
                 epn = mean(epn, na.rm = TRUE),
                 jvm = mean(jvm, na.rm = TRUE),
                 amlo = mean(amlo, na.rm = TRUE),
                 gqt = mean(gqt, na.rm = TRUE),
                 no.answer = mean(no.answer, na.rm = TRUE),
                 date = mean(date))
adn.gea$week <- NULL
polls <- rbind.fill(adn.gea, adn.no.gea)


#Unit tests
expect_that(as.vector(apply(na.omit(polls[,c("epn", "jvm", "amlo", "gqt")]), 1,
                  sum)),
            equals(rep(1, length(na.omit(polls$epn))), tolerance =.05))
expect_that(as.vector(na.omit(apply(polls[,c("epn.gross", "jvm.gross",
                           "amlo.gross", "gqt.gross", "no.answer")],1,
                  sum))),
            equals(rep(1, length(na.omit(polls$epn.gross))), tolerance =.05))
##expect_that(as.vector(na.omit(polls[,c("no.answer")] < 1)),
  ##          is_true())
write.csv(polls, "clean-data/polldb-weeklygea.csv", row.names = FALSE)

mpolls<- melt(polls[,c("pollster",
                            "date",
                            "epn",
                            "jvm",
                            "amlo",
                            "gqt")],
              id = c("date", "pollster")) 
mpolls.gross<- melt(na.omit(polls[, c("pollster",
                            "date",
                            "epn.gross",
                            "jvm.gross",
                            "amlo.gross",
                            "gqt.gross",
                            "no.answer")]),
                  id = c("date", "pollster", "no.answer"))



days.to.election <- ceiling(kelection.day - max(polls$date))

test_that(max(polls$date) + days.to.election, is_identical_to(kelection.day))
##polls <- subset(polls, !pollster %in% c("Milenio-GEA ISA", "GEA-ISA"))

ggplot(data.frame(error = sqrt(1/adn$size),
           poll.error = adn$error,
           diff = sqrt(1/adn$size) - adn$error,
                  pollster = adn$pollster),
       aes(poll.error, error, label = pollster)) +
  geom_point() +
  geom_text() +
  geom_abline(intercept = 0, slope = 1) 
ggsave("graphs/poll-error.png", dpi = 100, width = 7, height = 7)
