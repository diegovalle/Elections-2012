#Read the data from ADN Político
polls <- read.csv(file.path("data", "polls-reforma-adn.csv"), na.string = "NA",
                  fileEncoding = "UTF-8")

expect_that(apply(polls[,c("epn", "jvm", "amlo", "gqt")],1,
                  sum),
            equals(rep(1, nrow(polls)), tolerance =.05))
#polls <- subset(polls, no.answer <= .20)
#Make sure the poll data has not changed
if(file.exists(file.path("cache", "digest.rdata")))
  load(file.path("cache", "digest.rdata"))
if(exists("digest")){
   digest <- digest(polls)
   recalc <- TRUE
   save(digest, file =  file.path("cache", "digest.rdata"))
} else {
  if(all.equal(digest, digest(polls)))
    recalc <- FALSE
  else {
    digest <- digest(polls)
    recalc <- TRUE
    save(digest, file =  file.path("cache", "digest.rdata"))
  }
}

names(polls) <- c("start", "end", "pollster", "no.answer",
                  "size", #"type", "error",
                  "epn", "jvm", "amlo", "gqt")
                  #"no.answer", "winner")

#Convert to percentages
#polls[,c("epn", "jvm", "amlo", "gqt")] <- apply(polls[,c("epn", "jvm", "amlo", "gqt")], 2, 
#                                               function(x) x / 100)
#polls$start <- as.Date(polls$start, format = "%d/%m/%y")
#polls$end <- as.Date(polls$end, format = "%d/%m/%y")
polls$start <- as.Date(polls$start)
polls$end <- as.Date(polls$end)

#polls$no.answer <- NULL
#The polls are conducted over a period of several days
#take the midpoint as the date the polls was conducted
polls$date <- polls$start + ((polls$end - polls$start) / 2)
#There isn't much data before March 15
polls <- subset(polls, date >= as.Date("2012-03-15"))
polls <- subset(polls, !pollster %in% c("Milenio-GEA ISA", "GEA-ISA"))

#Subset crappy polls
#polls <- subset(polls, !pollster %in% c("Milenio-GEA ISA", "GEA-ISA"))
#mpolls <- subset(mpolls, !pollster %in% c("Milenio-GEA ISA", "GEA-ISA"))













cleanGEA <- function(filename) {
df <- read.csv(file.path("data", filename), na.string = "ND",
                  fileEncoding = "UTF-8")
names(df) <- c("start", "end", "pollster",
                  "size", "type", "error",
                  "epn", "jvm", "amlo", "gqt",
                  "no.answer", "winner")
#df <- subset(df, pollster %in% c("Milenio-GEA ISA"))

#Convert to percentages
df[,c("epn", "jvm",
      "amlo", "gqt",
      "no.answer")] <- apply(df[,c("epn", "jvm",
                                   "amlo", "gqt",
                                   "no.answer")], 2, 
                                               function(x) x / 100)
df$start <- as.Date(df$start, format = "%d/%m/%y")
df$end <- as.Date(df$end, format = "%d/%m/%y")

#df$no.answer <- NULL
#The df are conducted over a period of several days
#take the midpoint as the date the df was conducted
df$date <- df$start + ((df$end - df$start) / 2)
#There isn't much data before March 15
df <- subset(df, date >= as.Date("2012-03-15"))
## df$week <- week(df$date)
## df <- ddply(df, .(week), summarise,
##                start = min(start),
##                end = max(end),
##                pollster = pollster[1],
##                no.answer = mean(no.answer),
##                size = mean(size),
##                epn = mean(epn),
##                jvm = mean(jvm),
##                amlo = mean(amlo),
##                gqt = mean(gqt),
##                date = mean(date))
## df$week <- NULL
df
}


adn.raw <- cleanGEA("polls-adn-raw.csv")
adn <- cleanGEA("polls-adn-effective.csv")
adn <- rbind.fill(adn, read.csv("data/missing-adn.csv"))


adn <- merge(adn, adn.raw, by = c("start", "end", "pollster"),
             all = TRUE)
adn$winner.x <- NULL
adn$winner.y <- NULL
adn$date.x <- NULL
adn$error.x <- NULL
adn$error.y <- NULL
adn$no.answer.x <- NULL
adn$size.y <- NULL
adn$type.y <- NULL
names(adn) <- c("start", "end", "pollster", "size", "type", 
                "epn", "jvm", "amlo", "gqt",
                "epn.gross", "jvm.gross", "amlo.gross", 
                "gqt.gross", "no.answer", "date")
adn$date <- adn$start + ((adn$end - adn$start) / 2)
adn <- arrange(adn, desc(date))
write.csv(adn, "data/polldb.csv", row.names = FALSE)


adn.gea <- subset(adn, pollster %in% c("Milenio-GEA ISA", "GEA-ISA")) 
adn.no.gea <- subset(adn, !pollster %in% c("Milenio-GEA ISA", "GEA-ISA")) 
adn.gea$week <- week(adn.gea$date)
adn.gea <- ddply(adn.gea, .(week), summarise,
                 start = min(start),
                 end = max(end),
                 pollster = pollster[1],                 
                 size = mean(size, na.rm = TRUE),
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


milenio.raw <- cleanGEA("polls-adn-raw.csv")[,c("epn", "jvm", "amlo", "gqt",
                  "no.answer")]
names(milenio.raw) <- c("epn.raw", "jvm.raw", "amlo.raw", "gqt.raw",
                   "no.answer")
polls.gea <- (cleanGEA("polls-adn-effective.csv")[,-4], milenio.raw)




raw <- read.csv("data/polls-adn-raw.csv", na.string = "ND")
names(raw) <- c("start", "end", "pollster",
                   "size", "type", "error",
                   "epn.raw", "jvm.raw", "amlo.raw", "gqt.raw",
                   "no.answer", "winner")
# #Convert to percentages
raw[,c("epn.raw", "jvm.raw",
       "amlo.raw", "gqt.raw",
       "no.answer")] <- apply(raw[,c("epn.raw",
                                               "jvm.raw", 
                                               "amlo.raw",
                                               "gqt.raw",
                                               "no.answer")], 2, 
                                             function(x) x / 100)
raw$start <- as.Date(raw$start, format = "%d/%m/%y")
raw$end <- as.Date(raw$end, format = "%d/%m/%y")
raw$date <- raw$start + ((raw$end - raw$start) / 2)
#raw$start <- as.Date(raw$start)
#raw$end <- as.Date(raw$end)
# 
# #The polls are conducted over a period of several days
# #take the midpoint as the date the polls was conducted

# #There isn't much data before March 15
 raw <- subset(raw, date >= as.Date("2012-03-15"))
#
#raw <- subset(raw, !pollster %in% c("Milenio-GEA ISA", "GEA-ISA"))


polls.all <- merge(raw[,c("start", "end", "pollster",
                          "epn.raw",
                          "jvm.raw",
                          "amlo.raw",
                          "gqt.raw")],
                   polls,
                   by = c("start", "end", "pollster"), all.y = TRUE)
polls.all <- rbind(polls.all, polls.gea)
poll.all <- arrange(polls.all, desc(date))

write.csv(polls.all, "temp.csv", row.names = FALSE)
polls <- polls.all
#polls <- subset(polls,
#                !pollster %in% c("Milenio-GEA ISA", "GEA-ISA"))
#polls <- subset(polls, date >= as.Date("2012-04-01"))

mpolls<- melt(polls[,c("pollster",
                            "date",
                            "epn",
                            "jvm",
                            "amlo",
                            "gqt")],
              id = c("date", "pollster")) 
mpolls.raw<- melt(na.omit(polls[, c("pollster",
                            "date",
                            "epn.raw",
                            "jvm.raw",
                            "amlo.raw",
                            "gqt.raw",
                            "no.answer")]),
                  id = c("date", "pollster"))



days.to.election <- kelection.day - max(polls$date)


df <- read.csv(file.path("data", "polls-adn-effective.csv"), na.string = "ND",
                  fileEncoding = "UTF-8")
names(df) <- c("start", "end", "pollster",
                  "size", "type", "error",
                  "epn", "jvm", "amlo", "gqt",
                  "no.answer", "winner")
df <- subset(df, !pollster %in% c("Milenio-GEA ISA"))

#Convert to percentages
df[,c("epn", "jvm",
      "amlo", "gqt",
      "no.answer")] <- apply(df[,c("epn", "jvm",
                                   "amlo", "gqt",
                                   "no.answer")], 2, 
                                               function(x) x / 100)
df$start <- as.Date(df$start, format = "%d/%m/%y")
df$end <- as.Date(df$end, format = "%d/%m/%y")

#df$no.answer <- NULL
#The df are conducted over a period of several days
#take the midpoint as the date the df was conducted
df$date <- df$start + ((df$end - df$start) / 2)
#There isn't much data before March 15
df <- subset(df, date >= as.Date("2012-03-15"))


setdiff.data.frame = function(A, B){
     g <-  function( y, B){
                 any( apply(B, 1, FUN = function(x)
 identical(all.equal(x, y), TRUE) ) ) }
     unique( A[ !apply(A, 1, FUN = function(t) g(t, B) ), ] )
}
setdiff.data.frame(polls, df)

write.csv(df, "t.tmp")
write.csv(merge(polls, df, by = c("start", "end", "pollster"),
      all = TRUE), "t.csv")
