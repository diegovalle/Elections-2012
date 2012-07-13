formatJSONDate <- function(date) {
   format(date, "%a, %d %b %Y, %H:%M:%S")
}

head(alpha)
temp <- dcast(formula =  date ~ candidate,
      data = alpha[,c("date", "bar", "candidate")],
      value.var = "bar")
temp$date <- formatJSONDate(temp$date)

temppolls <- polls
temppolls$date <- formatJSONDate(temppolls$date)
temppolls$start <- formatJSONDate(temppolls$start)
temppolls$end <- formatJSONDate(temppolls$end)
temppolls$pollster <- as.character(temppolls$pollster)

##t.mean.prediction <- mean.prediction
##t.mean.prediction$date <- formatJSONDate(t.mean.prediction$date)

sink("clean-data/json.txt")
cat(toJSON(list(kalman = temp,
                polls = temppolls[!is.na(temppolls$epn),])))
sink()

library(RJSONIO)
options(stringsAsFactors = TRUE)
t <- data.frame(t(temppolls[!is.na(temppolls$epn),]))
nam <- names(t[[1]])

t <- apply(t, 2, as.character)
t <- apply(t, 2, function(x) {names(x) <- nam;x})

sink("clean-data/json.txt")
cat(toJSON(list(kalman = temp,
                polls = t)))
sink()

sink("clean-data/blah.txt")
modified <- list(
  traits = colnames(temppolls),
  values = unname(apply(temppolls, 1, function(x) as.data.frame(t(x))))
)
cat(toJSON(modified))
#cat(apply(temppolls, 1, toJSON))
sink()

toJSON(structure(apply(temppolls, 1, function(x) {x}), names = rep(nam, 90)))
   nrow(temppolls
               )
   x <- toJSON(tmp[1, ])

cat(asJSVars( a = 1:10, myMatrix = matrix(1:15, 3, 5)))
