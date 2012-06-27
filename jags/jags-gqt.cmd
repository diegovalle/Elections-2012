model in kalman-gqt.bug
data in dumpdata-gqt.R
compile
initialize
update 1000000
monitor alpha, thin(500)
monitor sigma, thin(500)
monitor house, thin(500)
update 1000000
coda *, stem(GQT)
exit
