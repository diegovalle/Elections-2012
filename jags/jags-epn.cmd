model in kalman-epn.bug
data in dumpdata-epn.R
compile
initialize
update 1000000
monitor alpha, thin(500)
monitor sigma, thin(500)
monitor house, thin(500)
update 1000000
coda *, stem(EPN)
exit
