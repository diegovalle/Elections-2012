model in kalman-epn.bug
data in dumpdata-epn.R
compile
initialize
update 2000000
monitor alpha, thin(500)
monitor sigma, thin(500)
monitor house, thin(500)
update 5000000
coda *, stem(EPN)
exit
