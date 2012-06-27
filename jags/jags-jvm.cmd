model in kalman-jvm.bug
data in dumpdata-jvm.R
compile
initialize
update 1000000
monitor alpha, thin(500)
monitor sigma, thin(500)
monitor house, thin(500)
update 1000000
coda *, stem(JVM)
exit
