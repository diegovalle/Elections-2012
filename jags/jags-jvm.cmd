model in kalman-jvm.bug
data in dumpdata-jvm.R
compile
initialize
update 2000000
monitor alpha, thin(500)
monitor sigma, thin(500)
monitor house, thin(500)
update 5000000
coda *, stem(JVM)
exit
