########################################################
# Author: Diego Valle-Jones
# Website: www.diegovalle.net
# Date Created: Wed Jun 20 18:51:30 2012
# Email: diegovalle at gmail.com
# Purpose: 
# Copyright (c) Diego Valle-Jones. All rights reserved

##The election is held on July 1
kelection.day <- as.Date("2012-07-01")
##Number of simulations to run in forecasting the election
##Note that if you change the number of simulations from
##1000 you'll also have to change kalman(candidate).bug files
##to generate more samples from the posterior
nSimulations <- 1000

#load packages
source("src/load-libraries.R")
#load data 
source("src/clean.R")
#A simple plot with loess
source("src/loess.R")
#A deprecated analysis with dirichlet regression
#source("src/dirichlet.R")
#functions to aid in simulating the election
source("src/pred-functions.R")
#deprecated forecast simulations with gamm and dirichlet regressions
#source("src/gamm-simulation.R")
#source("src/dirichlet-simulation.R")
#Analysis of the elcection
source("src/kalman.R")
#Generate forecasts based on the kalman filter
source("src/kalman-simulate.R")
