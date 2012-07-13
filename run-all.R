########################################################
# Author: Diego Valle-Jones
# Website: www.diegovalle.net
# Date Created: Wed Jun 20 18:51:30 2012
# Email: diegovalle at gmail.com
# Purpose: 
# Copyright (c) Diego Valle-Jones. All rights reserved

## The election was held on July 1
kelection.day <- as.Date("2012-07-01")
##Number of simulations to run in forecasting the election
## Note that if you change the number of simulations from
## 1000 you'll also have to change kalman(candidate).bug files
## to generate more samples from the posterior
nSimulations <- 1000
## The results of the election on July 1, 2012
final.result <- c(.3821, .3159, .2541, .029)
final.result <- sapply(final.result, function(x) x / sum(final.result))

## load packages
source("src/load-libraries.R")

## load data 
source("src/clean.R")

## A simple plot with loess
source("src/loess.R")

## Rank the pollsters
source("src/pollster-ranking.R")

##Analysis of non-response
source("src/no-answer.R")

## A deprecated analysis with dirichlet regression
##source("src/dirichlet.R")
## functions to aid in simulating the election
source("src/pred-functions.R")

###Functions for running the kalman filter
source("src/kalman-functions.R")


## deprecated forecast simulations with gamm and dirichlet regressions
##source("src/gamm-simulation.R")
##source("src/dirichlet-simulation.R")

## Analysis of the elcection
source("src/kalman.R")

## Generate forecasts based on the kalman filter
## Not needes since the election is now in the past
## Checkout the pre-election tag from git if you need it to work
##source("src/kalman-simulate.R")

##Analysis of the 2006 electiongs
source("src/2006.R")
