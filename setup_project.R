#########################################################
#   THESE NOTES ARE NO LONGER RELEVANT CHECK LATER
#   Each object is given a location in 3 space in a right handed system, given as a row in an attribute matrix. 
#   This object is displaced to [r*sqrt(2),0,0] in a left handed system. 
#   The altitude and azimuth from the perspective point to the point are calculated, and mapped onto a linear grid.
#
#
##########################################################

library(network)
library(scatterplot3d)
library(animation)
library(rgl)
#library(shiny)

library(Rcmdr)
#ani.options(interval=.001,nmax=360,loop=FALSE)
#source('C:/Users/Deadletter.DEADLETTER/Dropbox/phd/R files/network_project/setup_functions.R')
layout.par<-setup.layout.par(radius=1)
library(Rcpp)
library(microbenchmark)
#library(BH)
#Rcpp::sourceCpp('~/cpp projects/networks/rest.cpp')  #the rcpp:: isn't necessary
#library(manipulate)
#setup_functions.R
