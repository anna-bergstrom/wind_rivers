
##### setup #####
library(tidyverse)
library(lubridate)
library(zoo)
library(xts)


##### Load data #########
clear <- read.csv('R_import_data/Clear.csv')
dinwood1 <- read.csv('R_import_data/Dinwoody1.csv')
dinwood2 <- read.csv('R_import_data/Dinwoody2.csv')
dinwood4 <- read.csv('R_import_data/Dinwoody4.csv')
dinwood5 <- read.csv('R_import_data/Dinwoody5.csv')
dinwood6 <- read.csv('R_import_data/Dinwoody6.csv')
dinwood7 <- read.csv('R_import_data/Dinwoody7.csv')
dbllake <- read.csv('R_import_data/DoubleLake.csv')
downsfrk <- read.csv('R_import_data/DownsFork.csv')
ganet2 <- read.csv('R_import_data/Gannett2.csv')
grass3 <- read.csv('R_import_data/Grass3.csv')
klndke <- read.csv('R_import_data/klondike.csv')
nglgrs <- read.csv('R_import_data/NGLGrass.csv')

nest(clear,dinwood1,dinwood2,dinwood4,dinwood5,dinwood6,dinwood7,dbllake,downsfrk,ganet2,grass3,klndke,nglgrs)