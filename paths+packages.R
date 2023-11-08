## paths+packages.R
# a place to keep variables, functions, etc. relevant across numerous scripts

# load packages
library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(ggplot2)
library(xts)
library(reshape2)
library(misty)
library(dtwclust)
library(TSclust)
library(factoextra)
library(corrplot)



## GGplot theme
theme_cust <- function(base_size = 16, base_family = "") {
  theme_classic() %+replace%
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      axis.text = element_text(color = "black")
    )
}