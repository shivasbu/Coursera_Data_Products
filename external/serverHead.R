library(shiny)

pkgs <- c("randomForest","ElemStatLearn","ggplot2","caret","datasets")
pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(pkgs)) install.packages(pkgs,repos="http://cran.cs.wwu.edu/")
library(randomForest); library(ElemStatLearn); library(ggplot2);library(caret);library(datasets)
## Load data
data(vowel.train)
vowel.train$y <- as.factor(vowel.train$y)
data(imports85)
#imports85$price <- as.factor(imports85$price)
imports85<-imports85[,-2] # Too many NAs in normalizedLosses.
imports85 <- imports85[complete.cases(imports85), ]
## Drop empty levels for factors.
imports85[] <- lapply(imports85, function(x) if (is.factor(x)) x[, drop=TRUE] else x)
data(iris3)
iris$Species <- as.factor(iris$Species)