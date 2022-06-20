library(lpSolve)
install.packages("ExcelFunctionsR")
library(ExcelFunctionsR)

data_raw <- read.csv("Paintprimer.csv")

data_raw

?AVERAGEIF()
avg_processing_time <- AVERAGEIF(data_raw$Product.Number,data_raw$Product.Number,data_raw$Processing.Time)

lp(direction = "max", objective.in, const.mat, const.dir, const.rhs)

objective.in = c(400,300)

const.mat = matrix(c(10,15,1,0,0,1,1,0,0,1), nrow =5, byrow = TRUE)
const.mat


production__hour_cap <- 150
x_production_min <- 3
y_production_min <- 4
b_non <- 0
r_non <- 0

const.rhs = c(production__hour_cap, x_production_min,
              y_production_min, b_non, r_non)
const.rhs

const.dir = c("<=",">=",">=",">=",">=")
const.dir

optimum <- lp(direction = "max", objective.in, const.mat, const.dir, const.rhs)

optimum$solution
optimum$objval

summary(optimum)

optimum$constraints

get.sensitivity.obj(optimum)








#--------------------Political Advertising Budget Allocation-------#


objective.in = c(1200,400)

const.mat = matrix(c(1,0,0,1,25000,12500,1,0,0,1), nrow =5, byrow = TRUE)
const.mat


tv_min <- 30
radio_max <- 60
voters_reached <- 1000000
b_non <- 0
r_non <- 0

const.rhs = c(tv_min, radio_max,
              voters_reached, b_non, r_non)
const.rhs

const.dir = c(">=","<=",">=",">=",">=")
const.dir

optimum <- lp(direction = "min", objective.in, const.mat, const.dir, const.rhs)

optimum$solution
optimum$objval

summary(optimum)

optimum$constraints


