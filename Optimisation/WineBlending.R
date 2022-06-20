install.packages('/Library/gurobi950/macos_universal2/R/gurobi_9.5-0_R_4.1.1.tgz', repos=NULL)
install.packages('slam')


# Copyright 2021, Gurobi Optimization, LLC
#
# This example formulates and solves the following simple MIP model:
#  maximize
#        x +   y + 2 z
#  subject to
#        x + 2 y + 3 z <= 4
#        x +   y       >= 1
#        x, y, z binary
library(slam)
library(gurobi)

profits <- c(2.35,2.6,2.1,1.55,
             6.65,6.4,6.9,7.45,
             6.65, 6.4, 6.9, 7.45,
             6.65, 6.4, 6.9, 7.45,
             0.6,0.35,0.85,1.4)
             
           
variables <- c(1,1,1,1,
               1,1,1,1,
               1,1,1,1,
               1,1,1,1,
               1,1,1,1
               )

model <- list()

model$A          <- matrix(c(1,2,3,1,1,0), nrow=2, ncol=3, byrow=T)
model$obj        <- sum(variables*profits)
model$modelsense <- 'max'
model$rhs        <- c()
model$sense      <- c('<', '>')
model$vtype      <- 'B'

params <- list(OutputFlag=0)

result <- gurobi(model, params)

print('Solution:')
print(result$objval)
print(result$x)

# Clear space
rm(model, result, params)

#--------------------Winery Blending Case-------#

# vintage cab sells for 9$
# non vintage cab sells for 5.5$
# non vintage merlot sells for 2.95$

# max net profit

#--------- constraints--------#
# cab sauv acidity <= 0.7 / 100ml
# cab sauv sugar <= 0.3 %
# merlot acidity <= 0.3 / 100ml
# bottle >= 75% of grape
# alcohol >10% <15%
# vintage dated must be >= 95% this years grapes
# vintage dated >= 85% blending grapes

#0.35x1+0.75x2+0.55x3+0.25x4 <

#------ objective function--------#
#    5 products made up of 4 components
#   6.65x11+ 6.4x12+ 6.9x13 + 7.45x14 +
#   
#

# -------alchohol level constraints------
# 0.135x1+0.153x2+0.115x3+0.157x4 >= 0.10
# 0.135x1+0.153x2+0.115x3+0.157x4 <= 0.15


#--------quantiy constraints--------#
#-----

# cab SB = 9

objective.in = c(6.65,2.6,2.1,1.55)

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


