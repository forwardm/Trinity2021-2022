install.packages("lpSolve")
library(lpSolve)

lp(direction = "max", objective.in, const.mat, const.dir, const.rhs)

objective.in = c(950,1200)

const.mat = matrix(c(1.4,2.8,1,1,3,1,1,0,0,1), nrow =5, byrow = TRUE)
const.mat


production_cap <- 70
loading_cap <- 30
resource <- 65
b_non <- 0
r_non <- 0

const.rhs = c(production_cap, loading_cap,
              resource, b_non, r_non)
const.rhs

const.dir = c("<=","<=","<=",">=",">=")
const.dir

optimum <- lp(direction = "max", objective.in, const.mat, const.dir, const.rhs)

optimum$solution
optimum$objval

summary(optimum)
