
library(demest)
library(dplyr)

## Functions to calculation method-of-moments estimates

sd_half_norm <- function(y) {
    n <- length(y)
    ss <- sum(y^2)
    sqrt(ss / n)
}

param_beta <- function(y, min = 0.8, max = 1) {
    y_trans <- (y - min) / (max - min)
    m <- mean(y_trans)
    v <- var(y_trans)
    stopifnot(v < m * (1 - m))
    shape1 <- m * (m * (1 - m) / v - 1)
    shape2 <- (1 - m) * shape1
    c(shape1 = shape1,
      shape2 = shape2,
      min = min,
      max = max)
}


## Parameter values

scale_level <- fetch("out/model.est",
                     where = c("model", "hyper", "year", "scaleLevel")) %>%
    as.numeric()

scale_trend <- fetch("out/model.est",
                     where = c("model", "hyper", "year", "scaleTrend")) %>%
    as.numeric()

scale_error <- fetch("out/model.est",
                     where = c("model", "hyper", "year", "scaleError")) %>%
    as.numeric()

damp <- fetch("out/model.est",
              where = c("model", "hyper", "year", "damp"))  %>%
    as.numeric()


## Parameter estimates for hyper-parameters for year effec

param_hyper_year <- list(scale = list(level = sd_half_norm(scale_level),
                                      trend = sd_half_norm(scale_trend),
                                      error = sd_half_norm(scale_error)),
                         damp = param_beta(damp))

saveRDS(param_hyper_year,
        file = "out/param_hyper_year.rds")



  
  



