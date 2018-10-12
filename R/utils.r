##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param x 
##' @importFrom dplyr group_by summarise %>% select
##' @importFrom tidyr spread
##' @return 
##' @author Sebastian Funk
df_to_vecmat <- function(x) {
    list(y=x %>%
             group_by(last_obs) %>%
             summarise(incidence=unique(incidence)) %>%
             .$incidence,
         dat = x %>%
             select(-incidence) %>%
             spread(sample_id, value) %>%
             select(-last_obs) %>%
             as.matrix)
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param y 
##' @param dat 
##' @param J 
##' @param N 
##' @importFrom goftest ad.test
##' @return 
##' @author Sebastian Funk
pit_test_sample <- function(y, dat, J, N=10) {
    if (missing(J)) J <- as.integer(round(sqrt(length(y))))
    f <- lapply(seq_along(y), function(i) ecdf(dat[i, ]))
    P_x <- vapply(seq_along(y), function(i) f[[i]](y[i]), .0)
    P_xm1 <- vapply(seq_along(y), function(i) f[[i]](y[i]-1), .0)
    pvalues <-
        replicate(N, ad.test(P_xm1 + runif(length(P_x)) * (P_x - P_xm1))$p.value)
    return(pvalues)
}

calibration <- function (x) {
    args <- df_to_vecmat(x)
    pvalues <- do.call(pit_test_sample, args)
    c(mean=mean(pvalues), sd=sd(pvalues))
}

##' Determines calibration of incidence forecasts for the 2014--15 Ebola epidemic in Western Area in Sierra Leone
##'
##' @param x a forecast data frame with columns 'date', 'last_obs', 'incidence', 'R0', 'cases' and 'sample_id'
##' @importFrom dplyr filter left_join group_by_at vars select mutate
##' @importFrom tidyr gather nest unnest spread
##' @importFrom tibble enframe
##' @importFrom purrr map
##' @return data frame with (random) PIT p-values
##' @author Sebastian Funk
incidence_forecast_calibration <- function(x) {
    return(x %>%
           filter(date > last_obs) %>%
           left_join(ebola_wa) %>%
           filter(!is.na(incidence)) %>%
           gather(variable, value, R0, cases) %>%
           filter(variable=="cases") %>%
           mutate(horizon=as.integer((date-last_obs)/7)) %>%
           select(-date, -variable) %>%
           group_by_at(vars(-value, -last_obs, -sample_id, -incidence)) %>%
           nest %>%
           mutate(calibration=map(data, calibration)) %>%
           unnest(map(calibration, enframe)) %>%
           spread(name, value))
}
