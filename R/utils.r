##' Performs an Anderson-Darling test for uniformity for a randomised PIT histogram using predictive Monte-Carlo samples
##'
##' See Eqs. 1 and 2 in Czado, Geniting and Held (2009), Predictive Model Assessment for Count Data, Biometrics Vol. 65, No. 4 (Dec., 2009), pp. 1254-1261
##' @param y vector of data (length n)
##' @param dat Nxn matrix of predictive samples, N being the number of Monte Carlo samples
##' @param J the number of bins in the PIT histogram. If not given, will use the square root of n
##' @param N the number of tests to perform, each time re-randomising the PIT
##' @importFrom goftest ad.test
##' @return the n p-values from the tests
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
pit_test_sample <- function(y, dat, J, N=10) {
    if (missing(J)) J <- as.integer(round(sqrt(length(y))))
    f <- lapply(seq_along(y), function(i) ecdf(dat[i, ]))
    P_x <- vapply(seq_along(y), function(i) f[[i]](y[i]), .0)
    P_xm1 <- vapply(seq_along(y), function(i) f[[i]](y[i]-1), .0)
    pvalues <-
        replicate(N, ad.test(P_xm1 + runif(length(P_x)) * (P_x - P_xm1))$p.value)
    return(pvalues)
}

##' Determines sharpness of an incidence forecast with an Anderson-Darling test of the randomised PIT histogram.
##'
##' @inheritParams pit_test_sample
##' @return data frame with mean and standard deviation of the resulting p values
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
##' @seealso pit_test_sample
##' @param ... other arguments for \code{\link{pit_test_sample}}
calibration <- function (y, dat, ...) {
    pvalues <- pit_test_sample(y, dat, ...)
    return(data.frame(mean=mean(pvalues), sd=sd(pvalues)))
}

##' Determines sharpness of an incidence forecast as the width of the prediction interval
##'
##' @return data frame with sharpness for each interval by date
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
##' @param interval prediction interval(s) to use
sharpness <- function (y, dat, interval) {
    res <- list()
    for (int in interval) {
        width <- apply(dat, 1, function(x) {diff(quantile(x, 0.5 + c(-int, int)/2))})
        res <-
            c(res,
              list(data.frame(date=as.Date(names(width)), interval=int, width=width)))
    }
    res <- bind_rows(res)
    return(res)
}

##' Determines bias of an incidence forecast from predictive Monte-Carlo samples as the proportion of predictive samples greater than the data
##'
##' @return data frame with bias by date
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
bias <- function (y, dat) {
    f <- lapply(seq_along(y), function(i) ecdf(dat[i, ]))
    P_x <- vapply(seq_along(y), function(i) f[[i]](y[i]), .0)
    P_xm1 <- vapply(seq_along(y), function(i) f[[i]](y[i]-1), .0)
    res <- data.frame(date=as.Date(rownames(dat)),
                      bias=1-(P_x+P_xm1))
    return(res)
}

##' Determines calibration of an incidence forecast with an Anderson-Darling test of the randomised PIT histogram.
##'
##' @param x a data frame with 'date', 'last_obs', (observed) 'incidence',  (predicted) 'cases'and 'sample_id' columns
##' @param func function for forecast assessment
##' @return mean and standard deviation of the resulting p values
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
##' @seealso calibration sharpness bias
apply_forecast_metric <- function (x, func, ...) {
    y <- x %>%
        group_by(last_obs) %>%
        summarise(incidence=unique(incidence)) %>%
        .$incidence
    dat <- x %>%
        select(-incidence, -date) %>%
        spread(sample_id, cases) %>%
        select(-last_obs) %>%
        as.matrix
    colnames(dat) <- as.character(unique(x$sample_id))
    rownames(dat) <- as.character(unique(x$date))
    return(func(y, dat, ...))
}

##' Assesses incidence forecasts for the 2014--15 Ebola epidemic in Western Area in Sierra Leone from Monte-Carlo samples
##'
##' @param x a forecast data frame with columns 'date', 'last_obs', 'incidence', 'cases' and 'sample_id', and possibly more columns (which will be grouped by)
##' @inheritParams apply_forecast_metric
##' @importFrom dplyr %>% filter left_join group_by_at vars select mutate
##' @importFrom tidyr gather nest unnest spread
##' @importFrom tibble enframe
##' @importFrom purrr map
##' @return data frame with (random) PIT p-values
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
assess_incidence_forecast <- function(x, func, ...) {
    return(x %>%
           filter(date > last_obs) %>%
           left_join(ebola_wa, by="date") %>%
           filter(!is.na(incidence)) %>%
           mutate(horizon=as.integer((date-last_obs)/7)) %>%
           group_by_at(vars(-cases, -date, -last_obs, -sample_id, -incidence)) %>%
           nest %>%
           mutate(score=map(data, apply_forecast_metric, func, ...)) %>%
           unnest(score))
}

##' Calculate quantiles for a data frame
##'
##' am x a data frame with a 'value' and a 'sample_id' column
##' @param quantiles the quantiles to compute
##' @return data frame with quantiles
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
##' @importFrom dplyr %>% select group_by_at summarise
##' @importFrom tidyr unnest spread
##' @importFrom tibble enframe
##' @importFrom purrr map
calculate_quantiles <- function(x, quantiles)
{
    return (x %>%
            select(-sample_id) %>%
            group_by_at(vars(-value)) %>%
            summarise(quantiles=list(quantile(value, quantiles))) %>%
            unnest(map(quantiles, enframe)) %>%
            spread(name, value))
}

