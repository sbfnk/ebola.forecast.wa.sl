##' Creates Bayesian time-series regression null model forecasts for "Assessing the performance of real-time epidemic forecasts: A case study of the 2013-16 Ebola epidemic."  (doi: 10.1101/177451).
##'
##' Forecasts are generated for Western Area, Sierra Leone
##' @param start_forecast_date The date at which to start forecasts.
##' @param forecast_horizon The maximum number of weeks to forecast.
##' @return A data frame containing MCMC samples from the predictive probability distribution at each time point.
##' @author Sebastian Funk
null_model_bsts <- function(start_forecast_date=as.Date("2014-08-24"), forecast_horizon = 10)
{

    forecast_dates <- ebola_wa %>%
        dplyr::filter(date >= start_forecast_date) %>%
        .$date

    df <- list()

    for (id in seq_along(forecast_dates))
    {
        forecast_date <- forecast_dates[id]
        y <- ebola_wa %>%
            dplyr::filter(date<=forecast_date) %>%
            .$value

        ss <- AddLocalLinearTrend(list(), y)
        bsts.model <- bsts(y, ss, niter=6000, ping=0)

        p <- predict.bsts(bsts.model, horizon=forecast_horizon, burn=1000,
                          quantiles=c(0.05, 0.25, 0.75, 0.95))

        dist <- p$distribution

        df[[as.character(forecast_date)]] <- dist %>%
            tbl_df %>%
            mutate(sample_id=(1:n())-1L) %>%
            gather(horizon_week, value, -sample_id) %>%
            mutate(value=as.integer(round(value))) %>%
            mutate(forecast_week=as.integer(str_extract(horizon_week, "[0-9]+")),
                   last_obs=as.Date(forecast_date),
                   date=last_obs+7*forecast_week) %>%
            dplyr::select(sample_id, date, last_obs, value)
    }

    df <- lapply(df, bind_rows)
    df <- bind_rows(df)
    df <- df %>%
        mutate(value=if_else(value < 0, 0L, value),
               model="Autoregressive")
    return(df)
}
