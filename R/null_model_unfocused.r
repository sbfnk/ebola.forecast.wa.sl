##' Creates unfocused null model forecasts for "Assessing the performance of real-time epidemic forecasts: A case study of the 2013-16 Ebola epidemic."  (doi: 10.1101/177451).
##'
##' Forecasts are generated for Western Area, Sierra Leone
##' @param start_forecast_date The date at which to start forecasts.
##' @param forecast_horizon The maximum number of weeks to forecast.
##' @return A data frame containing MCMC samples from the predictive probability distribution at each time point.
##' @import rbi
##' @import rbi.helpers
##' @import dplyr
##' @import stringi
##' @author Sebastian Funk
null_model_unfocused <- function(start_forecast_date=as.Date("2014-08-24"), forecast_horizon = 10)
{

    model_str <-
'
  model nonmech {

      const rep = 0.6

      param p_phi // overdispersion in reporting
      param p_vol_Z // volatility of incidence

      state Z

      obs Inc

      noise n_Z_walk

      sub transition {
          n_Z_walk ~ wiener()
          Z <- Z * exp(p_vol_Z * n_Z_walk)
      }

      sub observation {
          Inc ~ negbin(rep * Z, 1/p_phi)
      }

      sub parameter {
          p_phi ~ uniform(lower = 0, upper = 0.5)
          p_vol_Z ~ uniform(lower = 0, upper = 0.5)
      }

      sub initial {
          Z ~ uniform(lower = 0, upper = 100)
      }
  }
'

    df <- list()

    df_data <- ebola_wa %>%
        mutate(day = as.integer(date-min(date)+7),
               week = day %/% 7L)

    min_week <- df_data %>%
        dplyr::filter(date >= start_forecast_date) %>%
        .$week %>%
        min

    for (loop_week in min_week:max(df_data$week)) {
        message("Week ", loop_week)
        cases <- df_data %>%
            dplyr::filter(week <= loop_week) %>%
            dplyr::select(time=day, value=incidence)
        bim <- bi_model(lines = stringi::stri_split_lines(model_str)[[1]])
        ## fit
        bi <- libbi(bim, with=("transform-initial-to-param"),
                    obs=list(Inc=cases),
                    end_time=max(cases$time),
                    thin=100) %>%
            sample(nsamples=10000, proposal="prior") %>%
            adapt_particles %>%
            adapt_proposal(min=0.1, max=0.5, adapt="both") %>%
            sample(nsamples=100000)

        ## forecast
        pred <- predict(bi, input=bi,
                        end_time=max(cases$time)+forecast_horizon*7,
                        output_every=7,
                        with=c("transform-obs-to-state"))
        forecast <- bi_read(pred, type=c("obs"))$Inc
        df <- c(df, list(forecast %>%
                         mutate(last_obs=df_data %>%
                                    dplyr::filter(week==loop_week) %>%
                                    .$date %>%
                                    unique
                                ) %>%
                         rename(day=time) %>%
                         inner_join(df_data %>% dplyr::select(day, date))))
        if (!bi$error_flag) gc() ## save memory
    }

    df <- df %>%
        bind_rows %>%
        tbl_df %>%
        mutate(sample_id=as.integer(np), cases=as.integer(value)) %>%
        select(last_obs, date, sample_id, cases) %>%
        mutate(model="Unfocused")

    return(df)
}
