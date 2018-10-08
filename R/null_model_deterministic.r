##' Creates deterministic null model forecasts for "Assessing the performance of real-time epidemic forecasts: A case study of the 2013-16 Ebola epidemic."  (doi: 10.1101/177451).
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
null_model_deterministic <- function(start_forecast_date=as.Date("2014-08-24"), forecast_horizon = 10)
{

    model_str <-
'
  model seir {

      const e_rho_erlang = 2 // number of Erlang compartments for incubation
      const d_onset2notification = 4.3 // days from onset to notification
      const d_onset2outcome = 11.2 // days from onset to outcome
      const d_incubation = 9.4 // incubation period in days
      const rep = 0.6
      const p_N = 1304507

      dim rho_erlang(e_rho_erlang)

      // estimated parameters
      param p_phi // overdispersion in reporting
      param p_R0

      state S (has_output=0) // susceptible
      state E[rho_erlang] // 2 exposed compartments
      state C // infectious in the community
      state Q // infectious but notified
      state R // recovered and immune
      state Z   // accumulator

      obs Inc // incidence

      sub transition {

          inline r_gamma = 1 / (d_onset2outcome - d_onset2notification)
          inline r_nu = 1 / d_onset2notification
          inline r_rho = 1 / d_incubation

          Z <- 0

          ode {

              dS/dt =
                - p_R0 / (d_onset2outcome) * (C + Q) / (S + E[0] + E[1] + C + Q + R) * S // infection

              dE[rho_erlang]/dt =
                + (rho_erlang == 0 ? p_R0 / (d_onset2outcome) * (C + Q) / (S + E[0] + E[1] + C + Q + R) * S : 0)
                + (rho_erlang > 0 ? e_rho_erlang * r_rho * E[rho_erlang - 1] : 0) // moving through incubation compartments
                - e_rho_erlang * r_rho * E[rho_erlang] // moving through incubation compartments

              dC/dt =
                + e_rho_erlang * r_rho * E[e_rho_erlang - 1] // becoming infectious
                - r_nu * C // notification

              dQ/dt =
                + r_nu * C // notification
                - r_gamma * Q // loss of infectiousness

              dR/dt =
                + r_gamma * Q  // loss of infectiousness

              dZ/dt =
                + r_nu * C // accumulator for incidence
          }
      }

      sub observation {
        Inc ~ negbin(rep * Z, 1/p_phi)
      }

      sub parameter {
        p_R0 ~ uniform(lower = 1, upper = 5)
        p_phi ~ uniform(lower = 0, upper = 0.5)
      }

      sub initial {
        R <- 0
        C ~ uniform(lower = 0, upper = 400)
        Q <- 0
        E[rho_erlang] <- 0
        S <- p_N - C
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
            adapt_proposal(min=0.1, max=0.5, adapt="both") %>%
            sample(nsamples=100000)

        ## forecast
        pred <- predict(bi,
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
        mutate(model="Deterministic")

    return(df)
}
