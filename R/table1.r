##' Prints table 1
##'
##' @return table in latex format
##' @importFrom dplyr %>% mutate if_else filter rename select
##' @importFrom tidyr spread
##' @importFrom xtable xtable
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
table1 <- function()
{
    ## calibration
    samples_semi_mechanistic %>%
        select(-R0) %>%
        incidence_forecast_calibration %>%
        filter(horizon<5) %>%
        mutate(p_score=if_else(mean<0.01, 0, mean),
               p_score=as.character(round(p_score, 2)),
               p_score=if_else(p_score == "0", "<0.01", p_score)) %>%
        select(-sd, -mean) %>%
        spread(horizon, p_score) %>%
        rename(start="start_n_week_before", averaged="weeks_averaged",
               `transmission rate`="transmission_rate") %>%
        mutate(averaged=if_else(averaged == 1, "no", paste(averaged, "weeks")),
               start=if_else(start==0, "at last data point", "1 week before")) %>%
        xtable %>%
        print(include.rownames=FALSE)
}

