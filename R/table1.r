##' Prints table 1
##'
##' @return table in latex format
##' @importFrom dplyr %>% mutate if_else filter rename select
##' @importFrom tidyr spread
##' @importFrom kableExtra kable add_header_above
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
##' @export
table1 <- function()
{
    ## calibration
    samples_semi_mechanistic %>%
        select(-R0) %>%
        assess_incidence_forecast(calibration) %>%
        select(-data, -model) %>%
        filter(horizon<5) %>%
        mutate(p_score=if_else(mean<0.01, 0, mean),
               p_score=as.character(round(p_score, 2)),
               p_score=if_else(as.numeric(p_score) >= 0.1,
                               paste0("{\\bfseries ", p_score, "}"), p_score),
               p_score=if_else(p_score == "0", "$<$0.01", p_score)) %>%
        select(-sd, -mean) %>%
        spread(horizon, p_score) %>%
        select(stochasticity, start_n_week_before, transmission_rate, weeks_averaged,
               `1`, `2`, `3`, `4`) %>%
        rename(start="start_n_week_before", averaged="weeks_averaged",
               `transmission`="transmission_rate") %>%
        mutate(averaged=if_else(averaged == 1, "no", paste(averaged, "weeks")),
               start=if_else(start==0, "at last data point", "1 week before")) %>%
        arrange(desc(stochasticity)) %>%
        kable("latex", align=c("l", "l", "l", "l", "|r", "r", "r", "r"),
              booktabs=TRUE, escape=FALSE, linesep="") %>%
        add_header_above(c("Predictive model variant"=4, "Forecast horizon (weeks)"=4))
}

