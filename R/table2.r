##' Prints table 2
##'
##' @return table in latex format
##' @importFrom dplyr %>% mutate filter rename select arrange
##' @importFrom tidyr spread
##' @importFrom kableExtra kable group_rows
##' @inheritParams assess_all_forecasts
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
table2 <- function(max_horizon=3)
{
    scores <- assess_all_forecasts(max_horizon=max_horizon)

    ## combine metrics
    df_metrics <- scores[["Calibration"]] %>%
        mutate(score="Calibration") %>%
        select(model, horizon, mean, score)

    for (score in setdiff(names(scores), "Calibration")) {
        df_metrics <- df_metrics %>%
            bind_rows(scores[[score]] %>% mutate(score=score))
    }

    ## print table
    df_metrics %>%
        rename(Model=model) %>%
        mutate(score=factor(score, levels=unique(score))) %>%
        spread(score, mean) %>%
        arrange(horizon, Model) %>%
        mutate(Calibration=if_else(Calibration<0.01, 0, Calibration),
               Calibration=as.character(round(Calibration, 2)),
               Calibration=if_else(as.numeric(Calibration) >= 0.1,
                               paste0("{\\bfseries ", Calibration, "}"), Calibration),
               Calibration=if_else(Calibration == "0", "$<$0.01", Calibration)) %>%
        mutate(Bias=signif(Bias, 2),
               Sharpness=signif(Sharpness, 2),
               RPS=signif(RPS, 2),
               DSS=signif(DSS, 2),
               AE=signif(AE, 2)) %>%
        select(-horizon) %>%
        kable("latex", align=c("l", "r", "r", "r", "r", "r", "r", "r"),
              format.args=list(drop0trailing=TRUE), booktabs=TRUE, escape=FALSE) %>%
        group_rows("1 week ahead", 1, 4) %>%
        group_rows("2 weeks ahead", 5, 8) %>%
        group_rows("3 weeks ahead", 9, 12)
}

