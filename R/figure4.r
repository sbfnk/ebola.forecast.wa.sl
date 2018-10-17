##' Plots figure 4
##'
##' @param max_horizon maximal forecast horizon
##' @return plot
##' @importFrom dplyr %>% filter bind_rows mutate select_if
##' @importFrom scoringRules crps_sample dss_sample logs_sample
##' @importFrom cowplot get_legend plot_grid
##' @import ggplot2
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
figure4 <- function(max_horizon=2)
{
    df_sm <- samples_semi_mechanistic %>%
        filter(date > last_obs) %>%
        select(-R0) %>%
        filter((date - last_obs)/7 <= max_horizon) %>%
        mutate(best_model=if_else(stochasticity == "deterministic" &
                                  start_n_week_before == 0 &
                                  weeks_averaged == 1 &
                                  transmission_rate == "fixed", "yes", "no")) %>%
        mutate(best_model=factor(best_model, levels=c("yes", "no")),
               model=paste(stochasticity, start_n_week_before, weeks_averaged,
                           transmission_rate, sep="_"),
               model=factor(model))

    evol_sm_calib <- list()
    for (last_obs_loop in as.character(unique(df_sm$last_obs))) {
        evol_sm_calib[[last_obs_loop]] <- df_sm %>%
            filter(last_obs <= as.Date(last_obs_loop)) %>%
            assess_incidence_forecast(calibration) %>%
            mutate(up_to=as.Date(last_obs_loop))
    }

    evol_sm_calib <- bind_rows(evol_sm_calib) %>%
        mutate(horizon_string=paste0(horizon, " week", if_else(horizon == 1, "", "s"),
                                      " ahead"))

    df <- samples_semi_mechanistic %>%
        filter(date > last_obs) %>%
        filter(stochasticity == "deterministic" &
               start_n_week_before == 0 &
               weeks_averaged == 1 &
               transmission_rate == "fixed") %>%
        bind_rows(samples_bsts) %>%
        bind_rows(samples_deterministic) %>%
        bind_rows(samples_unfocused) %>%
        filter((date - last_obs)/7 <= max_horizon) %>%
        mutate(model=factor(model, levels=c("Semi-mechanistic",
                                            setdiff(model, "Semi-mechanistic")))) %>%
        select_if(~ !any(is.na(.)))

    evol_calib <- list()
    for (last_obs_loop in as.character(unique(df_sm$last_obs))) {
        evol_calib[[last_obs_loop]] <- df %>%
            filter(last_obs <= as.Date(last_obs_loop)) %>%
            assess_incidence_forecast(calibration) %>%
            mutate(up_to=as.Date(last_obs_loop))
    }

    evol_calib <- bind_rows(evol_calib) %>%
        mutate(horizon_string=paste0(horizon, " week", if_else(horizon == 1, "", "s"),
                                      " ahead"))

    p_sm_calib <-
        ggplot(evol_sm_calib %>% filter(best_model == "yes"),
               aes(x=up_to, y=mean, color=model, group=model)) +
        geom_line(data=evol_sm_calib %>% filter(best_model == "no"),
                  color="#F2BFC0") +
        geom_line() +
        geom_hline(yintercept=0.1, linetype="dashed") +
        geom_hline(yintercept=0.01, linetype="dotted") +
        scale_x_date("", date_breaks="2 months", date_labels="%b %Y") +
        scale_y_continuous("Calibration", limits=c(0, 1)) +
        theme(legend.position="none") +
        scale_color_brewer("", palette="Set1") +
        facet_grid(~horizon_string)

    p_calib <-
        ggplot(evol_calib, aes(x=up_to, y=mean, color=model, group=model)) +
        geom_line() +
        geom_hline(yintercept=0.1, linetype="dashed") +
        geom_hline(yintercept=0.01, linetype="dotted") +
        scale_x_date("", date_breaks="2 months", date_labels="%b %Y") +
        scale_y_continuous("Calibration", limits=c(0, 1)) +
        theme(legend.position="top") +
        scale_color_brewer("", palette="Set1") +
        facet_grid(~horizon_string)

    pcol <- plot_grid(p_sm_calib, p_calib + theme(legend.position="none"),
                      nrow = 2, labels = c("A", "B"))
    legend <- get_legend(p_calib)
    p <- plot_grid(legend, pcol, rel_heights = c(.15, 1), ncol=1)
    return(p)
}

