##' Plots figure 3
##'
##' @param max_horizon maximal forecast horizon
##' @return plot
##' @importFrom dplyr %>% filter bind_rows mutate select_if
##' @importFrom scoringRules crps_sample dss_sample logs_sample
##' @importFrom cowplot get_legend plot_grid
##' @import ggplot2
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
figure3 <- function(max_horizon=5)
{
    df <- samples_semi_mechanistic %>%
        filter(stochasticity == "deterministic" &
               start_n_week_before == 0 &
               weeks_averaged == 1 &
               transmission_rate == "fixed") %>%
        bind_rows(samples_bsts) %>%
        bind_rows(samples_deterministic) %>%
        bind_rows(samples_unfocused) %>%
        mutate(model=factor(model, levels=c("Semi-mechanistic",
                                            setdiff(model, "Semi-mechanistic")))) %>%
        filter((date - last_obs)/7 <= max_horizon) %>%
        select_if(~ !any(is.na(.)))

    df_calibration <- df %>%
        assess_incidence_forecast(calibration)

    df_sharpness <- df %>%
        assess_incidence_forecast(sharpness, interval=0.5) %>%
        ## assess_incidence_forecast(sharpness, interval=c(0.5, 0.9)) %>%
        spread(interval, width) %>%
        group_by(model, horizon) %>%
        summarise(`0.5`=mean(`0.5`))
    ## summarise(`0.5`=mean(`0.5`), `0.9`=mean(`0.9`))

    df_bias <- df %>%
        assess_incidence_forecast(bias) %>%
        group_by(model, horizon) %>%
        summarise(bias=mean(bias))

    df_crps <- df %>%
        assess_incidence_forecast(crps_sample) %>%
        group_by(model, horizon) %>%
        summarise(crps=mean(score))

    df_dss <- df %>%
        assess_incidence_forecast(dss_sample) %>%
        group_by(model, horizon) %>%
        summarise(dss=mean(score))

    df_logs <- df %>%
        assess_incidence_forecast(logs_sample) %>%
        group_by(model, horizon) %>%
        summarise(logs=mean(score))

    p_calib <- ggplot(df_calibration, aes(x=horizon, y=mean, color=model)) +
        geom_line() +
        geom_point() +
        scale_color_brewer("", palette="Set1") +
        geom_hline(yintercept=0.1, linetype="dashed") +
        geom_hline(yintercept=0.01, linetype="dotted") +
        ylab("Calibration") + xlab("Horizon (weeks)") +
        theme(legend.position="top")

    p_sharpness <- ggplot(df_sharpness, aes(x=horizon, color=model)) +
        geom_line(aes(y=`0.5`), linetype="solid") +
        ## geom_line(aes(y=`0.9`), linetype="dashed") +
        geom_point(aes(y=`0.5`)) +
        ## geom_point(aes(y=`0.9`)) +
        scale_color_brewer(palette="Set1") +
        ## scale_y_log10() +
        ylab("Sharpness") + xlab("Horizon (weeks)")

    p_bias <- ggplot(df_bias, aes(x=horizon, y=bias, color=model)) +
        geom_line() +
        geom_point() +
        scale_color_brewer(palette="Set1") +
        ylim(c(-1, 1)) +
        ylab("Bias") + xlab("Horizon (weeks)") +
        geom_hline(yintercept=0, linetype="dashed")

    p_crps <- ggplot(df_crps, aes(x=horizon, y=crps, color=model)) +
        geom_line() +
        geom_point() +
        scale_color_brewer(palette="Set1") +
        ylab("RPS") + xlab("Horizon (weeks)")

    p_dss <- ggplot(df_dss, aes(x=horizon, y=dss, color=model)) +
        geom_line() +
        geom_point() +
        scale_color_brewer(palette="Set1") +
        coord_cartesian(ylim=c(8, 20)) +
        ylab("DSS") + xlab("Horizon (weeks)")

    p_logs <- ggplot(df_logs, aes(x=horizon, y=logs, color=model)) +
        geom_line() +
        geom_point() +
        scale_color_brewer(palette="Set1") +
        coord_cartesian(ylim=c(5, 15)) +
        ylab("LogS") + xlab("Horizon (weeks)")

    pcol <- plot_grid(p_calib + theme(legend.position = "none") +
                      scale_x_continuous(""),
                      p_sharpness + theme(legend.position = "none") +
                      scale_x_continuous(""),
                      p_bias + theme(legend.position = "none") +
                      scale_x_continuous(""),
                      p_crps + theme(legend.position = "none") +
                      scale_x_continuous(""),
                      p_dss + theme(legend.position = "none"),
                      p_logs + theme(legend.position = "none") +
                      scale_x_continuous(""),
                      nrow = 2, labels = c("A", "B", "C", "D", "E", "F"))
    legend <- get_legend(p_calib)
    p <- plot_grid(legend, pcol, rel_heights = c(.15, 1), ncol=1)
    return(p)
}

