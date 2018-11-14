##' Plots figure 3
##'
##' @param max_horizon maximal forecast horizon
##' @return plot
##' @importFrom dplyr %>% filter bind_rows mutate select_if
##' @importFrom tidyr spread
##' @importFrom scoringRules crps_sample dss_sample logs_sample
##' @importFrom cowplot get_legend plot_grid
##' @import ggplot2
##' @inheritParams assess_all_forecasts
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
figure3 <- function(max_horizon=5)
{
    scores <- assess_all_forecasts(max_horizon=max_horizon)

    p_calib <- ggplot(scores[["Calibration"]],
                      aes(x=horizon, y=mean, color=model, shape=model)) +
        geom_line() +
        geom_point() +
        scale_color_brewer("", palette="Set1") +
        scale_shape_discrete("") +
        geom_hline(yintercept=0.1, linetype="dashed") +
        geom_hline(yintercept=0.01, linetype="dotted") +
        ylab("Calibration") + xlab("Horizon (weeks)") +
        theme(legend.position="top")

    p_sharpness <- ggplot(scores[["Sharpness"]],
                          aes(x=horizon, y=mean, color=model, shape=model)) +
        geom_line() +
        geom_point() +
        scale_color_brewer(palette="Set1") +
        ylab("Sharpness") + xlab("Horizon (weeks)")

    p_bias <- ggplot(scores[["Bias"]],
                     aes(x=horizon, y=mean, color=model, shape=model)) +
        geom_line() +
        geom_point() +
        scale_color_brewer(palette="Set1") +
        ylim(c(-1, 1)) +
        ylab("Bias") + xlab("Horizon (weeks)") +
        geom_hline(yintercept=0, linetype="dashed")

    p_rps <- ggplot(scores[["RPS"]],
                    aes(x=horizon, y=mean, color=model, shape=model)) +
        geom_line() +
        geom_point() +
        expand_limits(y=0) +
        scale_color_brewer(palette="Set1") +
        ylab("RPS") + xlab("Horizon (weeks)")

    p_dss <- ggplot(scores[["DSS"]],
                    aes(x=horizon, y=mean, color=model, shape=model)) +
        geom_line() +
        geom_point() +
        scale_color_brewer(palette="Set1") +
        coord_cartesian(ylim=c(8, 20)) +
        ylab("DSS") + xlab("Horizon (weeks)")

    p_ae <- ggplot(scores[["AE"]] %>% filter(is.finite(mean)),
                     aes(x=horizon, y=mean, color=model, shape=model)) +
        geom_line() +
        geom_point() +
        expand_limits(y=0) +
        scale_color_brewer(palette="Set1") +
        ylab("AE") + xlab("Horizon (weeks)")

    pcol <- plot_grid(p_calib + theme(legend.position = "none") +
                      scale_x_continuous(""),
                      p_bias + theme(legend.position = "none") +
                      scale_x_continuous(""),
                      p_sharpness + theme(legend.position = "none") +
                      scale_x_continuous(""),
                      p_rps + theme(legend.position = "none") +
                      scale_x_continuous(""),
                      p_dss + theme(legend.position = "none"),
                      p_ae + theme(legend.position = "none") +
                      scale_x_continuous(""),
                      nrow = 2, labels = c("A", "B", "C", "D", "E", "F"))
    legend <- get_legend(p_calib)
    p <- plot_grid(legend, pcol, rel_heights = c(.15, 1), ncol=1)
    return(p)
}
