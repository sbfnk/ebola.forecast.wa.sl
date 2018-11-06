##' Plots figure 1
##'
##' @param conf.levels Confidence levels to show
##' @return plot
##' @importFrom magrittr %>%
##' @importFrom tidyr gather unnest spread
##' @importFrom purrr map
##' @importFrom tibble enframe
##' @importFrom dplyr filter ungroup
##' @importFrom cowplot plot_grid
##' @import ggplot2
##' @author Sebastian Funk \email{sebastian.funk@lshtm.ac.uk}
figure1 <- function(conf.levels=c(0, 0.5, 0.9))
{
    quantiles <- c((1-conf.levels)/2,(1+conf.levels)/2) %>% unique %>% sort

    ## calculate confidence levels
    ff_conf <- samples_semi_mechanistic %>%
        gather(variable, value, R0, cases) %>%
        mutate(variable=factor(variable)) %>%
        calculate_quantiles(quantiles)

    ff_conf_fit <- ff_conf %>%
        ungroup %>%
        filter(stochasticity=="stochastic",
               start_n_week_before==0,
               weeks_averaged==1,
               transmission_rate=="fixed",
               last_obs==max(last_obs))

    p_fit_plot <- list()
    for (var in levels(ff_conf_fit$variable))
    {
        p_fit_plot[[var]] <- ggplot(ff_conf_fit %>% filter(variable==var), aes(x=date)) +
            geom_ribbon(aes(ymin=`25%`, ymax=`75%`), alpha=0.5) +
            geom_ribbon(aes(ymin=`5%`, ymax=`95%`), alpha=0.25) +
            geom_line(aes(y=`50%`)) +
            scale_x_date("", date_breaks="2 months", date_labels="%b %Y",
                         limits=range(ebola_wa$date))
    }

    p_fit_plot[["cases"]] <-
        p_fit_plot[["cases"]] +
        ylab("Weekly incidence") +
        geom_point(data=ebola_wa, aes(y=incidence), colour="black")

    p_fit_plot[["R0"]] <-
        p_fit_plot[["R0"]] +
        ylab(expression(R[0])) +
        geom_hline(yintercept=1, linetype="dashed") +
        coord_cartesian(ylim=c(0, 6))

    p <- plot_grid(p_fit_plot[["cases"]], p_fit_plot[["R0"]], labels=c("A", "B"), nrow=1)
    return(p)
}

