#' @export
graph_similarity_mean <- function(aggregated_policies, cite_score_dat, sample_sim, stats_boot) {
    box::use(
        dplyr[...],
        ggplot2[...],
        glue[glue],
        stats[density, quantile, sd],
        tidyr[gather],
        . / burgled[quantcut, theme_apa]
    )

    # Set up data for graphing ----------------------------

    min_score <- cite_score_dat %>%
        slice_max(CiteScore, n = nrow(.) * 0.1) %>%
        pull(CiteScore) %>%
        range() %>%
        .[1]
    var <- aggregated_policies %>%
        filter(CiteScore >= min_score) %>%
        distinct(Title, .keep_all = TRUE) %>%
        select(CiteScore)
    low_mean <- stats_boot %>%
        filter(Statistic == "Mean") %>%
        pull(Low)
    low_sd <- stats_boot %>%
        filter(Statistic == "SD") %>%
        pull(Low)
    high_mean <- stats_boot %>%
        filter(Statistic == "Mean") %>%
        pull(High)
    high_sd <- stats_boot %>%
        filter(Statistic == "SD") %>%
        pull(High)

    j_mean <- mean(var$CiteScore)

    # Graphing ---------------------------------------------------

    temp_graph <- sample_sim %>%
        ggplot() +
        geom_density(aes(Mean, fill = "red"))

    graph_dat <- layer_data(temp_graph, 1) %>%
        select(x, y, density)
    sample_location <- graph_dat$y[
        which(abs(graph_dat$x - j_mean) == min(abs(graph_dat$x - j_mean)))
    ]

    graph <- temp_graph +
        geom_segment(
            size = 1, color = "blue",
            aes(
                x = j_mean,
                xend = j_mean,
                y = .00,
                yend = sample_location + .01
            )
        ) +
        scale_y_continuous(expand = c(0, 0)) +
        theme_apa(base_size = 11) +
        theme(
            text = element_text(size = 20),
            plot.caption = element_text(hjust = 0),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            legend.position = "none"
        ) +
        xlab("Cite Score Means") +
            ylab("Density") +
            annotate(
                "text",
                x = 16.5,
                y = .45,
                label = glue(
                    "Iterations = 100,000 \n
                    Bootstrap Mean CI = ({low_mean}, {high_mean})\n
                    Bootstrap SD CI = ({low_sd}, {high_sd})"
                ),
                lineheight = 0.5,
                size = 7,
                hjust = 0
            ) +
            annotate(
                "text",
                x = j_mean + 1.5,
                y = sample_location + .05,
                label = glue(
                    "Journal sample mean ({round(j_mean, digits = 2)})"
                ),
                size = 6
            )

    return(graph)
}
