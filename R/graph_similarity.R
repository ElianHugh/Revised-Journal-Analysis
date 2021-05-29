#' @export
graph_similarity_mean <- function(aggregatedPolicies, citeScoreDat, sampleSim, statsBoot) {
    box::use(
        dplyr[...],
        ggplot2[...],
        glue[glue],
        stats[density, quantile, sd],
        tidyr[gather],
        . / burgled[quantcut]
    )

    minScore <- citeScoreDat %>%
        slice_max(CiteScore, n = nrow(.) * 0.1) %>%
        pull(CiteScore) %>%
        range() %>%
        .[1]
    var <- aggregatedPolicies %>%
        filter(CiteScore >= minScore) %>%
        distinct(Title, .keep_all = TRUE) %>%
        select(CiteScore)
    lowMean <- statsBoot %>%
        filter(Statistic == "Mean") %>%
        pull(Low)
    highMean <- statsBoot %>%
        filter(Statistic == "Mean") %>%
        pull(High)
    jMean <- mean(var$CiteScore)

    tempGraph <- sampleSim %>%
        ggplot(aes(Mean, fill = "red")) +
        geom_density()

    graphDat <- layer_data(tempGraph, 1) %>%
        select(x, y, density)
    sampleLocation <- graphDat$y[
        which(abs(graphDat$x - jMean) == min(abs(graphDat$x - jMean)))
    ]

    graph <- tempGraph +
        geom_segment(size = 1, color = "blue", aes(x = jMean, xend = jMean, y = .00, yend = sampleLocation + .01)) +
        scale_y_continuous(expand = c(0, 0)) +
        theme_minimal(base_size = 11) +
        theme(
            text = element_text(size = 20),
            plot.caption = element_text(hjust = 0),
            plot.title.position = "plot",
            plot.caption.position = "plot",
            legend.position = "none"
        ) +
        xlab("Cite Score Means") +
        ylab("Density") +
        annotate("text", x = 16.5 + 0.9, y = .4, label = glue("Iterations = 100000 \nBootstrap Mean CI = ({lowMean}, {highMean})"), size = 7) +
        annotate("text", x = jMean + 1.5, y = sampleLocation + .02, label = "Journal sample mean", size = 7)

    return(graph)
}
