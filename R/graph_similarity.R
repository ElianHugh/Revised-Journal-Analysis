#' @export
graph_similarity_mean <- function(aggregatePolicies, citeScoreDat, boot.out, boot.stat) {
    box::use(
        dplyr[...],
        ggplot2[...],
        gtools[quantcut],
        papaja[theme_apa],
        glue[glue],
        stats[density, quantile, sd]
    )

    minScore <- citeScoreDat %>%
        slice_max(CiteScore, n = nrow(.) * 0.1) %>%
        pull(CiteScore) %>%
        range() %>%
        .[1]

    var <- aggregatePolicies %>%
        filter(CiteScore >= minScore) %>%
        distinct(Title, .keep_all = TRUE) %>%
        select(CiteScore)

    jMean <- mean(var$CiteScore)
    densMean <- density(boot.out$Mean)
    lowMean <- boot.stat %>%
        filter(Statistic == "Mean") %>%
        pull(Low)
    highMean <- boot.stat %>%
        filter(Statistic == "Mean") %>%
        pull(High)

    df <- data.frame(x = densMean$x, y = densMean$y)
    probs <- c(0, 0.25, 0.5, 0.75, 1)
    quantiles <- quantile(boot.out$Mean, prob = probs)
    loc <- df$y[which(abs(df$x - jMean) == min(abs(df$x - jMean)))]

    df$quant <- factor(findInterval(df$x, quantiles))
    ggplot(df, aes(x, y)) +
        geom_line() +
        geom_segment(size = 1, color = "red", aes(x = jMean, xend = jMean, y = .04, yend = loc)) +
        geom_ribbon(aes(ymin = 0, ymax = y, fill = quant)) +
        scale_fill_brewer(guide = "none") +
        scale_y_continuous(expand = c(0, 0)) +
        theme_apa(base_size = 11) +
        theme(
            text = element_text(size = 20),
            plot.caption = element_text(hjust = 0),
            plot.title.position = "plot",
            plot.caption.position = "plot"
        ) +
        xlab("Cite Score Means") +
        ylab("Density") +
        annotate("text", x = jMean + 0.9, y = loc + .05, label = "Journal sample mean", size = 6) +
        annotate("text", x = 12 + 0.9, y = .4, label = glue("Bootstrap Mean CI = ({lowMean}, {highMean})"), size = 6) +
        annotate("text", x = 12 + 0.9, y = .3, label = "Iterations = 100,000", size = 6)
}
