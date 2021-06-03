#' @export
graph_citeridge <- function(df, citeScoreDat) {
    box::use(
        dplyr[...],
        ggridges[stat_density_ridges, theme_ridges],
        ggplot2[...],
        broom[tidy],
        stats[pairwise.wilcox.test],
        forcats[fct_reorder],
        ./burgled[theme_apa]
    )

    # Calculate the top 10 percent of journals from the cite score data
    # as the Top10Perc column is inaccurate
    minScore <- citeScoreDat %>%
        slice_max(CiteScore, n = nrow(.) * 0.1) %>%
        pull(CiteScore) %>%
        range() %>%
        .[1]

    df <- df %>%
        filter(CiteScore >= minScore)

    totalN <- df %>%
        ungroup() %>%
        distinct(Title, .keep_all = TRUE) %>%
        count() %>%
        sum(.$n)

    # * TODO fix this
    df <- df %>%
        group_by(ScoreGrade) %>%
        mutate(N = paste0(ScoreGrade, "\n(n = ", n(), ")"))

    pal <- c(
        "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
        "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"
    )

    title <- sprintf(
        "Distribution of Cite Score with Open Science Implementation \n (n = %d)",
        totalN
    )

    citeGraph <- df %>%
        ggplot(aes(
            y = fct_reorder(N, ScoreMax, .desc = TRUE),
            x = CiteScore,
            fill = factor(stat(quantile))
        )) +
        stat_density_ridges(
            jittered_points = TRUE,
            position = "raincloud",
            alpha = 0.5,
            scale = 1.5,
            geom = "density_ridges_gradient",
            calc_ecdf = TRUE,
            quantiles = 4,
            quantile_lines = TRUE
        ) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_discrete(expand = c(0, 0)) +
        theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
        theme_apa() +
        ylab("Open Science Policy") +
        scale_fill_manual(values = pal) +
        theme(
            axis.ticks = element_blank(),
            text = element_text(size = 20),
            legend.position = "none",
            plot.caption = element_text(hjust = 0),
            plot.title.position = "plot",
            plot.caption.position = "plot"
        )

    citeGraph
}