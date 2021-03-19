#' @export
analyse_similarity <- function(aggregatePolicies, citeScore) {
    box::use(
        dplyr[...],
        parallel[detectCores],
        snow[makeCluster],
        doSNOW[registerDoSNOW],
        utils[setTxtProgressBar],
        foreach[`%dopar%`, foreach],
        . / helper[new_bar]
    )

    bootstrap <- function(x) {
        out <- sample(x$CiteScore, size = nrow(var2), replace = T)
        tibble(
            Mean = mean(out),
            SD = sd(out)
        )
    }

    var <- citeScore %>%
        filter(Top10Perc == TRUE) %>%
        distinct(Title, .keep_all = TRUE) %>%
        select(CiteScore)

    var2 <- aggregatePolicies %>%
        filter(Top10Perc == TRUE) %>%
        distinct(Title, .keep_all = TRUE) %>%
        select(CiteScore)

    set.seed(1234)
    iterations <- 100000

    pb <- new_bar(iterations)
    cores <- detectCores()
    cl <- makeCluster(cores, type = "SOCK")
    registerDoSNOW(cl)
    opts <- list(progress = (function(n) setTxtProgressBar(pb, n)))

    boot.out <- foreach(
        i = 1:iterations,
        .options.snow = opts,
        .errorhandling = "remove",
        .combine = "rbind",
        .packages = "tidyverse"
    ) %dopar% {
        x <- bootstrap(var)
        return(x)
    }

    close(pb)

    return(boot.out)
}