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
        out <- sample(x$CiteScore, size = nrow(analysisSample), replace = T)
        tibble(
            Mean = mean(out),
            SD = sd(out)
        )
    }

    minScore <- citeScore %>%
        slice_max(CiteScore, n = nrow(.) * 0.1) %>%
        pull(CiteScore) %>%
        range() %>%
        .[1]

    randomSample <- citeScore %>%
        ungroup() %>%
        filter(CiteScore >= minScore) %>%
        distinct(across(c(Title, ISSN)), .keep_all = TRUE) %>%
        select(CiteScore)

    analysisSample <- aggregatePolicies %>%
        ungroup() %>%
        filter(CiteScore >= minScore) %>%
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
        x <- bootstrap(randomSample)
        return(x)
    }

    close(pb)

    return(boot.out)
}