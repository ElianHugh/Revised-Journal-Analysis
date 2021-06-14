#' @export
analyse_similarity <- function(aggregated_policies, cite_score_dat) {
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
        out <- sample(x$CiteScore, size = nrow(analysis_sample), replace = TRUE)
        tibble(
            Mean = mean(out),
            SD = sd(out)
        )
    }

    min_score <- cite_score_dat %>%
        slice_max(CiteScore, n = nrow(.) * 0.1) %>%
        pull(CiteScore) %>%
        range() %>%
        .[1]

    random_sample <- cite_score_dat %>%
        ungroup() %>%
        filter(CiteScore >= min_score) %>%
        distinct(across(c(Title, ISSN)), .keep_all = TRUE) %>%
        select(CiteScore)

    analysis_sample <- aggregated_policies %>%
        ungroup() %>%
        filter(CiteScore >= min_score) %>%
        distinct(Title, .keep_all = TRUE) %>%
        select(CiteScore)

    set.seed(1234)
    iterations <- 100000

    pb <- new_bar(iterations)
    cores <- detectCores()
    cl <- makeCluster(cores, type = "SOCK")
    registerDoSNOW(cl)
    opts <- list(progress = (function(n) setTxtProgressBar(pb, n)))

    boot_out <- foreach(
        i = 1:iterations,
        .options.snow = opts,
        .errorhandling = "remove",
        .combine = "rbind",
        .packages = "tidyverse"
    ) %dopar% {
        x <- bootstrap(random_sample)
        return(x)
    }

    close(pb)

    return(boot_out)
}