#' @export
get_citeScore <- function() {
    box::use(
        readr[read_csv, col_skip, cols],
        magrittr[`%<>%`],
        dplyr[mutate, rename, `%>%`],
        tidyr[pivot_longer, drop_na],
        withr[with_options],
        stringr[str_pad]
    )

    dat <- read_csv("Data/CiteScore.csv",
        col_types = cols(
            `Scopus Source ID` = col_skip(),
            `Scholarly Output` = col_skip(),
            `Percent Cited` = col_skip(),
            SNIP = col_skip(),
            `Scopus ASJC Code (Sub-subject Area)` = col_skip(),
            Percentile = col_skip(),
            RANK = col_skip(),
            `Rank Out Of` = col_skip(),
            Type = col_skip(),
            `Open Access` = col_skip(),
            Quartile = col_skip(),
            `URL Scopus Source ID` = col_skip(),
            `Citation Count` = col_skip(),
            SJR = col_skip()
        )
    )

    dat %<>%
        pivot_longer(
            c(`Print ISSN`, `E-ISSN`),
            names_repair = "unique"
        ) %>%
        rename(
            ISSN = value,
            Top10Perc = `Top 10% (CiteScore Percentile)`
        ) %>%
        mutate(
            name = NULL,
            ISSN = with_options(
                c(scipen = 999),
                str_pad(.$ISSN, 8, pad = "0")
            )
        ) %>%
            drop_na(ISSN)

    return(dat)
}