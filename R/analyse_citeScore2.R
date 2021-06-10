box::use(
    dplyr[...],
    fuzzyjoin[stringdist_left_join],
    glue[glue],
    purrr[map_df],
    enumr[enum],
    topapi[...],
    . / helper[coalesce_join, checkmatch],
)

#' @export
analyse_citeScore <- function(cite_score_dat) {

    # find citescore matches via ISSN
    issn_df <- topapi::journal_issn(cite_score_dat$ISSN) %>%
        full_join(
            select(cite_score_dat, c(CiteScore, MatchTitle = Title, ISSN)),
            by = "ISSN"
        ) %>%
        filter(!is.na(Total), !is.na(CiteScore))

    # find citescore matches via name
    name_df <- topapi::journal_name(cite_score_dat$Title) %>%
        full_join(
            select(cite_score_dat, c(CiteScore, MatchTitle = Title)),
            by = c("Title" = "MatchTitle")
        ) %>%
        filter(!is.na(Total), !is.na(CiteScore))


    combined_df <- full_join(issn_df, name_df)

    # Fuzzy matching -------------------------------------------------------  #

    leftover_names <- anti_join(topapi::full_data(), combined_df, by = "Title")
    fuzzy_scopus <- find_in_scopus(
        leftover_names$Title,
        cite_score_dat,
        method = "lv",
        max_distance = 4
    )

    fuzzy_df <- full_join(
        leftover_names,
        select(fuzzy_scopus, c(CiteScore, MatchTitle = Title, OriginalTitle)),
        by = c("Title" = "OriginalTitle")
    ) %>%
        filter(!is.na(Total), !is.na(CiteScore)) %>%
        add_row(combined_df) %>%
        distinct(Title, .keep_all = TRUE)

    leftover_names <- anti_join(topapi::full_data(), fuzzy_df, by = "Title")
    fuzzy_scopus <- find_in_scopus(
        leftover_names$Title,
        cite_score_dat,
        method = "lv",
        max_distance = 50
    )

    # sanity check the matched title with the original title
    out <- tibble(
        Title = character(),
        CiteScore = numeric(),
        `Scopus Sub-Subject Area` = character(),
        Publisher = character(),
        Top10Perc = numeric(),
        ISSN = character(),
        distance = numeric(),
        OriginalTitle = character()
    )
    for (i in seq_len(nrow(fuzzy_scopus))) {
        iteration <- grep(
            fuzzy_scopus$Title[i],
            fuzzy_scopus$OriginalTitle[i],
            ignore.case = TRUE
        )
        if (length(iteration) > 0 && !is.na(fuzzy_scopus[i, ]$Title)) {
            out <- rbind(out, fuzzy_scopus[i, ])
        }
    }
    # Incorrect match filter + join to fuzzy
    out <- out %>%
        filter(OriginalTitle != "Nature Food") %>%
        full_join(fuzzy_df)

    # Finish up ---------------------------------------------------------  #

    combined_df <- full_join(
        combined_df,
        out
    ) %>%
        distinct(Title, .keep_all = TRUE)

}


find_in_scopus <- function(input, comparison, method = "osa", max_distance = 3) {
    # is x scalar?
    if (length(input) == 1) {
        find_scopus_helper(input, comparison, max_distance, method)
    } else {
        return(
            purrr::map_dfr(
                .x = input,
                .f = find_scopus_helper,
                comparison,
                max_distance,
                method
            )
        )
    }
}

find_scopus_helper <- function(input, comparison, max_distance, method) {
    comparison$distance <- stringdist::stringdist(
        a = tolower(comparison$Title),
        b = tolower(input),
        method = method
    )

    fuzzy_name <- comparison[which.min(comparison$distance), ]

    tryCatch(
        expr = {
            # code
            if (fuzzy_name$distance > max_distance) {
                return(NULL)
            }
            possible_journal <- comparison[
                comparison$Title == fuzzy_name$Title,
            ]
            possible_journal$OriginalTitle <- input
            return(possible_journal)
        }, warning = function(w) {
            message(
                sprintf("Warning in %s: %s", deparse(w[["call"]]), w[["message"]])
            )
        }, error = function(e) {
            message(
                sprintf("Error in %s: %s", deparse(e[["call"]]), e[["message"]])
            )
        }
    )
}