box::use(
    dplyr[...],
    fuzzyjoin[stringdist_left_join],
    glue[glue],
    purrr[map_df],
    enumr[enum],
    topapi,
    . / helper[coalesce_join, checkmatch, proper_issn],
)

#' @export
analyse_citeScore <- function(cite_score_dat) {

    cite_score_dat <- cite_score_dat %>%
        mutate(ISSN = proper_issn(.$ISSN)
    )

    # Search for matches via pairing dataset ISSNs
    issn_df <- topapi$journal_issn(cite_score_dat$ISSN) %>%
        full_join(
            select(cite_score_dat, c(CiteScore, ISSN)),
            by = "ISSN"
        ) %>%
        filter(!is.na(Total), !is.na(CiteScore))

    # Search for matches via pairing names
    name_df <- topapi$journal_name(cite_score_dat$Title) %>%
        full_join(
            select(cite_score_dat, c(CiteScore, MatchTitle = Title)),
            by = c("Title" = "MatchTitle")
        ) %>%
        filter(!is.na(Total), !is.na(CiteScore))

    combined_df <- full_join(issn_df, name_df) %>%
        distinct(Title, .keep_all = TRUE)

    # Fuzzy matching -------------------------------------------------------  #
    # Here, we iterate over the remaining names in the TOP dataset,
    # iteratively searching for names in the Scopus dataaset that are similar
    # in spelling. We measure string distance via levenshtein distance (lv)

    leftover_names <- anti_join(topapi$full_data(), combined_df, by = "Title")
    fuzzy_scopus <- find_in_scopus(
        leftover_names$Title,
        cite_score_dat,
        method = "lv",
        max_distance = 4
    )

    fuzzy_df <- full_join(
        leftover_names,
        select(fuzzy_scopus, c(CiteScore, MatchTitle = Title, Title = OriginalTitle)),
        by = "Title"
    ) %>%
        filter(!is.na(Total), !is.na(CiteScore)) %>%
        add_row(combined_df) %>%
        distinct(Title, .keep_all = TRUE)

    leftover_names <- anti_join(topapi$full_data(), fuzzy_df, by = "Title")
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
        matched_title <- fuzzy_scopus$Title[i]
        original_title <- fuzzy_scopus$OriginalTitle[i]

        # Alter characters for easier grepping
        # such as "&" to "and"
        # We do it to both just in case!
        matched_title <- gsub("&", "and", matched_title) %>%
            gsub("[^[:alnum:][:space:]]", "", .)

        original_title <- gsub("&", "and", original_title) %>%
            gsub("[^[:alnum:][:space:]]", "", .)

        iteration <- grep(
            matched_title,
            original_title,
            ignore.case = TRUE
        )
        if (length(iteration) > 0 && !is.na(fuzzy_scopus[i, ]$Title)) {
            out <- rbind(out, fuzzy_scopus[i, ])
        }
    }
    # Incorrect match filter + join to fuzzy
    out <- out %>%
        full_join(fuzzy_df)

    # Finish up ---------------------------------------------------------  #

    combined_df <- full_join(
        combined_df,
        out
    ) %>%
        distinct(Title, .keep_all = TRUE) %>%
        select(
            Title,
            MatchTitle,
            Publisher,
            ISSN,
            DataCitationScore:OpenScienceBadgesScore,
            CiteScore
        ) %>%
        mutate_at(vars(DataCitationScore:OpenScienceBadgesScore), ~ case_when(
            . == 3 ~ 1,
            . == 2 ~ 1,
            . == 1 ~ 1,
            . == 0 ~ 0
        )) %>%
        filter(
                Title != "Brain Communications",
                Title != "Meta-Psychology",
                Title != "Nature Food"
        )
    return(combined_df)

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

