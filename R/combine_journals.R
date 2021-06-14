#' @export
combine_journals <- function(combined_cite, journal_policies) {
    box::use(
        dplyr[...],
        enumr[enum],
        . / helper[coalesce_join]
    )

    journal <- enum(
        hasPolicies = 1L,
        noPolicies = 0L,
        unknown = 999L
    )

    journal_policies <- journal_policies %>%
        select(-ISSN) %>%
        distinct(Title, .keep_all = TRUE) %>%
        mutate_at(
            vars(Submitted:Published),
            ~ case_when(
                . == TRUE ~ journal$hasPolicies,
                . == FALSE ~ journal$noPolicies,
                is.na(.) ~ journal$unknown,
                TRUE ~ journal$unknown
            )
        )

    x <- combined_cite %>%
        ungroup()

    df <- left_join(x, journal_policies)
    y <- df[is.na(df$Submitted), ] %>%
        select(
            Title,
            MatchTitle,
            ISSN,
            Publisher,
            DataTransparencyScore:OpenScienceBadgesScore
        )

    journal_policies <- journal_policies %>%
        rename(MatchTitle = Title)
    df2 <- left_join(y, journal_policies, by = "MatchTitle")
    df <- coalesce_join(df, df2, by = "Title")
    leftover <- df[is.na(df$Submitted), ]
    df <- anti_join(df, leftover, by = "Title")

    message(
        "Final number of journals : ",
        nrow(df %>% ungroup() %>% distinct(Title))
    )

    return(df)
}