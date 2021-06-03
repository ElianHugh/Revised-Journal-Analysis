# Calculate an open science score by aggregating
# open science policies across cases.
# These are then grouped together by cutting into quantiles
#' @export
aggregate_policies <- function(df) {
    box::use(
        dplyr[...],
        . / burgled[quantcut]
    )

    df$OSS <- df %>%
        select(DataCitation:Badges, Submitted, Accepted, Published) %>%
        rowSums(na.rm = TRUE)

    df <- df %>%
     mutate(
        ScoreGrade = quantcut(df$OSS, q = 5)
    ) %>%
        group_by(ScoreGrade) %>%
        mutate(ScoreMin = min(OSS), ScoreMax = max(OSS)) %>%
        add_count(name = "ScoreGradeN") %>%
        ungroup()

    levels(df$ScoreGrade) <- c(
        "Very Low",
        "Low",
        "Medium",
        "High",
        "Very High"
    )

    df$Title <- coalesce(df$MatchTitle, df$Title)

    df <- df %>%
        distinct(Title, .keep_all = TRUE) %>%
        select(
            -DataCitation:-Badges,
            -Submitted:-Published,
            -MatchTitle,
            -SubjectArea,
            -Publisher,
            -ISSN
        )


    return(df)
}