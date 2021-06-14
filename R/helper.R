#' @export
coalesce_join <- function(x, y,
                          by = NULL, suffix = c(".x", ".y"),
                          join = dplyr::full_join, ...) {
    box::use(
        purrr[map_dfc],
        dplyr[coalesce, bind_cols]
    )

    joined <- join(x, y, by = by, suffix = suffix, ...)
    # names of desired output
    cols <- union(names(x), names(y))

    to_coalesce <- names(joined)[!names(joined) %in% cols]
    suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
    # remove suffixes and deduplicate
    to_coalesce <- unique(substr(
        to_coalesce,
        1,
        nchar(to_coalesce) - nchar(suffix_used)
    ))

    coalesced <- map_dfc(
        to_coalesce,
        ~ coalesce(
            joined[[paste0(.x, suffix[1])]],
            joined[[paste0(.x, suffix[2])]]
        )
    )
    names(coalesced) <- to_coalesce

    bind_cols(joined, coalesced)[cols]
}

# misc. matching function
# Returns false if both strings have no space
#' @export
checkmatch <- function(x, y) {
    box::use(stringr[str_detect])

    if (is.na(y)) {
        return(TRUE)
    } else {
        if ((str_detect(x, "\\s")) && (str_detect(y, "\\s"))) {
            return(TRUE)
        } else {
            if (!(str_detect(x, "\\s")) && !(str_detect(y, "\\s"))) {
                return(FALSE)
            } else {
                return(TRUE)
            }
        }
    }
}

#' @export
new_bar <- function(count) {
    box::use(utils[txtProgressBar])

    pb <- txtProgressBar(
        min = 0,
        max = count,
        style = 3
    )
    pb
}

#' @export
proper_issn <- function(x) {
    box::use(dplyr[...])

          x %>%
              gsub("[[:space:]]", "", .) %>%
              gsub("^(.{4})(.*)$", "\\1-\\2", .) %>%
              return()

}

#' @export
check_fuzzed_match <- function(x, col1, col2) {
    box::use(dplyr[...])

    listA <- x[col1][!is.na(x[col1])] %>%
        trimws("both")
    listB <- x[col2][!is.na(x[col2])] %>%
        trimws("both")

    matches <- sapply(
        listA,
        function(y) {
            sapply(listB, function(x) grepl(y, x, ignore.case = TRUE))
        }
    )
    return(matches)
}
