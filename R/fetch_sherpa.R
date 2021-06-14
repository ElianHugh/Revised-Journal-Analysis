# 1. We take in a list of journals
# 2. We submit the journals to the Sherpa API via parallel processing
# 3. We then filter the returned JSON object for policies, and change it
#    into a dataframe
# 4. If there are no policies for a given journal, we attempt
#    to find more by trying: inputting ISSN instead of title,
#    trying a modified title, and trying to find the journal
#    through the publisher (using string distance to see if the
#    journal exists)
# 5. All policies found are then returned as a single dataframe
#    for further analysis

box::use(
    dplyr[...],
    tibble[rownames_to_column, add_case],
    rlist[list.filter, list.select, list.rbind],
    purrr[flatten, map2],
    tidyr[unnest_longer, unnest, gather],
    parallel[detectCores],
    snow[makeCluster, stopCluster],
    doSNOW[registerDoSNOW],
    foreach[registerDoSEQ, foreach, `%dopar%`],
    fuzzyjoin[stringdist_left_join],
    utils[txtProgressBar, setTxtProgressBar],
    stringr[str_replace, str_replace_all],
    enumr[enum],
    . / helper[new_bar, proper_issn, check_fuzzed_match]
)

# Match journals by title
#' @export
fetch_sherpa_parse1 <- function(combined_cite, key) {
    journals <- combined_cite %>%
        ungroup() %>%
        mutate(ISSN = ISSN %>% proper_issn())
    next_journals <- journals %>%
        select(Title) %>%
        distinct()

    cat("\nInitial journals : ", as.character((nrow(next_journals)), "\n"))
    count <- length(next_journals$Title)
    API <- fetch_json(count, next_journals$Title, key)

    if (length(API) > 0) {
        parse1 <- explore_json(API)
        return(parse1)
    } else {
        message("\nDid not find any journals at parse 1.")
        return(NULL)
    }
}

# Match journals by matchtitle
#' @export
fetch_sherpa_parse2 <- function(combined_cite, key, parse1) {
    next_journals <- combined_cite %>%
        get_leftover_journals(parse1) %>%
        distinct(MatchTitle, .keep_all = TRUE)
    message("\n* PARSE 2 OF 5 * Finding journals by matchtitle\n")
    # * Initiate API fetching
    count <- length(next_journals$MatchTitle)
    API <- fetch_json(count, next_journals$MatchTitle, key)

    if (length(API) > 0) {
        parse2 <- explore_json(API)
        return(parse2)
    } else {
        message("\nDid not find any journals at parse 2.")
        return(NULL)
    }
}

# Match journals via ISSN
#' @export
fetch_sherpa_parse3 <- function(combined_cite, key, parse1, parse2) {
    next_journals <- combined_cite %>%
        get_leftover_journals(parse1, parse2) %>%
        distinct(ISSN, .keep_all = TRUE) %>%
        mutate(
            ISSN = ISSN %>%
            proper_issn()

        )
    cat("\nISSNs left : ", as.character((nrow(next_journals))))
    message("\n* PARSE 3 OF 5 * Finding journals by ISSN\n")
    # * Initiate API fetching
    count <- length(next_journals$ISSN)
    API <- fetch_json(count, next_journals$ISSN, key, is_issn = TRUE)

    if (length(API) > 0) {
        parse3 <- explore_json(API)
        return(parse3)
    } else {
        message("\nDid not find any journals at parse 3.")
        return(NULL)
    }
}

# Match journals via modified titles
#' @export
fetch_sherpa_parse4 <- function(combined_cite, key, parse1, parse2, parse3) {
    next_journals <- combined_cite %>%
        get_leftover_journals(parse1, parse2, parse3) %>%
        distinct(Title, .keep_all = TRUE) %>%
        mutate(
            Title = str_replace(Title, " and ", " & ")
        )
    message("\n* PARSE 4 OF 5 * Finding journals by modified title\n")

    # * Initiate API fetching
    count <- length(next_journals$Title)
    API <- fetch_json(count, next_journals$Title, key)

    if (length(API) > 0) {
        parse4 <- explore_json(API)
        return(parse4)
    } else {
        message("\nDid not find any journals at parse 4.")
        return(NULL)
    }
}

# Match journals via publisher
#' @export
fetch_sherpa_parse5 <- function(combined_cite, key, parse1, parse2, parse3, parse4) {
    next_journals <- combined_cite %>%
        get_leftover_journals(parse1, parse2, parse3, parse4) %>%
        distinct(MatchTitle, .keep_all = TRUE)

    next_publishers <- next_journals %>%
        ungroup() %>%
        distinct(Publisher, .keep_all = TRUE)

    message("\n* PARSE 5 OF 6 * Finding journals through publisher matching \n")

    # * Initiate API fetching
    count <- length(next_publishers$Publisher)
    API <- fetch_json(count, next_publishers$Publisher, key, is_publisher = TRUE)
    count <- length(API)

    # * Prep for explore
    if (count > 0) {
        explore_publisher <- function(API, i) {
            temp <- flatten(API)
            if ("publications" %in% names(temp)) {
                temp <- temp %>%
                    as_tibble(.name_repair = "minimal") %>%
                    select(publications) %>%
                    unnest(cols = (publications)) %>%
                    select(title) %>%
                    unnest(cols = (title)) %>%
                    select(title)
            }
            else {
                temp <- NULL
            }
            temp
        }
    }

    parse5 <- map2(API, seq_along(API), .f = explore_publisher)
    parse5 <- unlist(parse5, use.names = FALSE)
    cat("\n Publisher journals found: ", length(parse5))

    temp_df <- as.data.frame(parse5) %>%
        rename(Title = parse5)
    temp_df <- anti_join(temp_df, combined_cite)

    if (!is.null(parse1)) temp_df <- anti_join(temp_df, parse1, by = "Title")
    if (!is.null(parse2)) temp_df <- anti_join(temp_df, parse2, by = "Title")
    if (!is.null(parse3)) temp_df <- anti_join(temp_df, parse3, by = "Title")
    if (!is.null(parse4)) temp_df <- anti_join(temp_df, parse4, by = "ISSN")

    # ! Look for matches of title
    fuzzydf <- stringdist_left_join(
        next_journals,
        temp_df,
        by = "Title",
        distance_col = "Distance",
        max_dist = 20,
        ignore_case = TRUE
    ) %>%
    distinct(Title.x, .keep_all = TRUE)

    match_journals <- check_fuzzed_match(fuzzydf, "Title.x", "Title.y")
    match_journals <- match_journals %>%
        as.data.frame() %>%
        rownames_to_column("id") %>%
        gather(key = "key", value = "value", -id) %>%
        filter(value == TRUE) %>%
        distinct(id, .keep_all = TRUE)

    match_journals$id <- str_replace_all(match_journals$id, "[^[:alnum:]]", " ")
    match_journals$id <- trimws(match_journals$id, which = "both")
    match_journals$key <- str_replace_all(match_journals$key, "[^[:alnum:]]", " ")
    match_journals$key <- str_replace_all(match_journals$key, "[0-9]+", " ")
    match_journals$key <- trimws(match_journals$key, which = "both")

    if (nrow(match_journals) > 0) {
        cat("\nJournals matched: ", nrow(match_journals))

        nextJournals <- match_journals %>%
            select(id)

        message("\n* PARSE 5 OF 5 * Matching journals \n")
        # * Initiate API fetching
        count <- nrow(nextJournals)
        API <- fetch_json(count, nextJournals, key)

        if (length(API) > 0) {
            parse6 <- explore_json(API)
            return(parse6)
        } else {
            return(NULL)
        }
    } else {
        return(NULL)
    }
}

#' @export
aggregate_sherpa <- function(parse1, parse2, parse3, parse4, parse5) {
    combined_policies <- parse1
    if (!is.null(parse2)) {
        combinedPolicies <- add_case(
            combined_policies, parse2,
            .name_repair = "minimal"
        )
    }
    if (!is.null(parse3)) {
        combinedPolicies <- add_case(
            combined_policies, parse3,
            .name_repair = "minimal"
        )
    }
    if (!is.null(parse4)) {
        combinedPolicies <- add_case(
            combined_policies, parse4,
            .name_repair = "minimal"
        )
    }
    if (!is.null(parse5)) {
        combinedPolicies <- add_case(
            combined_policies, parse5,
            .name_repair = "minimal"
        )
    }
    combined_policies$ISSN <- gsub("-", "", combined_policies$ISSN)
    return(combined_policies)
}

# Import the API key for Sherpa
#' @export
get_key <- function() {
    box::use(readr[read_file])
    key <- read_file("Data/api-key.txt")
    return(key)
}

#' Fetch a nested JSON from Sherpa
#' MUST OPEN AND CLOSE CORE CLUSTERS PRIOR TO AND FOLLOWING THIS FUNCTION
#' @param opts.
#' @param count.
#' @param nextJournals.
#' @param key.
#' @param is_issn
#' @examples
#' out <- fetch_json(opts, count, nextJournals, key)
fetch_json <- function(count, next_journals, key, is_issn = FALSE, is_publisher = FALSE) {

    urls <- enum(
        base_url = 'https://v2.sherpa.ac.uk/cgi/retrieve?item-type=%s',
        issn = sprintf(
            .$base_url,
            'publication&api-key=%s&format=Json&filter=[["issn","equals","%s"]]'
        ),
        publisher = sprintf(
            .$base_url,
            'publisher&api-key=%s&format=Json&filter=[["name","equals","%s"]]'
        ),
        journal = sprintf(
            .$base_url,
            'publication&api-key=%s&format=Json&filter=[["title","equals","%s"]]'
        )
    )

    if (is_issn == TRUE) {
        urlArg <- urls$issn
    } else if (is_publisher == TRUE) {
        urlArg <- urls$publisher
    } else {
        urlArg <- urls$journal
    }

    opts <- list(progress = (function(n) setTxtProgressBar(pb, n)))
    cores <- detectCores()
    # * Initiate API fetching
    pb <- new_bar(count)
    cl <- makeCluster(cores, type = "SOCK")
    registerDoSNOW(cl)

    temp <- foreach(i = 1:count, .options.snow = opts, .errorhandling = "remove") %dopar% {
        box::use(
            utils[URLencode],
            jsonlite[fromJSON]
        )
        journal_arg <- next_journals[i]
        json_url <-
            sprintf(
                urlArg,
                key,
                toString(journal_arg)
            )
        json_url <- URLencode(json_url)
        loaded_JSON <- fromJSON(json_url)

        if (length(loaded_JSON[["items"]]) > 0) {
            loaded_JSON
        } else {
            next
        }
    }
    close(pb)
    stopCluster(cl)
    registerDoSEQ()

    temp
}

explore_json <- function(API) {
    parse1 <- API %>%
        flatten() %>%
        list.filter(!is.null(title)) %>%
        list.select(
            Titles = flatten(title),
            ISSNS = flatten(issns),
            PublisherPolicy = flatten(publisher_policy)
        ) %>%
        list.filter(!(is.null(PublisherPolicy$permitted_oa))) %>%
        list.select(
            Title = Titles$title,
            ISSN = ISSNS$issn,
            PermittedOA = flatten(PublisherPolicy$permitted_oa)
        ) %>%
        list.select(
            Title,
            ISSN,
            Submitted = "submitted" %in% PermittedOA$article_version,
            Accepted  = "accepted" %in% PermittedOA$article_version,
            Published = "published" %in% PermittedOA$article_version
        ) %>%
        list.rbind() %>%
        as_tibble(.name_repair = "minimal")

    parse2 <- API %>%
        flatten() %>%
        list.filter(!is.null(title)) %>%
        list.select(
            Titles = flatten(title),
            ISSNS = flatten(issns),
            PublisherPolicy = flatten(publisher_policy)
        ) %>%
        list.filter((is.null(PublisherPolicy$permitted_oa))) %>%
        list.select(
            Title = Titles$title,
            ISSN = ISSNS$issn,
            Submitted = FALSE,
            Accepted = FALSE,
            Published = FALSE
        ) %>%
        list.rbind() %>%
        as_tibble(.name_repair = "minimal")

    if (nrow(parse1) > 0 && nrow(parse2) > 0) {
        to_return <- full_join(parse1, parse2) %>%
            unnest_longer(Title) %>%
            unnest_longer(ISSN) %>%
            unnest_longer(Submitted) %>%
            unnest_longer(Accepted) %>%
            unnest_longer(Published)
    } else if (!nrow(parse1) > 0) {
        to_return <- parse2 %>%
            unnest_longer(Title) %>%
            unnest_longer(ISSN) %>%
            unnest_longer(Submitted) %>%
            unnest_longer(Accepted) %>%
            unnest_longer(Published)
    } else {
        to_return <- parse1 %>%
            unnest_longer(Title) %>%
            unnest_longer(ISSN) %>%
            unnest_longer(Submitted) %>%
            unnest_longer(Accepted) %>%
            unnest_longer(Published)
    }
    return(to_return)
}

get_leftover_journals <- function(combined_cite, parse1 = NULL, parse2 = NULL, parse3 = NULL, parse4 = NULL, parse5 = NULL, parse6 = NULL) {
    box::use(dplyr[...])
    leftover <- combined_cite %>%
        {
            if (!is.null(parse1)) anti_join(., parse1, by = "Title") else .
        } %>%
        {
            if (!is.null(parse2)) anti_join(., parse2, by = c("MatchTitle" = "Title")) else .
        } %>%
        {
            if (!is.null(parse3)) anti_join(., parse3, by = "Title") else .
        } %>%
        {
            if (!is.null(parse4)) anti_join(., parse4, by = "ISSN") else .
        } %>%
        {
            if (!is.null(parse5)) anti_join(., parse5, by = "Title") else .
        } %>%
        {
            if (!is.null(parse6)) anti_join(., parse6, by = "Title") else .
        }

    cat("\nJournals left : ", as.character((nrow(leftover))))
    return(leftover)
}