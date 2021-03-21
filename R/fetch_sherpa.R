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
    magrittr[`%<>%`],
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
    . / helper[new_bar]
)

# Match journals by title
#' @export
fetch_sherpa_parse1 <- function(combinedCite, key) {
    journals <- combinedCite %>%
        ungroup() %>%
        mutate(ISSN = {
            ISSN %>%
                gsub("[[:space:]]", "", .) %>%
                gsub("^(.{4})(.*)$", "\\1-\\2", .)
        })
    nextJournals <- journals %>%
        select(Title) %>%
        distinct()

    cat("\nInitial journals : ", as.character((nrow(nextJournals)), "\n"))
    count <- length(nextJournals$Title)
    API <- fetch_json(count, nextJournals$Title, key)

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
fetch_sherpa_parse2 <- function(combinedCite, key, parse1) {
    nextJournals <- combinedCite %>%
        get_leftover_journals(parse1) %>%
        distinct(MatchTitle, .keep_all = TRUE)
    message("\n* PARSE 2 OF 5 * Finding journals by matchtitle\n")
    # * Initiate API fetching
    count <- length(nextJournals$MatchTitle)
    API   <- fetch_json(count, nextJournals$MatchTitle, key)

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
fetch_sherpa_parse3 <- function(combinedCite, key, parse1, parse2) {
    nextJournals <- combinedCite %>%
        get_leftover_journals(parse1, parse2) %>%
        distinct(ISSN, .keep_all = TRUE) %>%
        mutate(
            ISSN = {
                ISSN %>%
                    gsub("[[:space:]]", "", .) %>%
                    gsub("^(.{4})(.*)$", "\\1-\\2", .)
            }
        )
    cat("\nISSNs left : ", as.character((nrow(nextJournals))))
    message("\n* PARSE 3 OF 5 * Finding journals by ISSN\n")
    # * Initiate API fetching
    count <- length(nextJournals$ISSN)
    API <- fetch_json(count, nextJournals$ISSN, key, is_issn = TRUE)

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
fetch_sherpa_parse4 <- function(combinedCite, key, parse1, parse2, parse3) {
    nextJournals <- combinedCite %>%
        get_leftover_journals(parse1, parse2, parse3) %>%
        distinct(Title, .keep_all = TRUE) %>%
        mutate(
            Title = str_replace(Title, " and ", " & ")
        )
    message("\n* PARSE 4 OF 5 * Finding journals by modified title\n")

    # * Initiate API fetching
    count <- length(nextJournals$Title)
    API <- fetch_json(count, nextJournals$Title, key)

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
fetch_sherpa_parse5 <- function(combinedCite, key, parse1, parse2, parse3, parse4) {
    nextJournals <- combinedCite %>%
        get_leftover_journals(parse1, parse2, parse3, parse4) %>%
        distinct(MatchTitle, .keep_all = TRUE)

    nextPublishers <- nextJournals %>%
        ungroup() %>%
        distinct(Publisher, .keep_all = TRUE)

    message("\n* PARSE 5 OF 6 * Finding journals through publisher matching \n")

    # * Initiate API fetching
    count <- length(nextPublishers$Publisher)
    API   <- fetch_json(count, nextPublishers$Publisher, key, is_publisher = TRUE)
    count <- length(API)

    # * Prep for explore
    if (count > 0) {
        explore_publisher <- function(API, i) {
            temp <- flatten(API)
            if ("publications" %in% names(temp)) {
                temp %>%
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
        }
    }

    parse5 <- map2(API, seq_along(API), .f = explore_publisher)
    parse5 <- unlist(parse5, use.names = FALSE)
    cat("\n Publisher journals found: ", length(parse5))

    tempdf <- as.data.frame(parse5) %>%
        rename(Title = parse5)
    tempdf <- anti_join(tempdf, combinedCite)

    if (!is.null(parse1)) tempdf <- anti_join(tempdf, parse1, by = "Title")
    if (!is.null(parse2)) tempdf <- anti_join(tempdf, parse2, by = "Title")
    if (!is.null(parse3)) tempdf <- anti_join(tempdf, parse3, by = "Title")
    if (!is.null(parse4)) tempdf <- anti_join(tempdf, parse4, by = "ISSN")

    # ! Look for matches of title
    fuzzydf <- stringdist_left_join(nextJournals, tempdf, by = "Title", distance_col = "Distance", max_dist = 10)

    listA <- fuzzydf$Title.x[!is.na(fuzzydf$Title.x)]
    listB <- fuzzydf$Title.y[!is.na(fuzzydf$Title.y)]

    matchJournals <- sapply(listA, function(y) sapply(listB, function(x) grepl(y, x)))

    matchJournals %<>%
        as.data.frame() %>%
        rownames_to_column("id") %>%
        gather(key = "key", value = "value", -id) %>%
        filter(value == TRUE)


    # ! There is definitely a better way to do this but...
    matchJournals$id <- str_replace_all(matchJournals$id, "[^[:alnum:]]", " ")
    matchJournals$id <- trimws(matchJournals$id, which = "both")
    matchJournals$key <- str_replace_all(matchJournals$key, "[^[:alnum:]]", " ")
    matchJournals$key <- str_replace_all(matchJournals$key, "[0-9]+", " ")
    matchJournals$key <- trimws(matchJournals$key, which = "both")


    matchJournals %<>%
        distinct()

    cat("\nJournals matched: ", nrow(matchJournals))

    nextJournals <- matchJournals %>%
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

}

#' @export
aggregate_sherpa <- function(parse1, parse2, parse3, parse4, parse5) {
    combinedPolicies <- parse1
    if (!is.null(parse2)) combinedPolicies <- add_case(combinedPolicies, parse2)
    if (!is.null(parse3)) combinedPolicies <- add_case(combinedPolicies, parse3)
    if (!is.null(parse4)) combinedPolicies <- add_case(combinedPolicies, parse4)
    if (!is.null(parse5)) combinedPolicies <- add_case(combinedPolicies, parse5)
    combinedPolicies$ISSN <- gsub("-", "", combinedPolicies$ISSN)
    return(combinedPolicies)
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
fetch_json <- function(count, nextJournals, key, is_issn = FALSE, is_publisher = FALSE) {
    if (is_issn == TRUE) {
        urlArg <- 'https://v2.sherpa.ac.uk/cgi/retrieve?item-type=publication&api-key=%s&format=Json&filter=[["issn","equals","%s"]]'
    } else if (is_publisher == TRUE) {
        urlArg <- 'https://v2.sherpa.ac.uk/cgi/retrieve?item-type=publisher&api-key=%s&format=Json&filter=[["name","equals","%s"]]'
    } else {
        urlArg <- 'https://v2.sherpa.ac.uk/cgi/retrieve?item-type=publication&api-key=%s&format=Json&filter=[["title","equals","%s"]]'
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
        journalArg <- nextJournals[i]
        jsonURL <-
            sprintf(
                urlArg,
                key,
                toString(journalArg)
            )
        jsonURL <- URLencode(jsonURL)
        loadedJSON <- fromJSON(jsonURL)

        if (length(loadedJSON[["items"]]) > 0) {
            loadedJSON
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
        x <- API                                                          %>%
            flatten()                                                     %>%
            list.filter(!is.null(title))                                  %>%
            list.select(
                Titles          = flatten(title),
                ISSNS           = flatten(issns),
                PublisherPolicy = flatten(publisher_policy))              %>%
            list.filter(!(is.null(PublisherPolicy$permitted_oa)))         %>%
            list.select(
                Title       = Titles$title,
                ISSN        = ISSNS$issn,
                PermittedOA = flatten(PublisherPolicy$permitted_oa))      %>%
            list.select(
                Title,
                ISSN,
                Submitted = "submitted" %in% PermittedOA$article_version,
                Accepted  = "accepted" %in% PermittedOA$article_version,
                Published = "published" %in% PermittedOA$article_version) %>%
            list.rbind()                                                  %>%
            as_tibble()

        y <- API                                                 %>%
            flatten()                                            %>%
            list.filter(!is.null(title))                         %>%
            list.select(
                Titles          = flatten(title),
                ISSNS           = flatten(issns),
                PublisherPolicy = flatten(publisher_policy))     %>%
            list.filter((is.null(PublisherPolicy$permitted_oa))) %>%
            list.select(
                Title     = Titles$title,
                ISSN      = ISSNS$issn,
                Submitted = FALSE,
                Accepted  = FALSE,
                Published = FALSE)                               %>%
            list.rbind()                                         %>%
            as_tibble()

        if (nrow(x) > 0 && nrow(y) > 0) {
            z <- full_join(x, y)         %>%
                unnest_longer(Title)     %>%
                unnest_longer(ISSN)      %>%
                unnest_longer(Submitted) %>%
                unnest_longer(Accepted)  %>%
                unnest_longer(Published)
        } else if (!nrow(x) > 0) {
            z <- y                       %>%
                unnest_longer(Title)     %>%
                unnest_longer(ISSN)      %>%
                unnest_longer(Submitted) %>%
                unnest_longer(Accepted)  %>%
                unnest_longer(Published)
        } else {
            z <- x                       %>%
                unnest_longer(Title)     %>%
                unnest_longer(ISSN)      %>%
                unnest_longer(Submitted) %>%
                unnest_longer(Accepted)  %>%
                unnest_longer(Published)
        }
        return(z)
    }

get_leftover_journals <- function(combinedCite, parse1 = NULL, parse2 = NULL, parse3 = NULL, parse4 = NULL, parse5 = NULL, parse6 = NULL) {
    box::use(dplyr[...])
    leftover <- combinedCite %>%
        { if (!is.null(parse1)) anti_join(., parse1, by = "Title") else . } %>%
        { if (!is.null(parse2)) anti_join(., parse2, by = c("MatchTitle" = "Title")) else . } %>%
        { if (!is.null(parse3)) anti_join(., parse3, by = "Title") else . } %>%
        { if (!is.null(parse4)) anti_join(., parse4, by = "ISSN") else . } %>%
        { if (!is.null(parse5)) anti_join(., parse5, by = "Title") else . } %>%
        { if (!is.null(parse6)) anti_join(., parse6, by = "Title") else . }

    cat("\nJournals left : ", as.character((nrow(leftover))))
    return(leftover)
}
