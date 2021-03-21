# Import a stored top factor
# import relevant columns only
#' @export
get_topFactor <- function() {
    box::use(
        readr[read_csv, cols, col_skip],
        magrittr[`%<>%`],
        dplyr[rename, mutate, `%>%`, all_vars]
    )

    dat <- read_csv("Data/top-factor.csv",
        col_types = cols(
            `Analysis code transparency justification`             = col_skip(),
            `Analysis plan preregistration justification`          = col_skip(),
            `Author guideline url`                                 = col_skip(),
            `Design & analysis reporting guidelines justification` = col_skip(),
            `Registered reports & publication bias justification`  = col_skip(),
            `Data citation justification`                          = col_skip(),
            `Data transparency justification`                      = col_skip(),
            `Materials transparency justification`                 = col_skip(),
            `Open science badges justification`                    = col_skip(),
            `Replication justification`                            = col_skip(),
            Society                                                = col_skip(),
            `Study preregistration justification`                  = col_skip(),
            Total                                                  = col_skip()
        ),
        trim_ws = TRUE
    )

    dat %<>%
        rename(
            Title = Journal,
            DataTransparency = `Data transparency score`,
            AnalysisTransparency = `Analysis code transparency score`,
            MaterialsTransparency = `Materials transparency score`,
            DesignAnalysis = `Design & analysis reporting guidelines score`,
            Preregistration = `Study preregistration score`,
            Replication = `Replication score`,
            AnalysisPreReg = `Analysis plan preregistration score`,
            RegRepPubBias = `Registered reports & publication bias score`,
            DataTransparency = `Data transparency score`,
            DataCitation = `Data citation score`,
            Badges = `Open science badges score`
        ) %>%
        mutate(
            ISSN = {
                gsub("-", "", .$Issn) %>%
                gsub("[[:space:]]", "", .)
            },
            Issn = NULL
        )

    return(dat)
}