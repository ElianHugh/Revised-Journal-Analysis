#' @export
describe_similarity <- function(boot_out) {
    box::use(
        tibble[tibble, add_case],
        stats[sd],
        dplyr[`%>%`],
        . / burgled[bca],
    )

    boot_SE <- sd(boot_out$Mean)
    boot_SD <- mean(boot_out$SD)
    boot_M <- mean(boot_out$Mean)
    boot_M_CI <- bca(boot_out$Mean, conf.level = 0.99)
    boot_SD_CI <- bca(boot_out$SD, conf.level = 0.99)

    boot_table <- tibble(
        Value = round(boot_SD, digits = 2),
        Low = round(boot_SD_CI[1], digits = 2),
        High = round(boot_SD_CI[2], digits = 2),
        Statistic = "SD",
        Sample = "Bootstrap"
    ) %>%
        add_case(
            Value = round(boot_M, digits = 2),
            Low = round(boot_M_CI[1], digits = 2),
            High = round(boot_M_CI[2], digits = 2),
            Statistic = "Mean",
            Sample = "Bootstrap"
        ) %>%
        add_case(
            Value = boot_SE,
            Low = NA,
            High = NA,
            Statistic = "SE",
            Sample = "Bootstrap"
        )

    return(boot_table)
}