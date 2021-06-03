box::use(
    targets[tar_make]
)

if (!requireNamespace("enumr")) {
    install.packages("enumr", repos = "https://elianhugh.r-universe.dev")
}

tar_make(
    names = NULL,
    callr_function = NULL
)