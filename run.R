box::use(
    targets[tar_make]
)


# The analysis uses two of my packages which aren't
# currently on CRAN, but are available through
# r-universe:
if (!requireNamespace("enumr")) {
    install.packages("enumr", repos = "https://elianhugh.r-universe.dev")
}

if (!requireNamespace("topapi")) {
    install.packages("topapi", repos = "https://elianhugh.r-universe.dev")
}


tar_make(
    names = NULL,
    callr_function = NULL
)