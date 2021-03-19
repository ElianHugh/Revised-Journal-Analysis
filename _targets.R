box::use(
    tarchetypes[tar_plan, tar_render],
    here[here],
    withr[with_options]
)
with_options(
    list('box.path' = here()),
    box::use(
        . / R / get_topFactor[get_topFactor],
        . / R / get_citeScore[get_citeScore],
        . / R / analyse_citeScore[analyse_citeScore],
        . / R / fetch_sherpa[fetch_sherpa, get_key]
        # . / R / combine_journals[combine_journals],
        # . / R / aggregate_policies[aggregate_policies],
        # . / R / analyse_similarity[analyse_similarity],
        # . / R / describe_similarity[describe_similarity],
        # . / R / graph_citeridge[graph_citeridge],
        # . / R / graph_similarity[graph_similarity]
    )
)

tar_plan(
    # Credentials
    key = get_key(),

    # Initial data import
    topFactorDat = get_topFactor(),
    citeScoreDat = get_citeScore(),

    # Analysis
    combinedCiteDat = analyse_citeScore(topFactorDat, citeScoreDat),
    fetchedPolicies = fetch_sherpa(combinedCiteDat, key),
    # combinedPolicies = combine_journals(combinedCiteDat, fetchedPolicies),
    # aggregatedPolicies = aggregate_policies(combinedPolicies),
    # sampleSim = analyse_similarity(aggregatedPolicies, citeScore),
    # statsBoot = describe_similarity(sampleSim),

    # # Graphs
    # citeRidge = graph_citeridge(aggregatePolicies),
    # bootGraph = graph_similarity(aggregatePolicies, sampleSim),

    # # Report
    # tar_render(report, "report.Rmd")
)