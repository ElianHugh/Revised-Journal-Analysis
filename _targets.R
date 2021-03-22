box::use(
    tarchetypes[tar_plan, tar_render],
    here[here],
    . / R / get_topFactor[get_topFactor],
    . / R / get_citeScore[get_citeScore],
    . / R / analyse_citeScore[analyse_citeScore],
    . / R / fetch_sherpa[
        fetch_sherpa_parse1,
        fetch_sherpa_parse2,
        fetch_sherpa_parse3,
        fetch_sherpa_parse4,
        fetch_sherpa_parse5,
        aggregate_sherpa,
        get_key
    ],
    . / R / combine_journals[combine_journals],
    . / R / aggregate_policies[aggregate_policies],
    . / R / graph_citeridge[graph_citeridge],
    . / R / analyse_similarity[analyse_similarity],
    . / R / describe_similarity[describe_similarity],
    . / R / graph_similarity[graph_similarity_mean]
)

tar_plan(
    # Credentials
    key = get_key(),

    # Initial data import
    topFactorDat = get_topFactor(),
    citeScoreDat = get_citeScore(),

    # Analysis
    combinedCiteDat = analyse_citeScore(topFactorDat, citeScoreDat),

    parse1 = fetch_sherpa_parse1(combinedCiteDat, key),
    parse2 = fetch_sherpa_parse2(combinedCiteDat, key, parse1),
    parse3 = fetch_sherpa_parse3(combinedCiteDat, key, parse1, parse2),
    parse4 = fetch_sherpa_parse4(combinedCiteDat, key, parse1, parse2, parse3),
    parse5 = fetch_sherpa_parse5(
        combinedCiteDat,
        key,
        parse1,
        parse2,
        parse3,
        parse4
        ),
    fetchedPolicies = aggregate_sherpa(parse1, parse2, parse3, parse4, parse5),

    combinedPolicies = combine_journals(combinedCiteDat, fetchedPolicies),
    aggregatedPolicies = aggregate_policies(combinedPolicies),
    sampleSim = analyse_similarity(aggregatedPolicies, citeScoreDat),
    statsBoot = describe_similarity(sampleSim),

    # # Graphs
    citeRidge = graph_citeridge(aggregatedPolicies),
    bootGraphMean = graph_similarity_mean(aggregatedPolicies, sampleSim, statsBoot),

    # # Report
    tar_render(report, "report.Rmd")
)