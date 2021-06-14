box::use(
    tarchetypes[tar_plan, tar_render],
    targets[tar_exist_objects, tar_load, tar_read, tar_invalidate],
    here[here],
    topapi[last_updated],
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

# Invalidation
if (tar_exist_objects("top_timestamp") &&
    tar_read(top_timestamp) != last_updated()) {
    tar_invalidate(
        c("top_timestamp", "combined_cite_dat")
    )
}

tar_plan(
    # Credentials
    key = get_key(),

    # Initial data import
    cite_score_dat = get_citeScore(),
    top_timestamp = last_updated(),

    # Analysis
    combined_cite_dat = analyse_citeScore(cite_score_dat),

    parse1 = fetch_sherpa_parse1(combined_cite_dat, key),
    parse2 = fetch_sherpa_parse2(combined_cite_dat, key, parse1),
    parse3 = fetch_sherpa_parse3(combined_cite_dat, key, parse1, parse2),
    parse4 = fetch_sherpa_parse4(combined_cite_dat, key, parse1, parse2, parse3),
    parse5 = fetch_sherpa_parse5(
        combined_cite_dat,
        key,
        parse1,
        parse2,
        parse3,
        parse4
        ),
    fetched_policies = aggregate_sherpa(parse1, parse2, parse3, parse4, parse5),

    combined_policies = combine_journals(combined_cite_dat, fetched_policies),
    aggregated_policies = aggregate_policies(combined_policies),
    sample_sim = analyse_similarity(aggregated_policies, cite_score_dat),
    stats_boot = describe_similarity(sample_sim),

    # # Graphs
    cite_ridge = graph_citeridge(aggregated_policies, cite_score_dat),
    boot_graph_mean = graph_similarity_mean(
        aggregated_policies, cite_score_dat, sample_sim, stats_boot
    ),

    # # Report
    tar_render(report, "report.Rmd")
)
