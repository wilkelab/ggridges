testthat::context('anovarm')

testthat::test_that('All options in the anovaRM work (sunny)', {
    # simulate data set
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(210)
    data <- list(
        between = factor(sample(c("A", "B", "C"), 60, replace = TRUE)),
        'm o n' = rnorm(60, .5),
        tue = rnorm(60, .6),
        fri = rnorm(60, .7)
    )
    attr(data, 'row.names') <- seq_len(length(data[[1]]))
    attr(data, 'class') <- 'data.frame'

    rm <- list(
        list(
            label="intake",
            levels=c("m o n", "tue", "fri")
        )
    )

    rmCells <- list(
        list(measure="m o n", cell="m o n"),
        list(measure="tue", cell="tue"),
        list(measure="fri", cell="fri")
    )

    postHoc <- list(
        "intake",
        "between",
        c("intake", "between")
    )

    r <- jmv::anovaRM(
        data = data,
        rm = rm,
        rmCells = rmCells,
        bs = "between",
        rmTerms = list("intake"),
        bsTerms = list("between"),
        effectSize = c("ges", "eta", "partEta"),
        spherTests = TRUE,
        spherCorr = c("none", "GG", "HF"),
        leveneTest = TRUE,
        postHoc = postHoc,
        postHocCorr = c("tukey", "none", "scheffe", "bonf", "holm"),
        emMeans = ~intake + between + intake:between,
        emmPlots = FALSE,
        emmTables = TRUE,
        groupSumm = TRUE
    )

    # Test repeated measures table
    rmTable <- r$rmTable$asDF
    testthat::expect_equal(c(0.561, 2.265, 107.879), rmTable[['ss[none]']], tolerance = 1e-3)
    testthat::expect_equal(c(2, 4, 114), rmTable[['df[none]']])
    testthat::expect_equal(c(0.281, 0.566, 0.946), rmTable[['ms[none]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.296, 0.598, NA), rmTable[['F[none]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.744, 0.665, NA), rmTable[['p[none]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.003, 0.013, NA), rmTable[['ges[none]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.003, 0.013, NA), rmTable[['eta[none]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.005, 0.021, NA), rmTable[['partEta[none]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.561, 2.265, 107.879), rmTable[['ss[GG]']], tolerance = 1e-3)
    testthat::expect_equal(c(1.97, 3.94, 112.299), rmTable[['df[GG]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.285, 0.575, 0.961), rmTable[['ms[GG]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.296, 0.598, NA), rmTable[['F[GG]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.741, 0.662, NA), rmTable[['p[GG]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.003, 0.013, NA), rmTable[['ges[GG]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.003, 0.013, NA), rmTable[['eta[GG]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.005, 0.021, NA), rmTable[['partEta[GG]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.561, 2.265, 107.879), rmTable[['ss[HF]']], tolerance = 1e-3)
    testthat::expect_equal(c(2, 4, 114), rmTable[['df[HF]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.281, 0.566, 0.946), rmTable[['ms[HF]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.296, 0.598, NA), rmTable[['F[HF]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.744, 0.665, NA), rmTable[['p[HF]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.003, 0.013, NA), rmTable[['ges[HF]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.003, 0.013, NA), rmTable[['eta[HF]']], tolerance = 1e-3)
    testthat::expect_equal(c(0.005, 0.021, NA), rmTable[['partEta[HF]']], tolerance = 1e-3)

    # Test between subjects table
    bsTable <- r$bsTable$asDF
    testthat::expect_equal(c(0.416, 68.827), bsTable[['ss']], tolerance = 1e-3)
    testthat::expect_equal(c(2, 57), bsTable[['df']], tolerance = 1e-3)
    testthat::expect_equal(c(0.208, 1.207), bsTable[['ms']], tolerance = 1e-3)
    testthat::expect_equal(c(0.172, NA), bsTable[['F']], tolerance = 1e-3)
    testthat::expect_equal(c(0.842, NA), bsTable[['p']], tolerance = 1e-3)
    testthat::expect_equal(c(0.002, NA), bsTable[['ges']], tolerance = 1e-3)
    testthat::expect_equal(c(0.002, NA), bsTable[['eta']], tolerance = 1e-3)
    testthat::expect_equal(c(0.006, NA), bsTable[['partEta']], tolerance = 1e-3)


    # Test sphericity table
    spherTable <- r$assump$spherTable$asDF
    testthat::expect_equal(0.985, spherTable[['mauch']], tolerance = 1e-3)
    testthat::expect_equal(0.652, spherTable[['p']], tolerance = 1e-3)
    testthat::expect_equal(0.985, spherTable[['gg']], tolerance = 1e-3)
    testthat::expect_equal(1, spherTable[['hf']], tolerance = 1e-3)

    # Test levene's table
    levenesTable <- r$assump$leveneTable$asDF
    testthat::expect_equal(c('m o n', 'tue', 'fri'), levenesTable[['name']])
    testthat::expect_equal(c(1.263, 0.333, 1.494), levenesTable[['F']], tolerance = 1e-3)
    testthat::expect_equal(c(2, 2, 2), levenesTable[['df1']])
    testthat::expect_equal(c(57, 57, 57), levenesTable[['df2']])
    testthat::expect_equal(c(0.291, 0.718, 0.233), levenesTable[['p']], tolerance = 1e-3)

    # Test post-hoc tables
    postHocTable1 <- r$postHoc[[1]]$asDF
    testthat::expect_equal(c('m o n', 'm o n', 'tue'), postHocTable1[['intake1']])
    testthat::expect_equal(c('tue', 'fri', 'fri'), postHocTable1[['intake2']])
    testthat::expect_equal(c(-0.139, -0.065, 0.074), postHocTable1[['md']], tolerance = 1e-3)
    testthat::expect_equal(c(0.191, 0.178, 0.173), postHocTable1[['se']], tolerance = 1e-3)
    testthat::expect_equal(c(57, 57, 57), postHocTable1[['df']])
    testthat::expect_equal(c(-0.727, -0.366, 0.428), postHocTable1[['t']], tolerance = 1e-3)
    testthat::expect_equal(c(0.47, 0.716, 0.67), postHocTable1[['pnone']], tolerance = 1e-3)
    testthat::expect_equal(c(0.748, 0.929, 0.904), postHocTable1[['ptukey']], tolerance = 1e-3)
    testthat::expect_equal(c(0.768, 0.935, 0.912), postHocTable1[['pscheffe']], tolerance = 1e-3)
    testthat::expect_equal(c(1, 1, 1), postHocTable1[['pbonferroni']], tolerance = 1e-3)
    testthat::expect_equal(c(1, 1, 1), postHocTable1[['pholm']], tolerance = 1e-3)

    postHocTable2 <- r$postHoc[[2]]$asDF
    testthat::expect_equal(c('A', 'A', 'B'), postHocTable2[['between1']])
    testthat::expect_equal(c('B', 'C', 'C'), postHocTable2[['between2']])
    testthat::expect_equal(c(-0.028, -0.117, -0.09), postHocTable2[['md']], tolerance = 1e-3)
    testthat::expect_equal(c(0.193, 0.203, 0.215), postHocTable2[['se']], tolerance = 1e-3)
    testthat::expect_equal(c(57, 57, 57), postHocTable2[['df']])
    testthat::expect_equal(c(-0.144, -0.578, -0.416), postHocTable2[['t']], tolerance = 1e-3)
    testthat::expect_equal(c(0.886, 0.566, 0.679), postHocTable2[['pnone']], tolerance = 1e-3)
    testthat::expect_equal(c(0.989, 0.832, 0.909), postHocTable2[['ptukey']], tolerance = 1e-3)
    testthat::expect_equal(c(0.99, 0.847, 0.917), postHocTable2[['pscheffe']], tolerance = 1e-3)
    testthat::expect_equal(c(1, 1, 1), postHocTable2[['pbonferroni']], tolerance = 1e-3)
    testthat::expect_equal(c(1, 1, 1), postHocTable2[['pholm']], tolerance = 1e-3)

    postHocTable3 <- r$postHoc[[3]]$asDF
    testthat::expect_equal(
        c('m o n', 'm o n', 'm o n', 'm o n', 'm o n', 'm o n', 'm o n', 'm o n', 'm o n', 'm o n',
          'm o n', 'm o n', 'm o n', 'm o n', 'm o n', 'm o n', 'm o n', 'm o n', 'm o n', 'm o n',
          'm o n', 'tue', 'tue', 'tue', 'tue', 'tue', 'tue', 'tue', 'tue', 'tue', 'tue', 'tue',
          'tue', 'fri', 'fri', 'fri'),
        postHocTable3[['intake1']],
    )
    testthat::expect_equal(
        c('A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'C', 'C', 'C',
          'C', 'C', 'C', 'A', 'A', 'A', 'A', 'A', 'B', 'B', 'B', 'B', 'C', 'C', 'C', 'A', 'A', 'B'),
        postHocTable3[['between1']],
    )
    testthat::expect_equal(
        c('m o n', 'm o n', 'tue', 'tue', 'tue', 'fri', 'fri', 'fri', 'm o n', 'tue', 'tue', 'tue',
          'fri', 'fri', 'fri', 'tue', 'tue', 'tue', 'fri', 'fri', 'fri', 'tue', 'tue', 'fri', 'fri',
          'fri', 'tue', 'fri', 'fri', 'fri', 'fri', 'fri', 'fri', 'fri', 'fri', 'fri'),
        postHocTable3[['intake2']]
    )
    testthat::expect_equal(
        c('B', 'C', 'A', 'B', 'C', 'A', 'B', 'C', 'C', 'A', 'B', 'C', 'A', 'B', 'C', 'A', 'B', 'C',
          'A', 'B', 'C', 'B', 'C', 'A', 'B', 'C', 'C', 'A', 'B', 'C', 'A', 'B', 'C', 'B', 'C', 'C'),
        postHocTable3[['between2']]
    )
    testthat::expect_equal(
        c(0.067, -0.286, -0.202, -0.376, -0.058, -0.076, -0.052, -0.286, -0.353, -0.268, -0.443,
          -0.125, -0.143, -0.119, -0.353, 0.084, -0.09, 0.228, 0.21, 0.234, 0, -0.175, 0.144, 0.125,
          0.15, -0.084, 0.318, 0.3, 0.325, 0.09, -0.018, 0.006, -0.228, 0.025, -0.21, -0.234),
        postHocTable3[['md']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.307, 0.323, 0.291, 0.304, 0.319, 0.271, 0.314, 0.331, 0.342, 0.304, 0.334, 0.339, 0.312,
          0.311, 0.35, 0.32, 0.339, 0.364, 0.328, 0.349, 0.338, 0.301, 0.317, 0.263, 0.312, 0.329,
          0.336, 0.31, 0.302, 0.347, 0.325, 0.346, 0.329, 0.32, 0.336, 0.357),
        postHocTable3[['se']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57,
          57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57),
        postHocTable3[['df']]
    )
    testthat::expect_equal(
        c(0.217, -0.887, -0.693, -1.24, -0.182, -0.282, -0.165, -0.865, -1.032, -0.882, -1.327,
          -0.368, -0.458, -0.382, -1.009, 0.263, -0.266, 0.627, 0.639, 0.672, -0.001, -0.579, 0.453,
          0.477, 0.481, -0.257, 0.947, 0.969, 1.077, 0.26, -0.057, 0.018, -0.694, 0.077, -0.624,
          -0.658),
        postHocTable3[['t']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.829, 0.379, 0.491, 0.22, 0.856, 0.779, 0.87, 0.391, 0.306, 0.381, 0.19, 0.714, 0.649,
          0.704, 0.317, 0.794, 0.791, 0.533, 0.525, 0.504, 1, 0.565, 0.652, 0.635, 0.633, 0.798,
          0.347, 0.336, 0.286, 0.796, 0.955, 0.986, 0.49, 0.939, 0.535, 0.513),
        postHocTable3[['pnone']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(1, 0.993, 0.999, 0.944, 1, 1, 1, 0.994, 0.981, 0.993, 0.919, 1, 1, 1, 0.984, 1, 1, 0.999,
          0.999, 0.999, 1, 1, 1, 1, 1, 1, 0.989, 0.987, 0.975, 1, 1, 1, 0.999, 1, 0.999, 0.999),
        postHocTable3[['ptukey']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(1, 0.999, 1, 0.991, 1, 1, 1, 0.999, 0.997, 0.999, 0.986, 1, 1, 1, 0.998, 1, 1, 1, 1, 1, 1,
          1, 1, 1, 1, 1, 0.999, 0.998, 0.997, 1, 1, 1, 1, 1, 1, 1),
        postHocTable3[['pscheffe']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
          1, 1, 1, 1, 1, 1),
        postHocTable3[['pbonferroni']], tolerance = 1e-3
    )
    testthat::expect_equal(
        c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
          1, 1, 1, 1, 1, 1),
        postHocTable3[['pholm']], tolerance = 1e-3
    )

    # Test estimated marginal means tables
    emMeansTable1 <- r$emm[[1]]$emmTable$asDF
    testthat::expect_equal(c('m o n', 'tue', 'fri'), emMeansTable1[['intake']])
    testthat::expect_equal(c(0.481, 0.62, 0.546), emMeansTable1[['mean']], tolerance = 1e-3)
    testthat::expect_equal(c(0.132, 0.13, 0.138), emMeansTable1[['se']], tolerance = 1e-3)
    testthat::expect_equal(c(0.216, 0.359, 0.27), emMeansTable1[['lower']], tolerance = 1e-3)
    testthat::expect_equal(c(0.746, 0.88, 0.822), emMeansTable1[['upper']], tolerance = 1e-3)

    emMeansTable2 <- r$emm[[2]]$emmTable$asDF
    testthat::expect_equal(c('A', 'B', 'C'), emMeansTable2[['between']])
    testthat::expect_equal(c(0.5, 0.528, 0.618), emMeansTable2[['mean']], tolerance = 1e-3)
    testthat::expect_equal(c(0.127, 0.146, 0.159), emMeansTable2[['se']], tolerance = 1e-3)
    testthat::expect_equal(c(0.246, 0.237, 0.3), emMeansTable2[['lower']], tolerance = 1e-3)
    testthat::expect_equal(c(0.754, 0.82, 0.935), emMeansTable2[['upper']], tolerance = 1e-3)

    emMeansTable3 <- r$emm[[3]]$emmTable$asDF
    testthat::expect_equal(
        c('A', 'A', 'A', 'B', 'B', 'B', 'C', 'C', 'C'),
        emMeansTable3[['between']]
    )
    testthat::expect_equal(
        c('m o n', 'tue', 'fri', 'm o n', 'tue', 'fri', 'm o n', 'tue', 'fri'),
        emMeansTable3[['intake']]
    )
    testthat::expect_equal(
        c(0.408, 0.609, 0.484, 0.341, 0.784, 0.459, 0.694, 0.466, 0.694),
        emMeansTable3[['mean']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.201, 0.198, 0.21, 0.231, 0.227, 0.241, 0.252, 0.248, 0.263),
        emMeansTable3[['se']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.004, 0.213, 0.063, -0.122, 0.329, -0.023, 0.189, -0.03, 0.168),
        emMeansTable3[['lower']],
        tolerance = 1e-3
    )
    testthat::expect_equal(
        c(0.811, 1.006, 0.905, 0.804, 1.239, 0.942, 1.198, 0.962, 1.22),
        emMeansTable3[['upper']],
        tolerance = 1e-3
    )

    # Test group summary table
    groupSummaryTable <- r$groupSummary$asDF
    testthat::expect_equal(c('A', 'B', 'C'), groupSummaryTable[['group:between']])
    testthat::expect_equal(c(25, 19, 16), groupSummaryTable[['n']])
    testthat::expect_equal(c(0, 0, 0), groupSummaryTable[['ex']])
})

testthat::test_that("Test sphericity footnote when there's a singularity error", {
    data <- data.frame(
        'id' = 1:15,
        'x1' = c(4, 13, 15, 12, 12, 2, 19, 10, 22, 13, 10, 22, 10, 14, 22),
        'x2' = c(55, 40, 26, 6, 20, 37, 37, 12, 45, 29, 28, 4, 26, 39, 30),
        'x3' = c(51, 36, 22, 2, 16, 33, 33, 8, 41, 25, 24, 0, 22, 35, 26)
    )

    r <- jmv::anovaRM(
        data = data,
        rm = list(
            list(
                label="var",
                levels=c("x1", "x2", "x3"))),
        rmCells = list(
            list(
                measure="x1",
                cell="x1"),
            list(
                measure="x2",
                cell="x2"),
            list(
                measure="x3",
                cell="x3")),
        rmTerms = ~ var,
        spherTests = TRUE)

    spher <- r$assump$spherTable$asDF
    testthat::expect_equal(spher$mauch, NaN)
})

testthat::test_that('emmeans work for unbalanced data', {
    set.seed(1337)
    N <- 100
    data <- data.frame(
        measure1 = rnorm(N, 0, 1),
        measure2 = rnorm(N, 1, 1),
        measure3 = rnorm(N, 2, 1),
        bsFactor = sample(letters[1:2], replace=TRUE, prob=c(0.3, 0.7), size=N),
        stringsAsFactors = TRUE
    )

    rm = list(list(
        label="rmFactor",
        levels=c("measure1", "measure2", "measure3")
    ))

    rmCells = list(
        list(
            measure="measure1",
            cell="measure1"),
        list(
            measure="measure2",
            cell="measure2"),
        list(
            measure="measure3",
            cell="measure3")
    )

    r <- jmv::anovaRM(
        data=data, rm=rm, rmCells=rmCells, bs="bsFactor",
        rmTerms=list("rmFactor"), bsTerms=list("bsFactor"),
        emMeans = ~bsFactor:rmFactor, emmPlots = FALSE, emmTables = TRUE
    )

    means <- aggregate(data[, -4], data[4], mean)
    emmeans <- r$emm[[1]]$emmTable$asDF

    testthat::expect_equal(means[1, 2], emmeans[1, "mean"], tolerance = 1e-4)
    testthat::expect_equal(means[2, 2], emmeans[2, "mean"], tolerance = 1e-4)
    testthat::expect_equal(means[1, 3], emmeans[3, "mean"], tolerance = 1e-4)
    testthat::expect_equal(means[2, 3], emmeans[4, "mean"], tolerance = 1e-4)
    testthat::expect_equal(means[1, 4], emmeans[5, "mean"], tolerance = 1e-4)
    testthat::expect_equal(means[2, 4], emmeans[6, "mean"], tolerance = 1e-4)
})

testthat::test_that('Provide error message when there are empty cells in bs design', {
    df <- data.frame(
        measure1 = 20:24,
        measure2 = 24:20,
        bsFactor1 = c("A", "A", "B", "A", "A"),
        bsFactor2 = c("A", "A", "B", "A", "A"),
        stringsAsFactors = TRUE
    )

    rm = list(list(
        label="rmFactor",
        levels=c("measure1", "measure2")
    ))

    rmCells = list(
        list(measure="measure1", cell="measure1"),
        list(measure="measure2", cell="measure2")
    )

    testthat::expect_error(
        jmv::anovaRM(
            data=df,
            rm=rm,
            rmCells=rmCells,
            bs=c("bsFactor1", "bsFactor2"),
            rmTerms=list("rmFactor"),
            bsTerms=list("bsFactor1", "bsFactor2")
        ),
        "Empty cells in between subject design"
    )
})

testthat::test_that("No warnings are thrown when bs terms contains interaction", {
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(1337)
    df <- data.frame(
        measure1 = rnorm(100),
        measure2 = rnorm(100),
        bsFactor1 = sample(LETTERS[1:2], 100, replace = TRUE),
        bsFactor2 = sample(LETTERS[1:2], 100, replace = TRUE),
        stringsAsFactors = TRUE
    )

    rm = list(list(
        label="rmFactor",
        levels=c("measure1", "measure2")
    ))

    rmCells = list(
        list(measure="measure1", cell="measure1"),
        list(measure="measure2", cell="measure2")
    )

    testthat::expect_no_warning(
        jmv::anovaRM(
            data=df,
            rm=rm,
            rmCells=rmCells,
            bs=vars(bsFactor1, bsFactor2),
            rmTerms=~rmFactor,
            bsTerms=~bsFactor1 + bsFactor2 + bsFactor1:bsFactor2,
        )
    )
})
