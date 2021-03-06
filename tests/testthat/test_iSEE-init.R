# This script tests the code related to initialization utilities.
# library(iSEE); library(testthat); source("setup_sce.R"); source("test_iSEE-init.R")

context("iSEE-extras")

memory <- list(
    ReducedDimensionPlot(),
    ColumnDataPlot(ColumnSelectionSource="ReducedDimensionPlot1", ColorBy="Feature name", ColorByFeatureSource="RowDataTable1"),
    ColumnDataPlot(ColumnSelectionSource="ReducedDimensionPlot1"),
    FeatureAssayPlot(ColumnSelectionSource="ColumnDataPlot1"),
    FeatureAssayPlot(ColumnSelectionSource="FeatureAssayPlot1", XAxis="Feature name",
        YAxisFeatureSource="RowDataTable1", XAxisFeatureSource="RowDataTable2"),
    RowDataTable(),
    RowDataTable(),
    SampleAssayPlot(XAxis="Sample name", XAxisSampleSource="ColumnDataTable1"),
    ColumnDataTable()
)

test_that(".prepare_SE works correctly", {
    sce <- iSEE:::.prepare_SE(sce, ExperimentColorMap(), memory)

    # Stores the colormap:
    expect_identical(ExperimentColorMap(), iSEE:::.get_colormap(sce))

    # Caches common information:
    expect_true(all(c("DotPlot", "ColumnDotPlot", "ReducedDimensionPlot", "RowDataTable", "RowDotPlot", "ColumnDataTable")
        %in% names(metadata(sce)$iSEE$cached)))
})

test_that(".setup_initial_state works correctly", {
    sce <- iSEE:::.prepare_SE(sce, ExperimentColorMap(), memory)
    init_out <- iSEE:::.setup_initial_state(sce, memory)

    memory2 <- init_out$memory

    # Filled in ID's make sense.
    expect_false(any(duplicated(names(memory2))))
    expect_identical(names(memory2), unname(vapply(memory2, .getEncodedName, "")))

    # Actually runs the refinement.
    expect_identical(memory2[["FeatureAssayPlot1"]][["YAxisFeatureName"]], rownames(sce)[1])
    expect_identical(memory2[["ReducedDimensionPlot1"]][["Type"]], "PCA")
    expect_identical(memory2[["ColumnDataPlot1"]][["YAxis"]], colnames(colData(sce))[1])
    expect_identical(memory2[["SampleAssayPlot1"]][["YAxisSampleName"]], colnames(sce)[1])

    # Counter makes sense.
    tab <- table(vapply(memory2, iSEE:::.encodedName, ""))
    expect_identical(unname(init_out$counter), as.integer(tab))
    expect_identical(names(init_out$counter), names(tab))

    # Counter and IDs respect user-specified IDs.
    memory[[5]][["PanelId"]] <- 100L
    init_out <- iSEE:::.setup_initial_state(sce, memory)
    expect_identical(init_out$counter[["FeatureAssayPlot"]], 101L)
    expect_identical("FeatureAssayPlot100", names(init_out$memory)[5])
    expect_identical("FeatureAssayPlot101", names(init_out$memory)[4])

    # Refinement tosses out panels that can't exist.
    sce2 <- sce
    reducedDims(sce2) <- NULL
    metadata(sce2) <- list()
    sce2 <- iSEE:::.prepare_SE(sce2, ExperimentColorMap(), memory)
    init_out <- iSEE:::.setup_initial_state(sce2, memory)
    expect_false("ReducedDimensionPlot" %in% names(init_out$counter))
})

test_that(".define_reservoir works correctly", {
    sce <- iSEE:::.prepare_SE(sce, ExperimentColorMap(), memory)
    init_out <- iSEE:::.setup_initial_state(sce, memory)

    # When we're adding an empty reservoir.
    res_out <- iSEE:::.define_reservoir(sce, list(), init_out$memory, init_out$counter)
    expect_identical(init_out$counter, res_out$counter)

    enc <- vapply(res_out$memory, iSEE:::.encodedName, "")
    expect_identical(init_out$reservoir, res_out$memory[!duplicated(enc)])

    # Adding a new panel class (assuming RowDataPlot wasn't in 'memory').
    res_out2 <- iSEE:::.define_reservoir(sce, list(RowDataPlot()), init_out$memory, init_out$counter)
    expect_identical(res_out2$counter, c(res_out$counter, RowDataPlot=0L))
    expect_identical(res_out2$reservoir, c(list(RowDataPlot=.refineParameters(RowDataPlot(), sce)), res_out$reservoir))

    # Multiple copies don't change the outcome.
    res_out3 <- iSEE:::.define_reservoir(sce,
        list(RowDataPlot(), RowDataPlot(PanelHeight=1000L)),
        init_out$memory, init_out$counter)
    expect_identical(res_out3, res_out2)

    # Parameters in 'extra' override those in 'memory'.
    res_out4 <- iSEE:::.define_reservoir(sce,
        list(RowDataPlot(PanelHeight=1000L), ColumnDataPlot(PanelWidth=12L), ReducedDimensionPlot(Type="TSNE")),
        init_out$memory, init_out$counter)

    expect_identical(sum(names(res_out4$reservoir)=="RowDataPlot"), 1L)
    expect_identical(res_out4$reservoir$RowDataPlot[["PanelHeight"]], 1000L)
    expect_identical(sum(names(res_out4$reservoir)=="ColumnDataPlot"), 1L)
    expect_identical(res_out4$reservoir$ColumnDataPlot[["PanelWidth"]], 12L)
    expect_identical(sum(names(res_out4$reservoir)=="ReducedDimensionPlot"), 1L)
    expect_identical(res_out4$reservoir$ReducedDimensionPlot[["Type"]], "TSNE")
})

test_that("persistent object setup works as expected", {
    sce <- iSEE:::.prepare_SE(sce, ExperimentColorMap(), memory)
    init_out <- iSEE:::.setup_initial_state(sce, memory)
    res_out <- iSEE:::.define_reservoir(sce, list(), init_out$memory, init_out$counter)
    pObjects <- iSEE:::.create_persistent_objects(init_out$memory, res_out$reservoir, res_out$counter)

    expect_identical(pObjects$memory, init_out$memory)
    expect_type(pObjects$commands, "list")
    expect_true(is(pObjects$selection_links, "igraph"))
})

test_that(".setup_initial_state throws an error for duplicated panel identifiers", {
    sce <- iSEE:::.prepare_SE(sce, ExperimentColorMap(), memory)
    memory <- list(ReducedDimensionPlot(PanelId=1L), ReducedDimensionPlot(PanelId=1L))
    expect_error(iSEE:::.setup_initial_state(sce, memory),
        "panels of same class with duplicated IDs 'ReducedDimensionPlot1'", fixed=TRUE)
})
