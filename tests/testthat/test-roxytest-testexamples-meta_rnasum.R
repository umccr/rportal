# Generated by roxytest: do not edit by hand!

# File R/meta_rnasum.R: @testexamples

test_that("Function meta_rnasum() @ L16", {
  
  pmeta <- "extdata/portaldb_workflow_top4.rds" |>
    system.file(package = "rportal") |>
    readr::read_rds()
  (m <- meta_rnasum(pmeta))
  expect_equal(m$rnasum_dataset[1], "PANCAN")
  expect_equal(basename(m$gds_outfile_rnasum_html[4]), "PRJ222637.RNAseq_report.html")
})

