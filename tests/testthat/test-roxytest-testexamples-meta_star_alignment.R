# Generated by roxytest: do not edit by hand!

# File R/meta_star_alignment.R: @testexamples

test_that("Function meta_star_alignment() @ L15", {
  
  pmeta <- "extdata/portaldb_workflow_top4.rds" |>
    system.file(package = "rportal") |>
    readr::read_rds()
  (m <- meta_star_alignment(pmeta))
  expect_equal(all(c("s3_outdir_star", "LibraryID") %in% colnames(m)), TRUE)
})

