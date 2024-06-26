# Generated by roxytest: do not edit by hand!

# File R/meta_umccrise.R: @testexamples

test_that("Function meta_umccrise() @ L15", {
  
  pmeta <- "extdata/portaldb_workflow_top4.rds" |>
    system.file(package = "rportal") |>
    readr::read_rds()
  (m <- meta_umccrise(pmeta))
  expect_equal(all(c("LibraryID_normal", "LibraryID_tumor") %in% colnames(m)), TRUE)
})

