library(hbGIS)
context("hbGIS pipeline")
test_that("hbGIS pipeline process file", {
  # Prepare input data
  GIS_files  = dir(system.file("testfiles_hbGIS", package = "hbGIS"), full.names = TRUE, pattern = "loc_")
  dn = "./GIS"
  if (!dir.exists(dn)) {
    dir.create(dn, recursive = TRUE)
  }
  for (fn in GIS_files) file.copy(from = fn, to = dn)
  
  # Prepare output folder
  outdir = "./output"
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE)
  }
  
  # Run pipeline
  hbGIS(gisdir = "./GIS",
        palmsdir = NULL, # note: function will simulat palms data if palmsdir not provided
        gislinkfile = NULL,
        outputdir = outdir,
        dataset_name = "test",
        verbose = FALSE,
        sublocationID = "ignore")
  
  # Check days
  file = paste0(outdir, "/hbGIS_output/test_days.csv")
  expect_true(file.exists(file))
  test_days = read.csv(file)
  expect_equal(length(test_days), 37)
  expect_equal(sum(test_days[, 3:ncol(test_days)]), 3005)
  
  # Check whenwhatwhere
  file = paste0(outdir, "/hbGIS_output/test_whenwhatwhere.csv")
  expect_true(file.exists(file))
  test_whenwhatwhere = read.csv(file)
  expect_equal(nrow(test_whenwhatwhere), 500)
  expect_equal(ncol(test_whenwhatwhere), 30)
  expect_equal(sum(test_whenwhatwhere[, 3:(ncol(test_whenwhatwhere) - 1)]), 6075)
  
  # Check trajectories
  file = paste0(outdir, "/hbGIS_output/test_trajectories.csv")
  expect_true(file.exists(file))
  test_trajectories = read.csv(file)
  expect_equal(nrow(test_trajectories), 2)
  expect_equal(ncol(test_trajectories), 18)
  expect_equal(sum(test_trajectories$length), 100582.7, tol = 0.1)
  expect_equal(sum(test_trajectories$speed), 1341.102, tol = 0.001)
  
  
  # Check multimodal
  file = paste0(outdir, "/hbGIS_output/test_multimodal.csv")
  expect_true(file.exists(file))
  test_multimodal = read.csv(file)
  expect_equal(nrow(test_multimodal), 2)
  expect_equal(ncol(test_multimodal), 31)
  expect_equal(test_multimodal$trip_numbers, c(1, 2))
  expect_equal(test_multimodal$start, c("2023-11-30T13:50:00Z", "2023-11-30T14:25:00Z"))
  expect_equal(test_multimodal$end, c("2023-11-30T13:54:00Z", "2023-11-30T14:29:00Z"))
  
  # Check formula_log
  file = paste0(outdir, "/hbGIS_output/formula_log.csv")
  expect_true(file.exists(file))
  test_formula_log = read.csv(file)
  expect_equal(nrow(test_formula_log), 45)
  expect_equal(ncol(test_formula_log), 7)
  
  # Clean up
  if (dir.exists(dn))  unlink(dn, recursive = TRUE)
  if (dir.exists(outdir))  unlink(outdir, recursive = TRUE)
  if (file.exists("./output_test_error_list.csv")) file.remove("./output_test_error_list.csv")
})
