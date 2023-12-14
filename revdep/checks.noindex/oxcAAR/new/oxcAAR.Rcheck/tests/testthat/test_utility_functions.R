context("getOxcalExecutablePath")

test_that("getOxcalExecutablePath returns error if not set already", {
  expect_error(oxcAAR:::getOxcalExecutablePath(), "Please set path to oxcal first")
})

context("setOxcalExecutablePath")

test_that("setOxcalExecutablePath sets the oxcal path in the environment variable",{
  setOxcalExecutablePath("ox_output.js")
  expect_true(options("oxcAAR.oxcal_path")=="ox_output.js")
})

context("getOxcalExecutablePath")

test_that("getOxcalExecutablePath returns no error if set already", {
  expect_error(oxcAAR:::getOxcalExecutablePath(), NA)
})

test_that("getOxcalExecutablePath returns path set before", {
  expect_equal(oxcAAR:::getOxcalExecutablePath(), "ox_output.js")
})

test_that("setOxcalExecutablePath complains when file does not exists",{
  expect_error(setOxcalExecutablePath("i_am_not_here.file"), "No file at given location")
})

context("quickSetupOxcal")
test_that("quickSetupOxcal downloads oxcal and sets correct path",{
  skip_on_cran()
  expect_error(quickSetupOxcal(), NA)
  expect_true(dir.exists(paste0(tempdir(), "/OxCal/bin/")))
  expect_true(basename(options("oxcAAR.oxcal_path")[[1]]) %in% c("OxCalLinux",
                                                                 "OxCalWin.exe",
                                                                 "OxCalMac"))
  unlink("OxCal/", recursive = T)
})

context("formatDateAdBc")
test_that("formatDateAdBc can handle NAs",{
  expect_error(oxcAAR:::formatDateAdBc(NA), NA)
  expect_equal(oxcAAR:::formatDateAdBc(NA), "NA")
})

precise_sigma_range <- data.frame(start=-3956.5,
                                  end = -3643,
                                  probability = 99.73002)

context("formatFullSigmaRange")
test_that("formatFullSigmaRange should have a precision of 2",{
  sigma_text <- oxcAAR:::formatFullSigmaRange(precise_sigma_range, "name")
  sigma_prescision <- stringr::str_extract_all(sigma_text, pattern = "(?<=\\().+?(?=%\\))")[[1]]
  expect_equal(as.numeric(sigma_prescision), round(as.numeric(sigma_prescision),2))
})
