context("oxcalSimulate")

test_that("oxcalSimulate produces error given wrong oxcal result file", {
  with_mock(
    `executeOxcalScript`= function(...) {
      return("ox_output_wrong.js");
    }, {
      expect_error( suppressWarnings(oxcalSimulate(5000,25,"KIA-12345")))
    })
})

with_mock(
  `executeOxcalScript`= function(...) {
    return("ox_output.js");
  }, {

    test_that("oxcalSimulate is processed without error given oxcal result file", {
      expect_error( suppressWarnings(oxcalSimulate(5000,25,"KIA-12345")), NA)
    })

    test_that("oxcalSimulate produces an oxcalSimulatedDatesList", {
      this_result <- oxcalSimulate(5000,25,"KIA-12345")
      expect_equal(class(this_result), c("list", "oxcAARCalibratedDatesList"))
    })

    test_that("oxcalSimulate produces an oxcalCalibratedDatesList with correct oxcalCalibratedDates", {
      this_result <- oxcalSimulate(5000,25,"KIA-12345")[[1]]
      expect_equal(class(this_result), c("oxcAARCalibratedDate"))
      expect_equal(names(this_result), c("name",
                                         "type",
                                         "bp",
                                         "std",
                                         "cal_curve",
                                         "sigma_ranges",
                                         "raw_probabilities",
                                         "posterior_sigma_ranges",
                                         "posterior_probabilities"))
      expect_equal(class(this_result$name), "character")
      expect_equal(class(this_result$type), "character")
      expect_equal(class(this_result$bp), "integer")
      expect_equal(class(this_result$std), "integer")
      expect_equal(class(this_result$cal_curve), "list")
      expect_equal(names(this_result$cal_curve), c("name","resolution","bp","bc","sigma"))
      expect_equal(class(this_result$cal_curve$name), "character")
      expect_equal(class(this_result$cal_curve$resolution), "numeric")
      expect_equal(class(this_result$cal_curve$bp), "numeric")
      expect_equal(class(this_result$cal_curve$bc), "numeric")
      expect_equal(class(this_result$cal_curve$sigma), "numeric")
      expect_true(length(this_result$cal_curve$sigma)==length(this_result$cal_curve$bc))
      expect_true(length(this_result$cal_curve$bc)==length(this_result$cal_curve$bp))
      expect_equal(class(this_result$sigma_ranges), "list")
      expect_equal(names(this_result$sigma_ranges), c("one_sigma", "two_sigma", "three_sigma"))
      expect_equal(class(this_result$raw_probabilities), "data.frame")
      expect_true(this_result$sigma_ranges$one_sigma[1]>=min(this_result$raw_probabilities$dates) &&
                    this_result$sigma_ranges$one_sigma[1]<=max(this_result$raw_probabilities$dates) &&
                    this_result$sigma_ranges$one_sigma[2]>=min(this_result$raw_probabilities$dates) &&
                    this_result$sigma_ranges$one_sigma[2]<=max(this_result$raw_probabilities$dates)
                    )
    })

  })
