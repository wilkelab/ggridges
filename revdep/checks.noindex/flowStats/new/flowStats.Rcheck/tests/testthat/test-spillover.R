context("Test spillover methods")

control_path <- system.file("extdata", "compdata", "data",
                            package="flowCore")
matchfile_path <- system.file("extdata", "compdata", "comp_match",
                              package="flowCore")
controls <- lapply(dir(control_path, full.names=TRUE), 
                   read.FCS)
filenames <- sapply(controls, keyword, "$FIL")
names(controls) <- c("UNSTAINED", "FL1-H", "FL2-H", "FL4-H", "FL3-H")
controls <- as(controls, "flowSet")
# Scramble the columns (particularly fsc, ssc) to make sure
# the methods still handle it appropriately
controls <- controls[,c(3,1,4,5,2,6,7)]


test_that("spillover: Match columns using regexpr", {
  ref_file <- system.file("extdata", "compdata", "compref1", package="flowCore")
  comp_ref <- as.matrix(read.table(ref_file, check.names = FALSE))
  comp <- spillover(controls, unstained = "UNSTAINED", fsc="FSC-H",
                    ssc="SSC-H", patt = "-H", stain_match="regexpr",
                    useNormFilt = FALSE)
  
  expect_equal(colnames(comp), colnames(comp_ref))
  expect_equal(rownames(comp), rownames(comp_ref))
  expect_equivalent(comp, comp_ref, tolerance=3e-08)
})

test_that("spillover: Match columns using ordered", {
  # The spillover matrix here should (appropriately) be incorrect bc the rows are
  # not in the order of the columns. In fact we expect a warning as we'll check now for
  # multiple off diagonal entries greater than 1.
  comp<-expect_warning(spillover(controls, unstained = "UNSTAINED", fsc="FSC-H",
                    ssc="SSC-H", patt = "-H", stain_match="ordered",
                    useNormFilt = FALSE))
  comp_ref <-
structure(c(1, 0.0077220477483574751, 0.23582570418697035, 0.065683952675143667, 
0.24202227763070464, 1, 0.29890069124793922, 0.76478483921335927, 
0.032083705820198934, 0.14078823194024204, 1, 4.3555171757803084, 
0.0011278155536909003, 0.0026326890940635483, 310.69275900646016, 
1), .Dim = c(4L, 4L), .Dimnames = list(c("FL1-H", "FL2-H", "FL3-H", 
"FL4-H"), c("FL1-H", "FL2-H", "FL3-H", "FL4-H")))
expect_equivalent(comp,comp_ref)
})

test_that("spillover: Match columns using intensity", {
  ref_file <- system.file("extdata", "compdata", "compref1", package="flowCore")
  comp_ref <- as.matrix(read.table(ref_file, check.names = FALSE))
  comp <- spillover(controls, unstained = "UNSTAINED", fsc="FSC-H",
                    ssc="SSC-H", patt = "-H", stain_match="intensity",
                    useNormFilt = FALSE)
  expect_equal(colnames(comp), colnames(comp_ref))
  expect_equal(rownames(comp), rownames(comp_ref))
  expect_equivalent(comp, comp_ref, tolerance=3e-08)
})

test_that("spillover_match: Using path to dir with files", {
  ref_file <- system.file("extdata", "compdata", "compref3", package="flowCore")
  comp_ref <- as.matrix(read.table(ref_file, check.names = FALSE))
  matched <- spillover_match(path=control_path, fsc="FSC-H", ssc="SSC-H", matchfile = matchfile_path)
  comp <- spillover(matched, prematched = TRUE, fsc="FSC-H", ssc="SSC-H", patt = "-H",
                    useNormFilt = FALSE, method = "mode")
  expect_equal(colnames(comp), colnames(comp_ref))
  expect_equal(rownames(comp), rownames(comp_ref))
  expect_equivalent(comp, comp_ref, tolerance=3e-08)
})

test_that("spillover_match: Using preconstructed flowSet with filenames as sample names", {
  sampleNames(controls) <- filenames
  ref_file <- system.file("extdata", "compdata", "compref3", package="flowCore")
  comp_ref <- as.matrix(read.table(ref_file, check.names = FALSE))
  matched <- spillover_match(controls, fsc="FSC-H", ssc="SSC-H", matchfile = matchfile_path)
  comp <- spillover(matched, prematched = TRUE, fsc="FSC-H", ssc="SSC-H", patt = "-H",
                    useNormFilt = FALSE, method = "mode")
  expect_equal(colnames(comp), colnames(comp_ref))
  expect_equal(rownames(comp), rownames(comp_ref))
  expect_equivalent(comp, comp_ref, tolerance=3e-08)
})

test_that("spillover_ng: Using path to dir with files", {
  ref_file <- system.file("extdata", "compdata", "compref4", package="flowCore")
  comp_ref <- as.matrix(read.table(ref_file, check.names = FALSE))
  comp <- spillover_ng(path=control_path, fsc="FSC-H", ssc="SSC-H", 
                       patt="-H", matchfile = matchfile_path, pregate = FALSE,
                       useNormFilt = FALSE)
  expect_equal(colnames(comp), colnames(comp_ref))
  expect_equal(rownames(comp), rownames(comp_ref))
  expect_equivalent(comp, comp_ref, tolerance=3e-08)
})

test_that("spillover_ng: Using preconstructed flowSet with filenames as sample names", {
  sampleNames(controls) <- filenames
  ref_file <- system.file("extdata", "compdata", "compref4", package="flowCore")
  comp_ref <- as.matrix(read.table(ref_file, check.names = FALSE))
  matched <- spillover_match(controls, fsc="FSC-H", ssc="SSC-H", matchfile = matchfile_path)
  comp <- spillover_ng(path=control_path, fsc="FSC-H", ssc="SSC-H", 
                       patt="-H", matchfile = matchfile_path, pregate = FALSE,
                       useNormFilt = FALSE)
  expect_equal(colnames(comp), colnames(comp_ref))
  expect_equal(rownames(comp), rownames(comp_ref))
  expect_equivalent(comp, comp_ref, tolerance=3e-08)
})

test_that("spillover_ng: Using preconstructed flowSet with channels in new order", {
  match <- read.csv(matchfile_path, colClasses = "character")
  fs <- read.flowSet(files=match$filename, path=control_path)

  # Scramble the channel order
  fs <- fs[,c("FSC-H", "FL4-H", "FL3-H", "FL1-A", "FL1-H", "FL2-H", "SSC-H")]
  # Note that the order of the channels used for compensation (FL4-H, FL3-H, FL1-H, FL2-H)
  # is c(3, 4, 2, 1), which has the property that
  # order(c(3, 4, 2, 1)) != c(3, 4, 2, 1).
  # This is in contrast to the channel order at the top of the file,
  # where order(c(2, 1, 3, 4)) == c(2, 1, 3, 4).
  # The channel order used here is important for testing the correctness of the
  # approach used to re-order rows of the matrix in the spillover function.
  
  comp <- spillover_ng(x=fs, fsc="FSC-H", ssc="SSC-H", 
                       patt="-H", matchfile = matchfile_path, pregate = FALSE,
                       useNormFilt = FALSE)
  # The diagonals should be equal to 1
  for (i in 1:4) {
    expect_equal(comp[i,i], 1)
  }
  comp_order <- c(3, 4, 2, 1)
  # If you unscramble the rows and columns, you should get the original comp_ref
  comp_ordered <- comp[comp_order, comp_order]
  ref_file <- system.file("extdata", "compdata", "compref4", package="flowCore")
  comp_ref <- as.matrix(read.table(ref_file, check.names = FALSE))
  expect_equal(colnames(comp_ordered), colnames(comp_ref))
  expect_equal(rownames(comp_ordered), rownames(comp_ref))
  expect_equivalent(comp_ordered, comp_ref, tolerance=3e-08)
})

