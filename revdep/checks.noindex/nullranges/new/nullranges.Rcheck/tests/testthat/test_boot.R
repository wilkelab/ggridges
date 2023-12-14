library(nullranges)
test_that("unsegmented and segmented block bootstrap work", {

  # construct some data for unit testing
  
  library(GenomicRanges)
  seq_nms <- rep(c("chr1","chr2","chr3"),c(4,5,2))
  gr <- GRanges(seqnames=seq_nms,
                IRanges(start=c(1,101,121,201,
                                101,201,216,231,401,
                                1,101),
                        width=c(20, 5, 5, 30,
                                20, 5, 5, 5, 30,
                                80, 40)),
                seqlengths=c(chr1=300,chr2=450,chr3=200),
                chrom=as.integer(factor(seq_nms)))

  blockLength <- 100

  ##########################
  ## permute within chrom ##
  ##########################

  library(plyranges)
  
  R <- 5
  set.seed(1)
  gr_prime <- bootRanges(gr, blockLength, R, type="permute", withinChrom=TRUE)
  
  # expect same number of features per chrom
  count_per_iter <- gr_prime %>%
    group_by(iter, chrom) %>%
    summarize(tally=n())
  obs <- gr %>% group_by(chrom) %>% summarize(tally=n())
  expect_equal(count_per_iter$tally, rep(obs$tally, R))

  #################################
  ## bootstrap within chromosome ##
  #################################

  set.seed(1)
  gr_prime <- bootRanges(gr, blockLength, R, type="bootstrap", withinChrom=TRUE)

  # expect only 1 distinct chromosomes of origin per chromosome
  count_origin <- gr_prime %>%
    group_by(iter, seqnames) %>%
    summarize(origin = n_distinct(chrom))
  expect_true(all(count_origin$origin == 1))

  ##########################
  ## permute across chrom ##
  ##########################

  set.seed(1)
  gr_prime <- bootRanges(gr, blockLength, R, type="permute", withinChrom=FALSE)

  # expect the per iteration number of features to be constant
  count_per_iter <- gr_prime %>%
    group_by(iter) %>%
    summarize(tally = n())
  expect_equal(count_per_iter$tally, rep(length(gr), R))
  
  ############################
  ## bootstrap across chrom ##
  ############################

  set.seed(1)
  gr_prime <- bootRanges(gr, blockLength, R, type="bootstrap", withinChrom=FALSE)

  # total is now random, very unlikely to get exactly the input number each time
  count_per_iter <- gr_prime %>%
    group_by(iter) %>%
    summarize(tally = n())
  expect_true(!all(count_per_iter$tally == length(gr)))
  
  ###############################
  ## segmented block bootstrap ##
  ###############################

  seg <- GRanges(c("chr1","chr1","chr2","chr2","chr3"),
                 IRanges(c(1,151,1,226,1),c(150,300,225,450,200)),
                 state=c(1,2,1,2,1))
  gr_seg <- gr %>%
    join_overlap_left(seg)
  
  set.seed(1)
  gr_prime <- bootRanges(gr_seg, blockLength, R, seg)

  # while we can carry ranges across state, this shouldn't happen the majority of the time
  count_state <- gr_prime %>%
    mutate(original_state = state) %>%
    select(-state) %>% 
    join_overlap_left(seg) %>%
    select(original_state, state, .drop_ranges=TRUE)
  same_rate <- prop.table(table(count_state$original_state == count_state$state))["TRUE"]
  expect_true(same_rate > .5)
  
})
