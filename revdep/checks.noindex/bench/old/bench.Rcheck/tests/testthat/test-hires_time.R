describe("hires_time", {
  skip_on_cran()

  it("returns hi resolution times", {
    # it is hard to test this, we will just sleep and verify the second time is
    # after the first.
    start <- hires_time()
    Sys.sleep(.1)
    end <- hires_time()
    expect_type(start, "double")
    expect_true(end > start)
  })
})
