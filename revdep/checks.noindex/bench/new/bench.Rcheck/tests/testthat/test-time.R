describe("as_bench_time", {
  it("accepts numeric input unchanged", {
    expect_equal(unclass(as_bench_time(123L)), 123L)
    expect_equal(unclass(as_bench_time(123)), 123)
  })
  it("accepts bench_byte input unchanged", {
    x <- as_bench_time(123)
    expect_equal(as_bench_time(x), x)
  })
  it("coerces character input", {
    expect_equal(unclass(as_bench_time("1")), 1)
    expect_equal(unclass(as_bench_time("1ns")), 1e-9)
    expect_equal(unclass(as_bench_time("1us")), 1e-6)
    expect_equal(unclass(as_bench_time("1ms")), 1e-3)
    expect_equal(unclass(as_bench_time("1s")), 1)
    expect_equal(unclass(as_bench_time("1m")), 60)
    expect_equal(unclass(as_bench_time("1h")), 60 * 60)
    expect_equal(unclass(as_bench_time("1d")), 60 * 60 * 24)
    expect_equal(unclass(as_bench_time("1w")), 60 * 60 * 24 * 7)
  })
})

describe("format.as_bench_time", {
  it("formats times under 60 as whole numbers", {
    expect_equal(format(as_bench_time(59)), "59s")
    expect_equal(format(as_bench_time(1)), "1s")
  })
  it("formats times 60 and up as abbreviated minutes / hours / days", {
    withr::with_options(list("cli.unicode" = FALSE), {
      expect_equal(format(as_bench_time(.000000005)), "5ns")
      expect_equal(format(as_bench_time(.0000005)), "500ns")
      expect_equal(format(as_bench_time(.000005)), "5us")
      expect_equal(format(as_bench_time(.0005)), "500us")
      expect_equal(format(as_bench_time(.005)), "5ms")
      expect_equal(format(as_bench_time(.5)), "500ms")
      expect_equal(format(as_bench_time(30)), "30s")
      expect_equal(format(as_bench_time(60)), "1m")
      expect_equal(format(as_bench_time(90)), "1.5m")
      expect_equal(format(as_bench_time(90 * 60)), "1.5h")
      expect_equal(format(as_bench_time(60 * 60 * 60)), "2.5d")
      expect_equal(format(as_bench_time(10.5 * 24 * 60 * 60)), "1.5w")
    })
  })
  it("handles NA and NaN and Inf", {
    expect_equal(format(as_bench_time(NA)), "NA")
    expect_equal(format(as_bench_time(NaN)), "NaN")
    expect_equal(format(as_bench_time(Inf)), "Inf")
    expect_equal(format(as_bench_time(-Inf)), "-Inf")
  })
  it("works with vectors", {
    v <- c(NA, .001, 60, 600, NaN, 6000)
    expect_equal(
      format(as_bench_time(v), trim = TRUE),
      c("NA", "1ms", "1m", "10m", "NaN", "1.67h"))

    expect_equal(format(as_bench_time(numeric())), character())
  })
})

describe("sum.as_bench_time", {
  it("sums its input and returns a bench_byte", {
    expect_equal(sum(as_bench_time(0)), new_bench_time(0))
    expect_equal(sum(as_bench_time(c(1, 2))), new_bench_time(3))
    expect_equal(sum(as_bench_time(c(1, NA))), new_bench_time(NA_real_))
  })
})

describe("min.as_bench_time", {
  it("finds minimum input and returns a bench_byte", {
    expect_equal(min(as_bench_time(0)), new_bench_time(0))
    expect_equal(min(as_bench_time(c(1, 2))), new_bench_time(1))
    expect_equal(min(as_bench_time(c(1, NA))), new_bench_time(NA_real_))
  })
})

describe("max.as_bench_time", {
  it("finds maximum input and returns a bench_byte", {
    expect_equal(max(as_bench_time(0)), new_bench_time(0))
    expect_equal(max(as_bench_time(c(1, 2))), new_bench_time(2))
    expect_equal(max(as_bench_time(c(1, NA))), new_bench_time(NA_real_))
  })
})

describe("[.as_bench_time", {
  it("retains the as_bench_time class", {
    x <- as_bench_time(c(100, 200, 300))
    expect_equal(x[], x)
    expect_equal(x[1], new_bench_time(100))
    expect_equal(x[1:2], new_bench_time(c(100, 200)))
  })
})

describe("Ops.as_bench_time", {
  it("errors for unary operators", {
    x <- as_bench_time(c(100, 200, 300))
    expect_error(!x, "unary '!' not defined for \"bench_time\" objects")
    expect_error(+x, "unary '\\+' not defined for \"bench_time\" objects")
    expect_error(-x, "unary '-' not defined for \"bench_time\" objects")
  })

  it("works with boolean comparison operators", {
    x <- as_bench_time(c(100, 200, 300))

    expect_equal(x == 100, c(TRUE, FALSE, FALSE))
    expect_equal(x != 100, c(FALSE, TRUE, TRUE))
    expect_equal(x > 100, c(FALSE, TRUE, TRUE))
    expect_equal(x >= 100, c(TRUE, TRUE, TRUE))
    expect_equal(x < 200, c(TRUE, FALSE, FALSE))
    expect_equal(x <= 200, c(TRUE, TRUE, FALSE))
  })

  it("works with arithmetic operators", {
    x <- as_bench_time(c(100, 200, 300))

    expect_equal(x + 100, as_bench_time(c(200, 300, 400)))
    expect_equal(x - 100, as_bench_time(c(0, 100, 200)))
    expect_equal(x * 100, as_bench_time(c(10000, 20000, 30000)))
    expect_equal(x / 2, as_bench_time(c(50, 100, 150)))
    expect_equal(x ^ 2, as_bench_time(c(10000, 40000, 90000)))
  })

  it("errors for other binary operators", {
    x <- as_bench_time(c(100, 200, 300))
    expect_error(x %/% 2, "'%/%' not defined for \"bench_time\" objects")
    expect_error(x & TRUE, "'&' not defined for \"bench_time\" objects")
    expect_error(x | TRUE, "'|' not defined for \"bench_time\" objects")
  })
})
