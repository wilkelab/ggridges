# mark: Can errors with the deparsed expressions

    Code
      mark(1, 1, 3, max_iterations = 10)
    Condition
      Error:
      ! Each result must equal the first result:
      `1` does not equal `3`

# mark: Works when calls are different lengths

    Code
      mark(if (TRUE) 2, if (TRUE) 1 else 3)
    Condition
      Error:
      ! Each result must equal the first result:
      `if (TRUE) 2` does not equal `if (TRUE) 1 else 3`

# mark: truncates long expressions when printing (#94)

    Code
      out
    Output
      # A tibble: 1 x 2
        expression            result
        <bch:expr>            <list>
      1 aaaaaaaaaaaaaaaaaaaa~ <dbl> 

