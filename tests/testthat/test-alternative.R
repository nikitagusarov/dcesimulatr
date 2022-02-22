####################
# Test alternative #
####################

# General object test
test_that(
  "object is generated", 
  {
    # Init
    alt = alternative$new()

    # Test
    expect_true(
      all(
        class(alt) == c("alternative", "R6")
      )
    )
  }
)

# Test
test_that(
  "adding attributes", 
  {
    # Init
    alt = alternative$new()
    alt$add_attributes(
      Price = rnorm(mean = 5)
    )

    # Test
    expect_true(
      all(
        class(alt) == c("alternative", "R6")
      )
    )
    expect_identical(
      names(alt$attributes), 
      "Price"
    )
    expect_true(
      any(
        class(alt$attributes$Price) == "call"
      )
    )

    # Overwrite chars
    alt$add_attributes(
      Opinion = rnorm(mean = 10), 
      Quality = rexp(scale = 5)
    )

    # Test
    expect_identical(
      names(alt$attributes), 
      c("Price", "Opinion", "Quality")
    )
    expect_true(
      any(
        class(alt$attributes$Quality) == "call"
      )
    )
  }
)