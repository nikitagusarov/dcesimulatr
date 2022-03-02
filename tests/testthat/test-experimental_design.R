#######################################
# 1. Test "experimental_design" class #
#######################################

# Test generation
test_that(
  "object creation",
  {
    # Init empty
    e_design <- experimental_design$new()

    # Test
    expect_true(
      all(
        class(e_design) == c("experimental_design", "R6")
      )
    )
    expect_true(
      e_design$design == "random"
    )

    # Init filled
    # Generate dummy alternative
    alt1 <- alternative$new()
    alt1$add_attributes(
      Price = rnorm(sd = 10)
    )
    alt2 <- alternative$new()
    alt2$add_attributes(
      Opinion = rnorm(mean = 10),
      Quality = rexp(rate = 5)
    )
    # Fill with corresponding alternative
    e_design <- experimental_design$new(
      list(alt1)
    )

    # Test
    expect_true(
      all(
        class(e_design) == c("experimental_design", "R6")
      )
    )
    expect_true(
      all(
        class(e_design$alternatives[[1]]) == c("alternative", "R6")
      )
    )

    # Fill with corresponding ind
    e_design <- experimental_design$new(
      alternatives = list(alt1, alt2)
    )

    # Test
    expect_true(
      all(
        class(e_design) == c("experimental_design", "R6")
      )
    )
    expect_true(
      all(
        class(e_design$alternatives[[1]]) == c("alternative", "R6"),
        class(e_design$alternatives[[2]]) == c("alternative", "R6")
      )
    )
  }
)



##############################################################
# 2. Test functions operating in "experimental_design" class #
##############################################################

# Fully random specification

test_that(
  "experimental_design generation function test (random)",
  {
    # Init filled
    # Generate dummy alternative
    alt1 <- alternative$new()
    alt1$add_attributes(
      Price = rnorm(sd = 10)
    )
    alt2 <- alternative$new()
    alt2$add_attributes(
      Opinion = rnorm(mean = 10),
      Quality = rexp(rate = 5)
    )
    # Fill with corresponding alternative
    e_design <- experimental_design$new(
      list(alt1, alt2),
      n = 10
    )

    # Generate Z
    Z <- alternatives_gen(e_design)

    # Test
    expect_true(
      class(Z) == "data.frame"
    )
    expect_true(
      all(colnames(Z) == c("Price", "Opinion", "Quality", "AID", "CID"))
    )
    expect_true(
      all(dim(Z) == c(20, 5))
    )
  }
)

# Fully factorial specification

test_that(
  "experimental_design generation function test (factorial)",
  {
    # Init filled
    # Generate dummy alternative
    alt1 <- alternative$new()
    alt1$add_attributes(
      Price = c(1:4)
    )
    alt2 <- alternative$new()
    alt2$add_attributes(
      Opinion = c(0:1),
      Quality = c(0:1)
    )
    # Fill with corresponding alternative
    e_design <- experimental_design$new(
      list(alt1, alt2),
      design = "factorial",
      n = 10
    )

    # Generate Z
    Z <- alternatives_gen(e_design)

    # Test
    expect_true(
      class(Z) == "data.frame"
    )
    expect_true(
      all(colnames(Z) == c("CID", "AID", "Price", "Opinion", "Quality"))
    )
    expect_true(
      all(dim(Z) == c(32, 5))
    )
  }
)

# Mixed specification

test_that(
  "experimental_design generation function test (mixed)",
  {
    # Init filled
    # Generate dummy alternative
    alt1 <- alternative$new()
    alt1$add_attributes(
      Price = c(1:4)
    )
    alt2 <- alternative$new()
    alt2$add_attributes(
      Opinion = c(0:1),
      Quality = rnorm(mean = 1, sd = 3)
    )
    # Fill with corresponding alternative
    e_design <- experimental_design$new(
      list(alt1, alt2),
      design = "mixed",
      n = 10
    )

    # Generate Z
    Z <- alternatives_gen(e_design)

    # Test
    expect_true(
      class(Z) == "data.frame"
    )
    expect_true(
      all(colnames(Z) == c("CID", "AID", "Price", "Opinion", "Quality"))
    )
    expect_true(
      all(dim(Z) == c(16, 5))
    )
  }
)
