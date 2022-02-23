#################################
# 1. Test "generation" function #
#################################



# Test generation
test_that(
    "generation function works (default params)",
    {
        # Init Population
        # Generate dummy ind
        ind1 = individual$new()
        ind1$add_characteristics(
            Age = rnorm(sd = 10),
            Income = rnorm(mean = 1, sd = 5)
        )
        ind2 = individual$new()
        ind2$add_characteristics(
            Income = rexp(rate = 10)
        )
        pop = population$new(
            list(ind1, ind2), 
            n = list(10, 15)
        )

        # Init experimental design
        # Generate dummy alternative
        alt1 = alternative$new()
        alt1$add_attributes(
            Price = rnorm(sd = 10)
        )
        alt2 = alternative$new()
        alt2$add_attributes(
            Opinion = rnorm(mean = 10), 
            Quality = rexp(rate = 5)
        )
        e_design = experimental_design$new(
            list(alt1, alt2)
        )

        # Call generation function
        exp_res = experiment_compose(
            population = pop,
            experimental_design = e_design
        )

        expect_true(
            class(exp_res) == "data.frame"
        )
        expect_true(
            all(dim(exp_res) == c(50, 8))
        )
        expect_true(
            all(colnames(exp_res) == c(
                "Age", "Income", "class", 
                "Price", "Opinion", "Quality", "alternative", "CID"
            ))
        )
    }
)

# Test generation
test_that(
    "generation function works (non-default params)",
    {
        # Init Population
        # Generate dummy ind
        ind1 = individual$new()
        ind1$add_characteristics(
            Age = rnorm(sd = 10),
            Income = rnorm(mean = 1, sd = 5)
        )
        ind2 = individual$new()
        ind2$add_characteristics(
            Income = rexp(rate = 10)
        )
        pop = population$new(
            list(ind1, ind2), 
            n = list(10, 15)
        )

        # Init experimental design
        # Generate dummy alternative
        alt1 = alternative$new()
        alt1$add_attributes(
            Price = rnorm(sd = 10)
        )
        alt2 = alternative$new()
        alt2$add_attributes(
            Opinion = rnorm(mean = 10), 
            Quality = rexp(rate = 5)
        )
        e_design = experimental_design$new(
            list(alt1, alt2), n = 3, identical = FALSE
        )

        # Call generation function
        exp_res = experiment_compose(
            population = pop,
            experimental_design = e_design
        )

        expect_true(
            class(exp_res) == "data.frame"
        )
        expect_true(
            all(dim(exp_res) == c(150, 8))
        )
        expect_true(
            all(colnames(exp_res) == c(
                "Age", "Income", "class", 
                "Price", "Opinion", "Quality", "alternative", "CID"
            ))
        )
        expect_true(
            sum(exp_res[1:6, 4:6] == exp_res[7:12, 4:6], na.rm = TRUE) == 0
        )

        # Identical = FALSE
        e_design = experimental_design$new(
            list(alt1, alt2), n = 3, identical = TRUE
        )

        # Call generation function
        exp_res = experiment_compose(
            population = pop,
            experimental_design = e_design
        )

        expect_true(
            class(exp_res) == "data.frame"
        )
        expect_true(
            all(dim(exp_res) == c(150, 8))
        )
        expect_true(
            all(colnames(exp_res) == c(
                "Age", "Income", "class", 
                "Price", "Opinion", "Quality", "alternative", "CID"
            ))
        )
        expect_true(
            all(colnames(exp_res) == c(
                "Age", "Income", "class", 
                "Price", "Opinion", "Quality", "alternative", "CID"
            ))
        )
        expect_true(
            sum(exp_res[1:6, 4:7] == exp_res[7:12, 4:7], na.rm = TRUE) == 15
        )
    }
)



#####################################
# 2. Test "experiment_compose" function #
#####################################



# Test generation
test_that(
    "experiment is run",
    {
        # Init Population
        # Generate dummy ind
        ind1 = individual$new()
        ind1$add_characteristics(
            Age = rnorm(sd = 10),
            Income = rnorm(mean = 1, sd = 5)
        )
        ind1$add_decision_rule(
            noise = evd::rgumbel(loc = 0, scale = 1),
            formula = 1.5*Age + 0.7*Quality
        )
        ind2 = individual$new()
        ind2$add_characteristics(
            Income = rexp(rate = 10)
        )
        ind2$add_decision_rule(
            noise = evd::rgumbel(loc = 0, scale = 1),
            formula = Age + 2*Income - 0.5*Quality
        )
        pop = population$new(
            list(ind1, ind2), 
            n = list(10, 15)
        )

        # Init experimental design
        # Generate dummy alternative
        alt1 = alternative$new()
        alt1$add_attributes(
            Price = rnorm(sd = 10), 
            Quality = rexp(rate = 2)
        )
        alt2 = alternative$new()
        alt2$add_attributes(
            Opinion = rnorm(mean = 10), 
            Quality = rexp(rate = 5)
        )
        e_design = experimental_design$new(
            list(alt1, alt2), n = 3
        )

        # Call generation function
        res = experiment_run(
            population = pop,
            experimental_design = e_design
        )

        expect_true(
            class(res) == "data.frame"
        )
        expect_true(
            all(dim(res) == c(150, 10))
        )
        expect_true(
            all(colnames(res) == c(
                "Age", "Income", "class", 
                "Price", "Quality", "Opinion", "alternative", "CID",
                "DU", "TU"
            ))
        )
    }
)