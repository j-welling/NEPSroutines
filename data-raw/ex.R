#'
#' Creates the simulated data sets resembling the NEPS data structure
#'


#' Simulate a Rasch testlet model following Wang and Wilson (2005)
#'
#' @description
#' `sim.testlet()` simulates responses according to a testlet model
#' that loosely resemble the data structures in the NEPS. This includes tests
#' with dichotomous and/or polytomous items, tests with multiple booklets that
#' contain unique items, and different types of missing values.
#'
#' Polytomous items will include a mixed number of response categories.
#' For dichotomous variables also unscored items will be generated.
#' These represent multiple-choice items with four response options. The
#' correct response option is randomly selected for each item.
#'
#' Each simulation includes three DIF variables. For `sex` (0 = male,
#' 1 = female) and `school` (0 = lower, 1 = intermediate, 2 = upper) there will
#' be no missing values, while for `mig` (0 = no, 1 = yes) about 20% of the
#' sample will have missing values. No DIF will be introduced for the three
#' variables, only main effects.
#'
#' If multiple booklets are requested, each booklet will contain several unique
#' items, while the remaining items will be common across booklets. The common
#' items will have lower item number, whereas unique items have higher item
#' numbers. The respondents will be randomly assigned to one of the booklets.
#'
#' The model includes 5 testlets to introduce some multidimensionality.
#'
#' The simulations follow the procedure outlined in Wang and Wilson (2005).
#'
#' Wang, W. C., & Wilson, M. (2005). The Rasch testlet model. Applied
#' Psychological Measurement, 29(2), 126-149.
#' https://doi.org/10.1177/0146621604271053
#'
#' @param N The total number of respondents.
#' @param I The total number of items.
#' @param poly The number of polytomous items;`I-poly` items will be
#' dichotomous.
#' @param booklets The number of test booklets. Each booklet will have 5
#' unique items; `I-5*booklets` items will be common across all booklets.
#' @param rotation A logical to include a test rotation with two positions
#' (first and second).
#' @param seed A random seed for reproducible results.
#' @param control A list of control settings:
#' * var.theta: Variance of the population abilities
#' * var.theta: Variance of each population testlet effect
#' * main.sex: Main effect for the DIF variable `sex`
#' * main.mig: Main effect for the DIF variable `mig`
#' * main.school: Main effect for the DIF variable `school`
#' * main.rotation: Main effect for the test rotations
#' * main.booklet: Main effect for the test booklets
#' * varname: Stem for the variable names. After the stem the item number is
#' added with four digits. Scored items are additionally appended with "_c".
#' * not.participated: Number of additional respondents that did not
#' participate and, thus, have missing code -56 on all variables. These are
#' added to the total sample size `N`.
#' * unique: The number of unique items in each booklet. The length of the
#' vector must match the number given in `booklets`. Otherwise, the same
#' number of unique items is used for all booklets.
#' * testlets: The number of testlets for multidimensionality.
#' * mis.mig: Percentage of missing values in DIF variable `mig`
#' * invalid: Percentage of invalid responses
#' * omitted: Percentage of omitted responses
#' @returns A list with two elements:
#' * resp: A data frame with the simulated item responses and respondent
#' characteristics.
#' * vars: A data frame with item characteristics.
sim.testlet <- function(N = 2000, I = 25, poly = 5, booklets = 1,
                        rotation = TRUE, seed = 6237, control = list()) {

  set.seed(seed)

  # Normalize arguments
  N <- as.integer(N[1])
  I <- as.integer(I[1])
  poly <- as.integer(poly[1])
  booklets <- as.integer(booklets[1])
  rotation <- as.logical(rotation[1])
  if (!is.list(control)) control <- list()
  cntrl <-
    list(var.theta = 1, var.testlet = 0.5, main.sex = 0.40, main.mig = 0.80,
         main.booklet = 0.50, main.rotation = 0.20, main.school = 0.50,
         varname = "mag5", not.participated = 10, unique = 5, testlets = 5,
         mis.mig = 0.2, invalid = 0.01, omitted = 0.03)
  control <- utils::modifyList(cntrl, control)
  if (booklets != length(control$unique))
    control$unique <- rep(control$unique[1], booklets)

  # Check arguments
  if (poly > I)
    stop(paste0("The number of polytomous items 'poly' must not be larger ",
                "than the total number of items 'I'!"))
  if (I - sum(control$unique) < 5)
    stop(paste0("The number of items 'I' is too small for the number of",
                "requested booklets! Please make sure that at least 5 common ",
                "items are available."))
  if (I < 15)
    stop(paste0("The minimum number items that can be used is 15!"))

  # Update sample size
  N <- N + control$not.participated

  # Create respondent variables
  Y <- data.frame(
    ID_t = seq_len(N),
    sex = base::sample(c(0, 1), N, replace = TRUE),
    mig = base::sample(c(0, 1), N, replace = TRUE),
    school = base::sample(c(0, 1, 2), N, replace = TRUE)
  )
  attr(Y$sex, "labels") <- c("male" = 0, "female" = 1)
  attr(Y$mig, "labels") <- c("no" = 0, "yes" = 1)
  attr(Y$school, "labels") <- c("lower" = 0, "medium" = 1, "upper" = 2)
  if (rotation) {
    Y$rotation <- base::sample(c(0, 1), N, replace = TRUE)
    attr(Y$rotation, "labels") <- c("first" = 0, "second" = 1)
  }
  if (booklets > 1L) {
    Y$booklet <- base::sample(seq_len(booklets), N, replace = TRUE)
    attr(Y$booklet, "labels") <- seq_len(booklets)
    names(attr(Y$booklet, "labels")) <- paste0("Booklet ", seq_len(booklets))
  }

  # Simulate person abilities and testlet effects
  Sigma <- diag(c(control$var.theta, rep(control$var.testlet, control$testlets)))
  theta <- mvtnorm::rmvnorm(N, mean = rep(0, 1 + control$testlets), sigma = Sigma)

  # Add main effects for DIF variables
  theta[Y$sex == 0L, 1] <-
    theta[Y$sex == 0L, 1] - control$main.sex / 2
  theta[Y$sex == 1L, 1] <-
    theta[Y$sex == 1L, 1] + control$main.sex / 2
  theta[Y$mig == 0L, 1] <-
    theta[Y$mig == 0L, 1] + control$main.mig / 2
  theta[Y$mig == 1L, 1] <-
    theta[Y$mig == 1L, 1] - control$main.mig / 2
  maineff <- c(scale(cumsum(rep(control$main.school, 3)), scale = FALSE))
  for (i in 0:2) {
    theta[Y$school == i, 1] <- theta[Y$school == i, 1] + maineff[i + 1]
  }

  # Add main effect for rotation
  if (rotation) {
    theta[Y$rotation == 0L, 1] <-
      theta[Y$rotation == 0L, 1] + control$main.rotation / 2
    theta[Y$rotation == 1L, 1] <-
      theta[Y$rotation == 1L, 1] - control$main.rotation / 2
  }

  # Add main effects for booklets
  if (booklets > 1L) {
    maineff <- cumsum(rep(control$main.booklet, booklets))
    maineff <- c(scale(maineff, scale = FALSE))
    for (i in seq_len(booklets)) {
      theta[Y$booklet == i, 1] <-
        theta[Y$booklet == i, 1] + maineff[i]
    }
  }

  # Item difficulties for dichotomous items
  if (booklets > 1) {
    unique <- seq(-2, 2, len = sum(control$unique))
    common <- seq(-2, 2, len = I - sum(control$unique))
    beta <- c(common, unique)
  } else {
    beta <- seq(-2, 2, len = I)
  }
  beta <- cbind(beta)

  # Add category thresholds for polytomous items
  nthres <- rep(1, I)
  if (poly > 0L) {
    nthres[base::sample(seq_len(I), poly)] <- rep(c(2, 3), poly)[seq_len(poly)]
    beta <- matrix(beta, I, max(nthres))
    beta[, -1] <- NA
    for (i in seq_len(I)) {
      if (nthres[i] == 1) next
      beta[i, seq_len(nthres[i])] <- beta[i, 1] + seq(-.5, .5, len = nthres[i])
    }
  }

  # Simulate responses
  p0 <- array(NA, dim = c(N, dim(beta)))
  facet <- rep(1, I)
  if (control$testlets > 0L) {
    while(length(unique(facet)) != control$testlets | any(table(facet) < 3))
      facet <- sample(seq_len(control$testlets), I, replace = TRUE)
  }
  for (i in seq_len(dim(p0)[2])) {
    for (j in seq_len(dim(p0)[3])) {
      logit <- theta[, 1] - beta[i, j]
      if (control$testlets > 0L) logit <- logit + theta[, facet[i] + 1]
      p0[, i, j] <- stats::plogis(logit)
    }
    p0[, i, ] <- t(apply(p0[, i, , drop = FALSE], 1, cumsum))
  }
  resp0 <- p0 > array(stats::runif(N * I), dim = dim(p0))
  resp0 <- apply(resp0, 1:2, sum, na.rm = TRUE)

  # Combine respondent variables with item responses
  items <- paste0(
    control$varname,
    formatC(seq_len(I), width = 4, format = "d", flag = "0"),
    "_c"
  )
  colnames(resp0) <- items
  resp <- cbind(Y, as.data.frame(resp0))

  # Create variable definitions
  vars <- data.frame(
    item = items,
    raw = FALSE,
    dich = nthres == 1,
    type = ifelse(nthres == 1, "MC", "CMC"),
    dim = facet,
    scoring = ifelse(nthres == 1, 1, 0.5),
    num_cat = nthres
  )
  if (poly > 0L) {
    vars$poly <- nthres > 1
    vars$mixed <- TRUE
  }
  if (booklets > 1) {
    for (i in seq(1, booklets)) {
      f <- rep(FALSE, I)
      f[seq(1, I - sum(control$unique))] <- TRUE # common items
      start <- I - sum(control$unique) + sum(control$unique[seq_len(i - 1)]) + 1
      f[seq(start, start + control$unique[i] - 1)] <- TRUE # unique items
      vars[[paste0("booklet", i)]] <- f
      # Present the unique items before or after the common items
      if (i %% 2 == 0L) {
        f <- rep(NA, I)
        f[vars[[paste0("booklet", i)]]] <- seq(1, sum(vars[[paste0("booklet", i)]]))
        vars[[paste0("pos", i)]] <- f
      } else {
        f <- rep(NA, I)
        pos <-
          c(seq(control$unique[i] + 1, sum(vars[[paste0("booklet", i)]])),
            seq_len(control$unique[i]))
        f[vars[[paste0("booklet", i)]]] <- pos
        vars[[paste0("pos", i)]] <- f
      }
    }
  } else {
    vars$pos <- seq_len(I)
  }

  # Create unscored responses for MC items
  vars$correct <- NA
  for (i in vars$item[vars$type == "MC"]) {
    correct <- base::sample(1:4, 1)
    incorrect <- seq(1, 4)[1:4 != correct]
    resp[[gsub("_c", "", i)]] <- base::sample(incorrect, N, replace = TRUE)
    resp[[gsub("_c", "", i)]][resp[[i]] %in% 1] <- correct
    vars <- rbind(vars, vars[vars$item == i, ])
    vars$item[nrow(vars)] <- gsub("_c", "", i)
    vars$raw[vars$item == gsub("_c", "", i)] <- TRUE
    vars$dich[vars$item == gsub("_c", "", i)] <- FALSE
    if (poly > 0L) {
      vars$poly[vars$item == gsub("_c", "", i)] <- FALSE
      vars$mixed[vars$item == gsub("_c", "", i)] <- FALSE
    }
    vars$correct[vars$item == i] <-
      vars$correct[vars$item == gsub("_c", "", i)] <- correct
  }

  # Create invalid responses
  for (i in vars$item[!vars$raw]) {
    resp[[i]] <- ifelse(stats::runif(N) < control$invalid, -95, resp[[i]])
  }

  # Create omitted responses for all items
  for (i in vars$item[!vars$raw]) {
    f <- stats::runif(N) < control$omitted
    resp[[i]] <- ifelse(f, -97, resp[[i]])
    if (vars$dich[vars$item == i])
      resp[[gsub("_c", "", i)]] <- ifelse(f, -97, resp[[gsub("_c", "", i)]])
  }

  # Create missing responses because items were not reached
  if (booklets == 1L) {
    z <- stats::rbinom(N, size = 1, prob = 0.95)
    reached <- I + 1 - ifelse(z == 0, 0, stats::rgeom(N, 5 / I))
    reached[reached < 3] <- 3
    items <- vars$item[!vars$raw]
    items <- items[order(vars$pos[!vars$raw])]
    for (i in seq_len(N)) {
      if (reached[i] == I + 1) next
      resp[i, items[seq(reached[i], I)]] <- -94
    }
  } else {
    for (j in seq_len(booklets)) {
      k <- sum(vars[[paste0("booklet", j)]][!vars$raw])
      z <- stats::rbinom(N, size = 1, prob = 0.95)
      reached <- k + 1 - ifelse(z == 0, 0, stats::rgeom(N, 5 / k))
      reached[reached < 3] <- 3
      items <- vars$item[!vars$raw & vars[[paste0("booklet", j)]]]
      pos <- vars[[paste0("pos", j)]][!vars$raw & vars[[paste0("booklet",j)]]]
      items <- items[order(pos)]
      for (i in seq_len(N)[Y$booklet == j]) {
        if (reached[i] == k + 1) next
        resp[i, items[seq(reached[i], k)]] <- -94
      }
    }
  }

  # Create missing responses for items that were not administered
  if (booklets > 1L) {
    for (i in seq_len(booklets)) {
      items <- vars$item[!vars[[paste0("booklet", i)]]]
      resp[resp$booklet == i, items] <- -54
    }
  }

  # Set missing for respondents that have not participated
  resp$valid <- TRUE
  resp$valid[sample(seq_len(N), control$not.participated)] <- FALSE
  resp[!resp$valid, grepl(paste0("^", control$varname), colnames(resp))] <- -56

  # Copy missing values to raw scores
  for (i in vars$item[vars$type == "MC" & !vars$raw]) {
    resp[[gsub("_c", "", i)]][resp[[i]] < 0] <- resp[[i]][resp[[i]] < 0]
  }

  # Missing values for DIF variable mig
  resp$mig[resp$valid] <-
    ifelse(stats::runif(sum(resp$valid)) < control$mis.mig,
           NA, resp$mig[resp$valid])

  # Booklet indicator
  if (booklets > 1L) {
    for (i in seq_len(booklets))
      resp[[paste0("booklet", i)]] <- resp$booklet == i
  }

  return(list(resp = resp, vars = vars))

}


#'
#' Example 1
#'
#' The example is loosely based on the scaling of grammar in A12. The
#' simulated test includes 20 dichotomous items without a booklet design.
#'

# Simulate data
ex1 <- sim.testlet(
  N = 1000,
  I = 15,
  poly = 0,
  booklets = 1,
  seed = 9780,
  rotation = FALSE,
  control = list(varname = "grk1", testlets = 0)
)
ex1$vars$dim <- NULL
ex1$vars
head(ex1$resp)

# Save data
save(ex1, file = "data/ex1.rda")


#'
#' Example 2
#'
#' The example is loosely based on the scaling of grammar in A14. The
#' simulated test includes 13 dichotomous and 4 polytomous items without a
#' booklet design.
#'

# Simulate data
ex2 <- sim.testlet(
  N = 1500,
  I = 17,
  poly = 4,
  booklets = 1,
  seed = 1923,
  rotation = TRUE,
  control = list(varname = "mag12", not.participated = 50, testlets = 4)
)
ex2$vars$content <-
  factor(ex2$vars$dim, 1:4, c("units", "change", "space", "data"))
ex2$vars$dim <- NULL
ex2$vars
head(ex2$resp)

# Save data
save(ex2, file = "data/ex2.rda")


#'
#' Example 3
#'
#' The example is loosely based on the scaling of reading in B129 The
#' simulated test includes 13 dichotomous and 4 polytomous items without a
#' booklet design.
#'

# Simulate data
ex3 <- sim.testlet(
  N = 2100,
  I = 21,
  poly = 5,
  booklets = 3,
  seed = 8631,
  rotation = TRUE,
  control = list(varname = "reg7", main.school = 0, unique = c(5, 6, 5))
)
ex3$resp$school <- NULL
ex3$vars$texttype <- as.character(factor(
  ex3$vars$dim, 1:5,
  c("information", "instruction", "advertising", "commenting", "literary")
))
ex3$vars$dim <- NULL
ex3$vars
head(ex3$resp)

# Save data
save(ex3, file = "data/ex3.rda")



#'
#' # Test simulated data
#'
#' ## Example 1
#'

library(TAM)
load("data/ex1.rda")
resp <- ex1$resp[ex1$resp$valid, ]
resp[resp < 0] <- NA
items <- ex1$vars$item[ex1$vars$dich]

# Rasch model
mod1.1 <- TAM::tam.mml(resp = resp[, items], verbose = FALSE)
summary(mod1.1)

# Rasch model with main effect for sex
mod1.2 <- TAM::tam.mml(resp = resp[, items], group = resp$sex, verbose = FALSE)
mod1.2$beta

# Rasch model with main effect for mig
mod1.3 <- TAM::tam.mml(resp = resp[!is.na(resp$mig), items],
                       group = resp$mig[!is.na(resp$mig)],
                       verbose = FALSE)
mod1.3$beta


#'
#' ## Example 2
#'

library(TAM)
load("data/ex2.rda")
resp <- ex2$resp[ex2$resp$valid, ]
resp[resp < 0] <- NA
items <- ex2$vars$item[ex2$vars$mixed]

# Rasch model
mod2.1 <- TAM::tam.mml(resp = resp[, items], verbose = FALSE)
summary(mod2.1)

# Rasch model with main effect for sex
mod1.2 <- TAM::tam.mml(resp = resp[, items], group = resp$sex, verbose = FALSE)
mod1.2$beta

# Rasch model with main effect for mig
mod2.3 <- TAM::tam.mml(resp = resp[!is.na(resp$mig), items],
                       group = resp$mig[!is.na(resp$mig)],
                       verbose = FALSE)
mod2.3$beta

# Rasch model with main effects for school
mod2.4 <- TAM::tam.mml(resp = resp[, items], group = resp$school,
                       verbose = FALSE)
mod2.4$beta

# Multidimensional (testlet) model
Q <- matrix(0, length(items), 5)
Q[, 1] <- 1
Q[ex2$vars$content[ex2$vars$item %in% items] == "units", 2] <- 1
Q[ex2$vars$content[ex2$vars$item %in% items] == "change", 3] <- 1
Q[ex2$vars$content[ex2$vars$item %in% items] == "space", 4] <- 1
Q[ex2$vars$content[ex2$vars$item %in% items] == "data", 5] <- 1
var.fixed <- c()
for (i in 1:4) {
  for (j in seq(i + 1, 5))
    var.fixed <- rbind(var.fixed, c(i, j, 0))
}
mod2.5 <- TAM::tam.mml(resp = resp[, items], Q = Q, control = list(snodes = 5000),
                       variance.fixed = var.fixed, verbose = T)
summary(mod2.5)


#'
#' ## Example 3
#'

library(TAM)
load("data/ex3.rda")
resp <- ex3$resp[ex3$resp$valid, ]
resp[resp < 0] <- NA
items <- ex3$vars$item[ex3$vars$mixed]

# Rasch model
mod3.1 <- TAM::tam.mml(resp = resp[, items], verbose = FALSE)
summary(mod3.1)

# Rasch model with main effect for sex
mod3.2 <- TAM::tam.mml(resp = resp[, items], group = resp$sex, verbose = FALSE)
mod3.2$beta

# Rasch model with main effect for mig
mod3.3 <- TAM::tam.mml(resp = resp[!is.na(resp$mig), items],
                       group = resp$mig[!is.na(resp$mig)],
                       verbose = FALSE)
mod3.3$beta

# Rasch model with main effect for booklet
mod3.4 <- TAM::tam.mml(resp = resp[, items], group = resp$booklet, verbose = FALSE)
mod3.4$beta

# Rasch model for Booklet 1
mod3.5 <- TAM::tam.mml(
  resp = resp[resp$booklet1, ex3$vars$item[ex3$vars$mixed & ex3$vars$booklet1]],
  verbose = FALSE
)
summary(mod3.5)
