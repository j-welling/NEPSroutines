#'
#' Creates simulated data sets resembling the NEPS data structure
#'


#' Simulate a Rasch testlet model following Wang and Wilson (2005)
#'
#' @description
#' `sim.testlet()` simulates responses according to a Rasch testlet model
#' that loosely resemble the data structures in the NEPS. This includes tests
#' with dichotomous and/or polytomous items, tests with multiple booklets that
#' contain unique items, multi-stage tests, and different types of missing
#' values.
#'
#' Polytomous items will include a mixed number of response categories. For
#' polytomous items also the respective subitems are simulated. Dichotomous
#' items include simple multiple-choice (MC) and short constructed response
#' (SCR) formats. The unscored MC will be generated with three to six
#' response options. The correct response option is randomly selected for each
#' item and represents either a single number or a single letter. The unscored
#' SCR items will include between 2 and 4 correct responses, either as a number
#' or a letter.
#'
#' Each simulation includes three DIF variables. For `sex` (0 = male,
#' 1 = female) and `school` (0 = lower, 1 = intermediate, 2 = upper) there will
#' be no missing values, while for `mig` (0 = no, 1 = yes) missing values can
#' be introduced. In addition to main effects, also DIF for selected items
#' can be simulated. DIF is only simulated for dichotomous items.
#'
#' If multiple booklets are requested, each booklet will contain several unique
#' items, while the remaining items will be common across booklets. The
#' respondents will be randomly assigned to one of the booklets.
#'
#' If a multi-stage test is requested, each block will contain several unique
#' items, while the remaining items will be common across all levels within a
#' stage. The respondents will be assigned to the different levels of the
#' multi-stage test based on their true proficiency to create roughly equal
#' sample sizes for each level.
#'
#' The model can include two or more testlets to introduce some
#' multidimensionality.
#'
#' The simulations follow the procedure outlined in Wang and Wilson (2005).
#'
#' Wang, W. C., & Wilson, M. (2005). The Rasch testlet model. Applied
#' Psychological Measurement, 29(2), 126-149.
#' https://doi.org/10.1177/0146621604271053
#'
#' @param N              The total number of respondents.
#' @param I              The total number of items.
#'                       Either `I` or `MC` must be specified.
#' @param CMC            The number of polytomous CMC items, while `I-CMC`
#'                       items will be dichotomous; each CMC item will
#'                       include between 3 and 6 subitems.
#' @param SCR            The number of dichotomous SCR items.
#' @param MC             The number of dichotomous MC items.
#'                       Either `I` or `MC` must be specified.
#' @param booklets       The number of test booklets. Each booklet will have
#'                       `control$unique` unique items;
#'                       `I-control$unique*booklets` items will be common across
#'                       all booklets.
#' @param mst            A vector defining a multi-stage test. Each element
#'                       represents a stage, with each value indicating the
#'                       number of levels within the stage. If `mst != 1`,
#'                       the argument `booklets` will be ignored.
#'                       Each stage contains `control$unique` unique items;
#'                       the remaining items will be distributed as common
#'                       items across the stages.
#' @param rotation       A logical to include a test rotation with two positions
#'                       (first and second).
#' @param unscored       A logical indicating to return the unscored variables
#' @param scored         A logical indicating to return the scored variables
#' @param seed           A random seed for reproducible results.
#' @param control        A list of control settings:
#' * var.theta:          Variance of the population abilities.
#' * var.testlet:        Variance of each population testlet effect.
#' * var.cmc:            Variance of each population CMC effect;
#'                       for each CMC item a separate testlet is specified to
#'                       introduce some dependencies between subitems.
#' * testlets:           The number of testlets introducing some
#'                       multidimensionality.
#' * unique:             The number of unique items in each booklet. For a
#'                       multi-stage test, the value refers to the number of
#'                       unique items in each level of a given stage.
#'                       The length of the vector must match the number given in
#'                       `booklets` or the length of `mst`. Otherwise, the same
#'                       number of unique items is used for all booklets or
#'                       levels.
#' * mc.resp.letter:     Use a mixture of numbers and letters as unscored
#'                       responses (TRUE) or only numbers (FALSE)
#' * mc.resp.options:    A numeric vector for the number of response options
#'                       in MC items; for a given item, the number is randomly
#'                       selected from this vector.
#' * varname:            Stem for the variable names. After the stem the item
#'                       number is added with four digits. Scored items are
#'                       additionally appended with "_c". CMC items are
#'                       indicated by "s".
#' * varname.testlets    A name for the testlet variable.
#' * lbl.booklets        A vector of descriptive labels for the different
#'                       booklets.
#' * lbl.testlets        A vector of descriptive labels for the different
#'                       testlets.
#' * mis.mig:            Percentage of missing values in the DIF variable `mig`.
#' * mis.invalid:        Percentage of invalid responses.
#' * mis.omitted:        Percentage of omitted responses.
#' * mis.notreached:     Percentage of respondents with not reached items.
#' * mis.notdeterminable Percentage of not determinable responses in CMC items.
#' * mis.notparticipated Number of additional respondents that did not
#'                       participate and, thus, have missing code -56 on all
#'                       variables. These are added to the total sample size
#'                       `N`.
#' * main.sex:           Main effect for the DIF variable `sex`.
#' * main.mig:           Main effect for the DIF variable `mig`.
#' * main.school:        Main effect for the DIF variable `school`.
#' * main.rotation:      Main effect for the test rotations.
#' * main.booklet:       Main effect for the test booklets.
#' * dif.sex:            Difference in item difficulties for the DIF variable
#'                       `sex`.
#' * dif.mig:            Difference in item difficulties for the DIF variable
#'                       `mig`.
#' * dif.school:         Difference in item difficulties for lower school types
#'                       in the DIF variable `school`.
#' * items.sex:          A vector with item numbers for items with DIF on the
#'                       variable `sex`.
#' * items.mig:          A vector with item numbers for items with DIF on the
#'                       variable `mig`.
#' * items.school:       A vector with item numbers for items with DIF on the
#'                       variable `school`.
#' * age:                The mean age of the sample used to create the birth date.
#' * testyear:           The assessment year.
#' @returns              A list with two elements:
#' * resp:               A data frame with the simulated item responses and
#'                       respondent characteristics.
#' * vars:               A data frame with item characteristics.
sim.testlet <- function(N = 2000, I = 25, CMC = 5, SCR = 5, MC = NULL,
                        booklets = 1, mst = 1, rotation = TRUE,
                        unscored = TRUE, scored = TRUE,
                        seed = 6237, control = list()) {

  set.seed(seed)

  ###################################################################
  # Validate arguments
  ###################################################################

  # Normalize arguments
  if (!is.list(control)) control <- list()
  cntrl <-
    list(var.theta = 1, var.testlet = 0.5, var.cmc = 0.2,
         testlets = 5, unique = 5,
         mc.resp.letter = TRUE, mc.resp.options = 3:6,
         varname = "mag5", varname.testlets = "dimension",
         lbl.booklets = NULL, lbl.testlets = NULL,
         mis.mig = 0.2, mis.invalid = 0.01, mis.omitted = 0.03,
         mis.notreached = .05, mis.notdeterminable = .05,
         mis.notparticipated = 10,
         main.sex = 0.40, main.mig = 0.80, main.school = 0.50,
         main.booklet = 0.50, main.rotation = 0.20,
         dif.sex = 0.80, dif.mig = 0.80, dif.school = 0.80,
         items.sex = c(2, 3), items.mig = c(4, 5), items.school = c(6, 7),
         age = 16, testyear = 2024)
  control <- utils::modifyList(cntrl, control)
  N <- as.integer(N[1])
  CMC <- as.integer(CMC[1])
  SCR <- as.integer(SCR[1])
  MC <- ifelse(is.null(MC), as.integer(I[1]) - CMC - SCR, as.integer(MC[1]))
  I <- MC + CMC + SCR
  booklets <- as.integer(booklets[1])
  mst <- as.integer(mst)
  rotation <- as.logical(rotation[1])
  MST <- FALSE
  if (length(mst) > 1L & all(mst > 0)) {
    booklets <- 1L
    MST <- TRUE
  }
  unscored <- as.logical(unscored[1])
  scored <- as.logical(scored[1])
  if (!MST & booklets != length(control$unique))
    control$unique <- rep(control$unique[1], booklets)
  if (MST & length(mst) != length(control$unique))
    control$unique <- rep(control$unique[1], length(mst))
  if (!MST & booklets != length(control$lbl.booklets))
    control$lbl.booklets <- paste0("Booklet ", seq_len(booklets))
  if (control$testlets != length(control$lbl.testlets))
    control$lbl.testlets <- paste0("Dimension ", seq_len(control$testlets))

  # Check arguments
  if (CMC > I)
    stop(paste0("The number of polytomous items 'CMC' must not be larger ",
                "than the total number of items 'I'!"))
  if (SCR > I)
    stop(paste0("The number of short constructed response items 'SCR' must not ",
                "be larger than the total number of items 'I'!"))
  if (SCR > I - CMC)
    stop(paste0("More short constructed response items 'SCR' requested ",
                "than available number of items 'I - CMC'!"))
  k <- length(unique(c(control$items.sex, control$items.mig, control$items.school)))
  if (k > I - CMC)
    stop(paste0("Not enough MC or SCR items to create the DIF effects!"))
  if (MST & sum(control$unique * mst) > I - sum(mst > 1))
    stop(paste0("Not enough items for the multi-stage design! Increase 'I' or ",
                "reduce the number of unique items."))
  if (I < 15)
    stop(paste0("The minimum number items that can be used is 15!"))
  if (!unscored & ! scored)
    stop(paste0("No item responses requested! Set either 'unscored' or ",
                "'scored' to 'TRUE'."))

  # Update sample size
  N <- N + control$mis.notparticipated

  ###################################################################
  # Create respondent variables
  ###################################################################

  # DIF variables
  Y <- data.frame(
    ID_t = seq_len(N),
    sex = base::sample(c(0, 1), N, replace = TRUE),
    mig = stats::rbinom(N, 1, 0.20),
    school = base::sample(c(0, 1, 2), N, replace = TRUE),
    t34005a = sample(1:6, N, replace = TRUE) # number of books
  )
  Y$books <- ifelse(Y$t34005a <= 3, 0, 1)

  # Create raw migration variable
  Y$t400500 <- NA
  Y$t400500[Y$mig %in% 0L] <-
    base::sample(c(0, 4:10), sum(Y$mig %in% 0L), replace = TRUE)
  Y$t400500[Y$mig %in% 1L] <-
    base::sample(1:3, sum(Y$mig %in% 1L), replace = TRUE)

  # Rotation indicator
  if (rotation) {
    Y$rotation <- base::sample(c(0, 1), N, replace = TRUE)
  }

  # Birth and test date
  Y$testy <- rep(control$testyear, N)
  Y$testm <- base::sample(1:12, N, replace = TRUE)
  Y$birthy <-
    base::sample(control$testyear - control$age + c(-1, 0, 1), N,
                 replace = TRUE)
  Y$birthm <- base::sample(1:12, N, replace = TRUE)
  Y$age <- NEPSroutines::calculate_age(Y)

  # Booklet indicators
  if (booklets > 1L) {
    Y$booklet <- factor(
      base::sample(seq_len(booklets), N, replace = TRUE),
      seq_len(booklets),
      control$lbl.booklets[seq_len(booklets)]
    )
    for (i in unique(Y$booklet)) Y[[tolower(sub(" ", "", i))]] <- Y$booklet == i
  }

  ###################################################################
  # Create variable definitions
  ###################################################################

  # Item type
  type <- sample(c(rep("MC", MC), rep("CMC", CMC), rep("SCR", SCR)))
  while (all(type[c(control$items.mig, control$items.school, control$items.sex)] == "CMC"))
    type <- sample(c(rep("MC", MC), rep("CMC", CMC), rep("SCR", SCR)))

  # Number of response categories
  ncat <- rep(1, I)
  ncat[type == "CMC"] <- base::sample(3:6, CMC, replace = TRUE)

  # Names for scored items
  items <- paste0(
    control$varname,
    formatC(seq_len(I), width = 3, format = "d", flag = "0"),
    ifelse(type == "CMC", "s", 0),
    "_c"
  )

  # Variable definition
  vars <- data.frame(
    item = items,
    raw = FALSE,
    dich = (type != "CMC"),
    poly = (type == "CMC"),
    mixed = TRUE,
    type = type,
    scoring = ifelse(type == "CMC", 0.5, 1),
    num_cat = ncat
  )

  # Add testlets for items
  if (control$testlets > 0L) {
    testlet <- rep(1, I)
    while(length(unique(testlet)) != control$testlets | any(table(testlet) < 3))
      testlet <- base::sample(
        control$lbl.testlets[seq_len(control$testlets)],
        I,
        replace = TRUE
      )
    vars[[control$varname.testlets]] <- testlet
  }

  # Add booklet and position indicators
  if (booklets > 1L) {
    for (i in seq(1, booklets)) {
      f <- rep(FALSE, I)
      f[seq(1, I - sum(control$unique))] <- TRUE # common items
      start <- I - sum(control$unique) + sum(control$unique[seq_len(i - 1)]) + 1
      f[seq(start, start + control$unique[i] - 1)] <- TRUE # unique items
      lbl <- tolower(gsub(" ", "", control$lbl.booklets[i]))
      vars[[lbl]] <- f
      # Present the unique items before or after the common items
      if (i %% 2 == 0L) {
        f <- rep(NA, I)
        f[vars[[lbl]]] <- seq(1, sum(vars[[lbl]]))
        vars[[paste0("pos_", lbl)]] <- f
      } else {
        f <- rep(NA, I)
        pos <-
          c(seq(control$unique[i] + 1, sum(vars[[lbl]])),
            seq_len(control$unique[i]))
        f[vars[[lbl]]] <- pos
        vars[[paste0("pos_", lbl)]] <- f
      }
    }
  } else if (!MST & booklets == 1L) {
    vars$pos <- seq_len(I)
  }

  # Add subitems for CMCs
  for (i in vars$item[vars$poly]) {
    tab <- c()
    for (r in seq_len(vars$num_cat[vars$item == i]))
      tab <- rbind(tab, vars[vars$item == i, ])
    tab$dich <- TRUE
    tab$poly <- tab$mixed <- FALSE
    tab$type <- "MC"
    tab$scoring <- 1
    tab$num_cat <- 1
    tab$item <- mapply(\(x, i) sub("s(?=_c)", i, x, perl = TRUE), tab$item, seq_len(nrow(tab)))
    vars <- rbind(vars, tab)
  }


  ###################################################################
  # Simulate responses
  ###################################################################

  # Simulate person abilities, testlet effects, and CMC effect
  Sigma <- diag(c(
    control$var.theta,
    rep(control$var.testlet, control$testlets),
    rep(control$var.cmc, CMC)
  ))
  theta <- mvtnorm::rmvnorm(
    N,
    mean = rep(0, 1 + control$testlets + CMC),
    sigma = Sigma
  )
  colnames(theta) <- c(
    "Theta",
    control$lbl.testlet,
    sub("s_c", "", vars$item[vars$poly])
  )

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
  for (i in 1:3) {
    theta[Y$school == i, 1] <- theta[Y$school == i, 1] + maineff[i]
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
    names(maineff) <- levels(Y$booklet)
    for (i in levels(Y$booklet)) {
      theta[Y$booklet == i, 1] <-
        theta[Y$booklet == i, 1] + maineff[i]
    }
  }

  # Item difficulties for MC, SCR, and subitems of CMC
  beta <- runif(sum(vars$dich), -3, 3)
  names(beta) <- vars$item[vars$dich]

  # Simulate responses
  items <- vars$item[vars$mixed]
  p0 <- c()
  for (i in names(beta)) {
    logit <- theta[, 1] - beta[i]
    # Add testlet effect
    if (control$testlets > 0L)
      logit <- logit + theta[, vars[[control$varname.testlet]][vars$item == i]]
    # Add CMC effect
    if (sub("[1-9]_c", "", i) %in% colnames(theta))
      logit <- logit + theta[, sub("[1-9]_c", "", i)]
    # Add DIF
    if(i %in% items[control$items.sex])
      logit <- logit + control$dif.sex * as.integer(Y$sex == 1L)
    if(i %in% items[control$items.mig])
      logit <- logit - control$dif.mig * as.integer(Y$mig == 0L)
    if(i %in% items[control$items.school])
      logit <- logit + control$dif.school * as.integer(Y$school == 0L)
    p0 <- cbind(p0, stats::plogis(logit))
  }
  resp0 <- (p0 > matrix(stats::runif(N * length(beta)), N, length(beta))) * 1

  # Create CMC responses
  for (i in vars$item[vars$poly]) {
    score <- rowSums(resp0[, sub("[1-9]_c", "", names(beta)) %in% sub("s_c", "", i)])
    resp0 <- cbind(resp0, score)
  }

  # Combine respondent variables with item responses
  colnames(resp0) <- c(names(beta), vars$item[vars$poly])
  resp <- cbind(Y, as.data.frame(resp0))

  ###################################################################
  # Create unscored responses
  ###################################################################

  vars$correct <- NA
  for (i in vars$item[vars$raw == FALSE & vars$type %in% c("MC", "SCR")]) {
    if (vars$type[vars$item == i] == "MC") {
      k <- control$mc.resp.options
      if (length(k) > 1)
        k <- base::sample(k, 1)
      correct <- base::sample(seq(1, k), 1)
      incorrect <- seq(1, k)[-correct]
    } else {
      correct <- unique(round(runif(round(runif(1, 1.6, 4.4)), 1, 26)))
      incorrect <- seq(1, 26)[-correct]
    }
    if (control$mc.resp.letter & stats::rbinom(1, 1, .5) == 0L) {
      correct <- letters[correct]
      incorrect <- letters[incorrect]
    }
    resp[[gsub("_c", "", i)]] <-
      base::sample(incorrect, N, replace = TRUE)
    if (vars$type[vars$item == i] == "MC") {
      resp[[gsub("_c", "", i)]][resp[[i]] %in% 1] <- correct
    } else {
      resp[[gsub("_c", "", i)]][resp[[i]] %in% 1] <-
        base::sample(correct, sum(resp[[i]] %in% 1), replace = TRUE)
    }
    vars <- rbind(vars, vars[vars$item == i, ])
    vars$item[nrow(vars)] <- gsub("_c", "", i)
    vars$raw[vars$item == gsub("_c", "", i)] <- TRUE
    vars$dich[vars$item == gsub("_c", "", i)] <- FALSE
    vars$poly[vars$item == gsub("_c", "", i)] <- FALSE
    vars$mixed[vars$item == gsub("_c", "", i)] <- FALSE
    vars$correct[vars$item == gsub("_c", "", i)] <- paste0(correct, collapse = ";")
  }

  ###################################################################
  # Set MST design
  ###################################################################

  if (MST) {

    # Available items
    item_pool <- data.frame(item = names(beta), beta = beta)
    for (i in vars$item[vars$poly]) {
      it <- vars$item[sub("[1-9]_c", "", vars$item) == sub("s_c", "", i)]
      item_pool <- rbind(item_pool, data.frame(item = i, beta = mean(beta[it])))
      item_pool <- item_pool[!(item_pool$item %in% it), ]
    }

    # Number of common items in each stage
    common <- I - sum(control$unique * mst)
    control$common <- rep(0, length(mst))
    i <- 0
    while (common > 0) {
      i <- ifelse(i == length(control$common), 1, i + 1)
      if (mst[i] == 1) next
      control$common[i] <- control$common[i] + 1
      common <- common - 1
    }

    # Stage and level indicators
    vars[, paste0("stage", seq_along(mst))] <- FALSE
    vars[, paste0("level", seq_len(max(mst)))] <- FALSE

    # Select items for each stage
    for (stage in seq_along(mst)) {

      # Select common items
      item_pool$common <- FALSE
      item_pool$common[sample(nrow(item_pool), control$common[stage])] <- TRUE
      selected_items <- item_pool$item[item_pool$common]
      vars[vars$item %in% selected_items, paste0("stage", stage)] <- TRUE
      vars[vars$item %in% selected_items, paste0("level", seq_len(mst[stage]))] <- TRUE
      item_pool <- item_pool[!item_pool$common, ]

      # Create item groups for levels depending on item difficulties
      item_pool$group <- NA
      if (mst[stage] == 1) {
        item_pool$group <- rep(1, nrow(item_pool))
      } else {
        item_pool$group <- cut(rank(item_pool$beta), mst[stage], labels = FALSE)
      }

      # Select unique items for each level
      for (level in seq_len(mst[stage])) {

        # Select items
        selected_items <- base::sample(
          item_pool$item[item_pool$group %in% level],
          control$unique[stage]
        )
        vars[vars$item %in% selected_items, paste0("stage", stage)] <- TRUE
        vars[vars$item %in% selected_items, paste0("level", level)] <- TRUE

        # Remove selected items from pool
        item_pool <- item_pool[!(item_pool$item %in% selected_items), ]

      }

    }

    # Create position indicator
    vars$pos <- 0
    for (stage in seq_along(mst)) {
      start <- max(vars$pos)
      items <- vars[vars$mixed & vars[[paste0("stage", stage)]],]
      items$common <-
        rowSums(items[, c(paste0("stage", stage), paste0("level", seq_len(mst[stage])))])
      items$common <- items$common == mst[stage] + 1
      vars$pos[vars$item %in% items$item[items$common]] <- start + seq_len(sum(items$common))
      start <- start + sum(items$common)
      for (level in seq_len(mst[stage])) {
        it <- items$item[!items$common & items[[paste0("level", level)]]]
        vars$pos[vars$item %in% it] <- start + seq_along(it)
      }

    }
    # Set stages and levels for subitems of CMC
    if (CMC > 0L) {
      for (i in vars$item[vars$poly]) {
        it <- vars$item[sub("[1-9]_c", "", vars$item) == sub("s_c", "", i)]
        for (stage in seq_len(length(mst)))
          vars[vars$item %in% it, paste0("stage", stage)] <-
            vars[vars$item %in% i, paste0("stage", stage)]
        for (level in seq_len(max(mst)))
          vars[vars$item %in% it, paste0("level", level)] <-
            vars[vars$item %in% i, paste0("level", level)]
        vars$pos[vars$item %in% it] <- vars$pos[vars$item %in% i]
      }
    }

    # Set stages and levels for unscored items
    if (CMC > 0L) {
      for (i in vars$item[!vars$raw]) {
        it <- sub("_c", "", i)
        for (stage in seq_len(length(mst)))
          vars[vars$item %in% it, paste0("stage", stage)] <-
            vars[vars$item %in% i, paste0("stage", stage)]
        for (level in seq_len(max(mst)))
          vars[vars$item %in% it, paste0("level", level)] <-
            vars[vars$item %in% i, paste0("level", level)]
        vars$pos[vars$item %in% it] <- vars$pos[vars$item %in% i]
      }
    }

    # Assign respondents to levels based on true proficiency
    for (stage in seq_along(mst)) {
      if (mst[stage] == 1) {
        levels <- 1
      } else {
        levels <- cut(rank(theta[, 1]), mst[stage], labels = FALSE)
      }
      resp[[paste0("stage", stage)]] <- levels
    }

  }

  ###################################################################
  # Set missing values
  ###################################################################

  # Create invalid responses
  for (i in vars$item[vars$mixed]) {
    resp[[i]] <- ifelse(stats::runif(N) < control$mis.invalid, -95, resp[[i]])
  }

  # Create omitted responses for all items
  for (i in vars$item[vars$mixed]) {
    f <- stats::runif(N) < control$mis.omitted
    resp[[i]] <- ifelse(f, -97, resp[[i]])
  }

  # Create missing responses because items were not reached
  if (!MST & booklets == 1L) {
    z <- stats::rbinom(N, size = 1, prob = 1 - control$mis.notreached)
    reached <- I + 1 - ifelse(z == 0, 0, stats::rgeom(N, 5 / I))
    reached[reached < 3] <- 3
    items <- vars$item[vars$mixed]
    items <- items[order(vars$pos[vars$mixed])]
    for (i in seq_len(N)) {
      if (reached[i] == I + 1) next
      resp[i, items[seq(reached[i], I)]] <- -94
    }
  } else if (!MST & booklets > 1L){
    for (j in control$lbl.booklets) {
      lbl <- tolower(gsub(" ", "", j))
      k <- sum(vars[[lbl]][vars$mixed])
      z <- stats::rbinom(N, size = 1, prob = 1 - control$mis.notreached)
      reached <- k + 1 - ifelse(z == 0, 0, stats::rgeom(N, 5 / k))
      reached[reached < 3] <- 3
      items <- vars$item[vars$mixed & vars[[lbl]]]
      pos <- vars[[paste0("pos_", lbl)]][vars$mixed & vars[[lbl]]]
      items <- items[order(pos)]
      for (i in seq_len(N)[Y$booklet == j]) {
        if (reached[i] == k + 1) next
        resp[i, items[seq(reached[i], k)]] <- -94
      }
    }
  } else if (MST) {
    for (i in seq_len(N)) {
      items_administered <- c()
      for (stage in seq_along(mst)) {
        level <- resp[i, paste0("stage", stage)]
        items_administered <- c(
          items_administered,
          vars$item[vars$mixed &
                      vars[[paste0("stage", stage)]] &
                      vars[[paste0("level", level)]]]
        )
      }
      items_administered <- vars[vars$item %in% items_administered, ]
      items_administered <- items_administered[order(items_administered$pos),]
      k <- nrow(items_administered)
      z <- stats::rbinom(1, size = 1, prob = 1 - control$mis.notreached)
      reached <- k + 1 - ifelse(z == 0, 0, stats::rgeom(1, 5 / k))
      reached[reached < 3] <- 3
      if (reached == k + 1) next
      resp[i, items_administered$item[seq(reached, k)]] <- -94
    }

  }

  # Set undeterminable missing for first CMC item
  if (CMC > 0L) {
    f <- stats::runif(N) < control$mis.notdeterminable
    item <- vars$item[vars$poly][1]
    resp[[item]] <- ifelse(f, -55, resp[[item]])
  }

  # Create missing responses for items that were not administered
  if (booklets > 1L) {
    for (i in control$lbl.booklets) {
      lbl <- tolower(gsub(" ", "", i))
      items <- vars$item[vars$mixed & !vars[[lbl]]]
      resp[resp$booklet == i, items] <- -54
    }
  } else if (MST) {
    for(i in seq_len(N)) {
      items_administered <- c()
      for (stage in seq_along(mst)) {
        level <- resp[i, paste0("stage", stage)]
        items_administered <- c(
          items_administered,
          vars$item[vars$mixed &
                    vars[[paste0("stage", stage)]] &
                    vars[[paste0("level", level)]]]
        )
      }
      items_excluded <- setdiff(vars$item[vars$mixed], items_administered)
      for (j in vars$item[vars$poly & vars$item %in% items_excluded]) {
        items_excluded <- c(
          items_excluded,
          vars$item[sub("[1-9]_c", "", vars$item) == sub("s_c", "", j)]
        )
      }
      resp[i, items_excluded] <- -54
    }
  }

  # Set missing for respondents that have not participated
  resp$valid <- TRUE
  resp$valid[sample(seq_len(N), control$mis.notparticipated)] <- FALSE
  resp[!resp$valid, vars$item[!vars$raw]] <- -56

  # Set missingness for subitems of CMCs
  if (CMC > 0L) {
    for (i in vars$item[vars$poly]) {
      items <- vars$item[vars$dich & !vars$mixed]
      items <- items[sub("[1-9]_c", "", items) %in% sub("s_c", "", i)]
      for (r in seq_len(N)) {
        k <- base::sample(seq(ceiling(length(items) / 2) + 1, length(items)), 1)
        it <- base::sample(items, k)
        if (resp[r, i] %in% c(-54, -56, -94)) {  # not participated / not administered / not reached
          resp[r, items] <- resp[r, i]
        } else if (resp[r, i] == -55) {     # not determinable
          resp[r, it] <- rep(c(-97, -95), length(it))[seq_along(it)]
        } else if (resp[r, i] < 0) {        # omitted / invalid
          resp[r, it] <- resp[r, i]
        }
      }
    }
  }

  # Set missingness for one subitem of last CMC to be imputed during scaling
  if (CMC > 0L) {
    i <- rev(vars$item[vars$poly])[1]
    items <- vars$item[vars$dich & !vars$mixed]
    items <- items[sub("[1-9]_c", "", items) %in% sub("s_c", "", i)]
    f <- stats::runif(N) < control$mis.omitted & resp[[i]] >= 0
    resp[f, sample(items, 1)] <- -97
  }

  # Copy missing values to raw scores
  for (i in vars$item[vars$dich & !vars$raw]) {
    resp[[gsub("_c", "", i)]][resp[[i]] < 0] <- resp[[i]][resp[[i]] < 0]
  }

  # Missing values for DIF variable mig
  resp$mig[resp$valid & stats::runif(N) < control$mis.mig] <- NA
  resp$t400500[is.na(resp$mig)] <- NA

  # Select variables to return
  if (CMC == 0L) vars$poly <- vars$mixed <- NULL
  if (!unscored) {
    resp$t34005a <- resp$t400500 <- resp$testy <- resp$testm <-
      resp$birthy <- resp$birthm <- NULL
    resp[, grepl(paste0("^", control$varname, "[0-9s]{4}$"), colnames(resp))] <- NULL
    vars <- vars[!grepl(paste0("^", control$varname, "[0-9s]{4}$"), vars$item), ]
  }
  if (!scored) {
    resp$mig <- resp$books <- resp$age <- NULL
    resp[, grepl(paste0("^", control$varname, "[0-9s]{4}_c$"), colnames(resp))] <- NULL
    vars <- vars[!grepl(paste0("^", control$varname, "[0-9s]{4}_c$"), vars$item), ]
  }

  return(list(resp = resp, vars = vars))

}


#'
#' Example 1
#'
#' The example is loosely based on the scaling of grammar in A12. The
#' simulated test includes 15 MC items without a booklet design.
#'

ex1 <- sim.testlet(
  N = 1000,
  I = 15,
  SCR = 0,
  CMC = 0,
  seed = 9780,
  rotation = FALSE,
  control = list(
    varname = "grk1",
    testlets = 0,
    mc.resp.letter = FALSE,
    mc.resp.options = 4,
    age = 4,
    testyear = 2011
  )
)
ex1$vars
head(ex1$resp)

save(ex1, file = "data/ex1.rda")


#'
#' Example 2
#'
#' The example is loosely based on the scaling of grammar in A14. The
#' simulated test includes 9 MC, 4 SCR, and 4 CMC items without a
#' booklet design.
#'

# Simulate data
ex2 <- sim.testlet(
  N = 1500,
  MC = 9,
  SCR = 4,
  CMC = 4,
  seed = 1923,
  rotation = TRUE,
  control = list(
    varname = "mag12",
    mis.notparticipated = 50,
    testlets = 4,
    varname.testlets = "content",
    lbl.testlets = c("units", "change", "space", "data"),
    age = 6,
    testyear = 2013
  )
)
ex2$vars
head(ex2$resp)

save(ex2, file = "data/ex2.rda")


#'
#' Example 3
#'
#' The example is loosely based on the scaling of reading in B129 The
#' simulated test includes 13 MC and 4 CMC items with 3 booklets.
#'

ex3 <- sim.testlet(
  N = 2100,
  I = 21,
  SCR = 0,
  CMC = 5,
  booklets = 3,
  seed = 8631,
  rotation = TRUE,
  control = list(
    varname = "reg7",
    main.school = 0,
    unique = c(5, 6, 5),
    varname.testlets = "texttype",
    lbl.testlets =  c("information", "instruction", "advertising", "commenting",
                      "literary"),
    lbl.booklets = c("easy", "medium", "difficult")
  ),
  age = 12,
  testyear = 2018
)
ex3$resp$school <- NULL
ex3$vars
head(ex3$resp)

save(ex3, file = "data/ex3.rda")


#'
#' Example 4
#'
#' The example is loosely based on the scaling of science in B114. The
#' simulated test includes a multi-stage test with 34 items.
#'

ex4 <- sim.testlet(
  N = 3700,
  I = 34,
  SCR = 0,
  CMC = 8,
  mst = c(2, 2, 3),
  seed = 9513,
  rotation = TRUE,
  control = list(
    varname = "sca14",
    main.school = 0,
    main.mig = 0,
    unique = c(4, 4, 4),
    testlets = 2,
    varname.testlets = "component",
    lbl.testlets =  c("KOS", "KAS"),
    age = 45,
    testyear = 2021
  )
)
ex4$resp$school <- ex4$resp$mig <- NULL
ex4$vars
head(ex4$resp)

save(ex4, file = "data/ex4.rda")


#'
#' # Test simulated data
#'
#' ## Example 1
#'

library(TAM)
load("data/ex1.rda")
items <- ex1$vars$item[ex1$vars$dich]
resp <- ex1$resp[ex1$resp$valid, ]
resp[resp < 0] <- NA

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
items <- ex2$vars$item[ex2$vars$mixed]
resp <- ex2$resp[ex2$resp$valid, ]
resp[resp < 0] <- NA

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
mod2.4 <- TAM::tam.mml(resp = resp[, items], group = factor(resp$school, 0:2),
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
items <- ex3$vars$item[ex3$vars$mixed]
resp <- ex3$resp[ex3$resp$valid, ]
resp[resp < 0] <- NA

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

# Rasch model for easy booklet
mod3.5 <- TAM::tam.mml(
  resp = resp[resp$easy, ex3$vars$item[ex3$vars$mixed & ex3$vars$easy]],
  verbose = FALSE
)
summary(mod3.5)
