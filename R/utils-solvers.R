#' Re-wrote the equations with the correct matrix syntax that will be used to evaluate
#' the expressions inside the Gauss Seidel algorithm
#'
#' @param ordered_eqs ordered equations after passing through \code{.sfcr_find_order()} function.
#' @param external Tibble of exogenous values and parameters, already separated with
#' \code{.eq_as_tb()} function.
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.prep_equations <- function(ordered_eqs, external) {

  # pend <- paste0("(?<![[:alnum:]]|\\.|\\_)(", paste0(ordered_eqs$lhs, collapse = "|"), ")(?![[:alnum:]]|\\[|\\.|\\_)")
  # pendlag <- paste0("(?<![[:alnum:]]|\\.|\\_)(", paste0(ordered_eqs$lhs, collapse = "|"), ")(?=___)")
  # pexg <- paste0("(?<![[:alnum:]]|\\.|\\_)(", paste0(c(external$lhs), collapse = "|"), ")(?![[:alnum:]]|\\[|\\.|\\_)")
  # pexglag <- paste0("(?<![[:alnum:]]|\\.|\\_)(", paste0(c(external$lhs), collapse = "|"), ")(?=___)")
  #
  # # Operating on rows
  # x <- ordered_eqs %>%
  #   dplyr::mutate(rhs = gsub(pend, "m\\[i,'\\1'\\]", .data$rhs, perl = T),
  #                 rhs = gsub(pendlag, "m\\[i-1,'\\1'\\]", .data$rhs, perl = T),
  #                 rhs = gsub(pexg, "m\\[i,'\\1'\\]", .data$rhs, perl = T),
  #                 rhs = gsub(pexglag, "m\\[i-1,'\\1'\\]", .data$rhs, perl = T),
  #                 rhs = gsub("___", "", .data$rhs),
  #                 id = dplyr::row_number())

  x <- ordered_eqs %>%
    dplyr::mutate(rhs = gsub(.pvar(ordered_eqs$lhs), "m\\[.i, '\\1'\\]", .data$rhs, perl = T),
                  rhs = gsub(.pvarlag(ordered_eqs$lhs), "m\\[.i-1,'\\1'\\]", .data$rhs, perl = T),
                  rhs = gsub(.pvar(external$lhs), "m\\[.i,'\\1'\\]", .data$rhs, perl = T),
                  rhs = gsub(.pvarlag(external$lhs), "m\\[.i-1,'\\1'\\]", .data$rhs, perl = T),
                  rhs = gsub("___", "", .data$rhs),
                  id = dplyr::row_number())

  # Operating on columns
  # x <- ordered_eqs %>%
  #   dplyr::mutate(rhs = gsub(pend, "m\\['\\1', i\\]", rhs, perl = T),
  #                 rhs = gsub(pendlag, "m\\['\\1', i-1\\]", rhs, perl = T),
  #                 rhs = gsub(pexg, "m\\['\\1', i\\]", rhs, perl = T),
  #                 rhs = gsub(pexglag, "m\\['\\1', i-1\\]", rhs, perl = T),
  #                 rhs = gsub("___", "", rhs),
  #                 id = dplyr::row_number())

  # Uncomment to loop on a list instead of a matrix
  # x <- ordered_eqs %>%
  #  dplyr::mutate(rhs = gsub(pend, "m\\[\\['\\1'\\]\\]\\[\\[i\\]\\]", rhs, perl = T),
  #                rhs = gsub(pendlag, "m\\[\\['\\1'\\]\\]\\[\\[i-1\\]\\]", rhs, perl = T),
  #                rhs = gsub(pexg, "m\\[\\['\\1'\\]\\]\\[\\[i\\]\\]", rhs, perl = T),
  #                rhs = gsub(pexglag, "m\\[\\['\\1'\\]\\]\\[\\[i-1\\]\\]", rhs, perl = T),
  #                rhs = gsub("___", "", rhs),
  #                id = dplyr::row_number())

  return(x)
}

#' Make the underlying matrix that will be modified in place with the Gauss Seidel
#' algorithm
#'
#' @param equations Prepared equations.
#' @param external Exogenous and parameters as tibble.
#' @param periods Total number of rows.
#' @param initial Initial values, if supplied.
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.make_matrix <- function(equations, external, periods, initial = NULL) {

  ends <- rep(1e-15, vctrs::vec_size(equations$lhs))
  names(ends) <- equations$lhs

  # Exogenous variables are supplied
  exgs <- rep(1e-15, vctrs::vec_size(external$lhs))
  exgs_names <- external$lhs
  exg_exprs <- purrr::map(external$rhs, function(x) parse(text=x))

  # Blocks of independent equations
  blocks <- sort(unique(equations$block))
  blocks <- paste0("block", blocks)
  lblocks <- rep(0, vctrs::vec_size(blocks))
  names(lblocks) <- blocks

  mcols <- vctrs::vec_size(ends) + vctrs::vec_size(exgs) + vctrs::vec_size(lblocks)
  mnames <- c(names(ends), exgs_names, names(lblocks))

  # Matrix with variables (operating on rows)
  m1 <- matrix(c(ends, exgs, lblocks), nrow = periods, ncol = mcols, dimnames = list(1:periods, mnames), byrow = T)

  for (var in seq_along(exgs_names)) {
    m1[, exgs_names[[var]]] <- eval(exg_exprs[[var]])
  }

  # All variables start at almost 0 (including exogenous)
  # Otherwise problems may arise if some endogenous
  # variable depends on lagged exogenous

  m1[1, ] <- 1e-15
  #m1[1, ] <- 1

  if (!is.null(initial)) {
    initial <- .eq_as_tb(initial)
    initial_names <- initial$lhs
    initial_exprs <- purrr::map(initial$rhs, function(x) parse(text = x))

    for (var in seq_along(initial_names)) {
      m1[1, initial_names[[var]]] <- eval(initial_exprs[[var]])
    }
  }

  # Matrix with variables (operating on columns)
  # m1 <- matrix(c(ends, exgs, lblocks), nrow = mcols, ncol = periods, dimnames = list(mnames, 1:periods), byrow = F)
  #
  # for (var in seq_along(exgs_names)) {
  #   m1[exgs_names[[var]], ] <- eval(exg_exprs[[var]])
  # }
  #
  # m1[, 1] <- 1
  #
  # if (!is.null(initial)) {
  #   initial <- .eq_as_tb(initial)
  #   initial_names <- initial$lhs
  #   initial_exprs <- purrr::map(initial$rhs, function(x) parse(text = x))
  #
  #   for (var in seq_along(initial_names)) {
  #     m1[initial_names[[var]], 1] <- eval(initial_exprs[[var]])
  #   }
  # }

  # Loop on a list instead of a matrix
  #m1 <- as.list(data.frame(m1))

  return(m1)
}

#' Prep equations for Broyden and Newton solvers
#'
#' @param .block Blocks of equations
#'
#' @author João Macalós
#'
#' @keywords internal
#'
.prep_broyden <- function(.block) {
  for (.i in seq_len(vctrs::vec_size(.block))) {
    .block$rhs2 <- gsub(.block$lhs2[[.i]], paste0(".x\\[", .i, "\\]"), .block$rhs2)
  }

  return(.block)
}
