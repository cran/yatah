#' Deprecated functions
#'
#' @importFrom lifecycle deprecate_warn
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Use [get_last_clade()] instead of `last_clade()`.
#'
#' @export
#' @keywords internal
#' @name deprecated
last_clade <- function(...) {
  deprecate_warn("1.0.0", "last_clade()", "get_last_clade()")

  get_last_clade(...)
}

#' @rdname deprecated
#'
#' @description
#'
#' Use [get_last_rank()] instead of `last_rank()`.
#'
#' @export
last_rank <- function(...) {
  deprecate_warn("1.0.0", "last_rank()", "get_last_rank()")

  get_last_rank(...)
}

#' @rdname deprecated
#'
#' @description
#'
#' Use [get_all_clades()] instead of `all_clades()`.
#'
#' @export
all_clades <- function(...) {
  deprecate_warn("1.0.0", "all_clades()", "get_all_clades()")

  get_all_clades(...)
}
