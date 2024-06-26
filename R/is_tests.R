#' Test if a string is a lineage
#'
#' @param string string to be tested as lineage.
#'
#' @details Alphanumeric character, hyphen, dots, square brackets
#' and non-consecutive underscores are allowed in clades names.
#'
#' @return A logical.
#' @importFrom stringr str_detect str_replace_all
#' @export
#'
#' @examples
#' is_lineage("k__Bacteria|p__Firmicutes|c__Clostridia|o__Clostridiales")
is_lineage <- function(string){

  sep <- getOption("yatah_sep", default = "\\|")

  only_clades <- str_replace_all(string,
                                 paste0("(^|", sep, ")[kpcofgst]__"), " ")
  cond1 <- ! str_detect(only_clades, "__")

  cond2 <- str_detect(string, paste0("^k__", .allchr, "*",
                                     "($|", sep, "p__", .allchr, "*)",
                                     "($|", sep, "c__", .allchr, "*)",
                                     "($|", sep, "o__", .allchr, "*)",
                                     "($|", sep, "f__", .allchr, "*)",
                                     "($|", sep, "g__", .allchr, "*)",
                                     "($|", sep, "s__", .allchr, "*)",
                                     "($|", sep, "t__", .allchr, "*)$"))

  as.logical(cond1 * cond2)
}



#' Test if a lineage goes down to a specified rank
#'
#' @param lineage string. Vector of lineages.
#' @param rank string. One of \code{c("kingdom", "phylum", "class",
#' "order", "family", "genus", "species", "strain")} with partial matching.
#'
#' @return logical.
#' @importFrom stringr str_detect
#' @export
#'
#' @examples
#' lineage1 <- "k__Bacteria|p__Verrucomicrobia|c__Verrucomicrobiae"
#' lineage2 <- "k__Bacteria|p__Firmicutes|c__Clostridia"
#' is_rank(c(lineage1, lineage2), "class")
#' is_rank(c(lineage1, lineage2), "order")
is_rank <- function(lineage, rank = yatah::all_ranks) {

  error_lineage(lineage)

  rank <- match.arg(rank)
  letter <- names(which(.ranks == rank))

  str_detect(lineage, paste0(letter, "__", .allchr, "*$"))
}

#' @rdname is_rank
#' @export
#' @examples
#' is_at_least_rank(c(lineage1, lineage2), "phylum")
#' is_at_least_rank(c(lineage1, lineage2), "order")
is_at_least_rank <- function(lineage, rank = yatah::all_ranks) {

  error_lineage(lineage)

  rank <- match.arg(rank)
  letter <- names(which(.ranks == rank))

  str_detect(lineage, paste0(letter, "__", .allchr))

}


#' Test if a lineage belongs to a clade
#'
#' @details  If \code{rank} is set to \code{.}, clade is looked for
#' among all ranks.
#'
#' @inheritParams is_rank
#' @param clade string.
#'
#' @return logical.
#' @export
#'
#' @examples
#' lineage1 <- "k__Bacteria|p__Verrucomicrobia|c__Verrucomicrobiae"
#' lineage2 <- "k__Bacteria|p__Firmicutes|c__Clostridia"
#' is_clade(c(lineage1, lineage2), clade = "Verrucomicrobia", rank = "phylum")
#' is_clade(c(lineage1, lineage2), clade = "Clostridia")
is_clade <- function(lineage, clade,
                     rank = c(".", yatah::all_ranks)) {

  error_lineage(lineage)

  sep <- getOption("yatah_sep", default = "\\|")

  stopifnot(length(clade) == 1)

  rank <- match.arg(rank)
  letter <- ifelse(rank == "strain", "t", str_sub(rank, end = 1))

  str_detect(lineage, paste0("(^|", sep, ")", letter,
                             "__", clade, "($|", sep, ")"))

}

