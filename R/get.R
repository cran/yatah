#' Extract the last clade of a lineage
#'
#' @inheritParams is_rank
#' @param same logical. Does the lineage have the same depth? Default to `TRUE`.
#'
#' @return A string. The last clades of the given lineages.
#' @importFrom stringr str_remove
#' @export
#'
#' @examples
#' lineage1 <- "k__Bacteria|p__Verrucomicrobia|c__Verrucomicrobiae"
#' lineage2 <- "k__Bacteria|p__Firmicutes|c__Clostridia"
#' get_last_clade(c(lineage1, lineage2))
get_last_clade <- function(lineage, same = TRUE) {

  error_lineage(lineage)

  if (same) depth(lineage)

  str_remove(lineage, ".*__")
}

#' Extract the clade of a desired rank in a lineage
#'
#' @inheritParams get_last_clade
#' @param rank The desired rank of the clade.
#'
#' @return A string.
#'
#' @importFrom stringr str_extract str_remove_all
#' @export
#'
#' @examples
#' lineage1 <- "k__Bacteria|p__Verrucomicrobia|c__Verrucomicrobiae"
#' lineage2 <- "k__Bacteria|p__Firmicutes|c__Clostridia"
#' get_clade(c(lineage1, lineage2), "phylum")
get_clade <- function(lineage, rank = yatah::all_ranks, same = TRUE) {

  error_lineage(lineage)

  if (same) depth(lineage)

  rank <- match.arg(rank)
  letter <- names(which(.ranks == rank))

  sep <- getOption("yatah_sep", default = "\\|")

  str_remove_all(
    str_extract(lineage, paste0("(?<=", letter, "__).*")),
    paste0(sep, ".*")
    )
}


#' Extract the last rank of a lineage
#'
#' @inheritParams get_last_clade
#'
#' @return A string. The last rank of the given lineages.
#' @importFrom stringr str_remove
#' @export
#'
#' @examples
#' lineage1 <- "k__Bacteria|p__Verrucomicrobia|c__Verrucomicrobiae"
#' lineage2 <- "k__Bacteria|p__Firmicutes|c__Clostridia"
#' get_last_rank(c(lineage1, lineage2))
get_last_rank <- function(lineage, same = TRUE) {

  error_lineage(lineage)

  if (same) depth(lineage)

  letter <- str_sub(str_remove(lineage, paste0("__", .allchr, "*$")),
                    start = -1)

  unname(.ranks[letter])
}


#' Extract all clades present in the lineages
#'
#' @details If a clade correspond to different ranks (e.g. Actinobacteria
#' is both a phylum and a clade), it will be displayed only one time when
#' \code{simplify} is set to \code{TRUE}. It is also the case for different
#' clades with same name and same rank when \code{simplify} is set to
#' \code{FALSE}.
#'
#' @inheritParams get_last_clade
#' @param simplify logical. Should the output be a vector or a dataframe?
#'
#' @return The clades present in the lineage. Vector of ordered strings
#'  or data.frame.
#' @importFrom stringr str_split str_sub
#' @export
#'
#' @examples
#' lineage1 <- "k__Bacteria|p__Verrucomicrobia|c__Verrucomicrobiae"
#' lineage2 <- "k__Bacteria|p__Firmicutes|c__Clostridia"
#' lineage3 <- "k__Bacteria|p__Actinobacteria|c__Actinobacteria"
#' get_all_clades(c(lineage1, lineage2, lineage3))
#' get_all_clades(c(lineage1, lineage2, lineage3), simplify = FALSE)
get_all_clades <- function(lineage, simplify = TRUE) {

  error_lineage(lineage)

  sep <- getOption("yatah_sep", default = "\\|")

  clades <- unique(unlist(str_split(lineage, pattern = sep)))

  if (simplify) {

    return(sort(unique(str_sub(clades, start = 4))))

  } else {

    ranks_ <- .ranks[str_sub(clades, end = 1)]
    df <- data.frame(clade = str_sub(clades, start = 4), rank = ranks_,
                     stringsAsFactors = FALSE)
    df <- unique(df)
    ind <- order(df$clade)

    return(df[ind, ])

  }
}


