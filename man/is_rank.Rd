% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_tests.R
\name{is_rank}
\alias{is_rank}
\alias{is_at_least_rank}
\title{Test if a lineage goes down to a specified rank}
\usage{
is_rank(lineage, rank = yatah::all_ranks)

is_at_least_rank(lineage, rank = yatah::all_ranks)
}
\arguments{
\item{lineage}{string. Vector of lineages.}

\item{rank}{string. One of \code{c("kingdom", "phylum", "class",
"order", "family", "genus", "species", "strain")} with partial matching.}
}
\value{
logical.
}
\description{
Test if a lineage goes down to a specified rank
}
\examples{
lineage1 <- "k__Bacteria|p__Verrucomicrobia|c__Verrucomicrobiae"
lineage2 <- "k__Bacteria|p__Firmicutes|c__Clostridia"
is_rank(c(lineage1, lineage2), "class")
is_rank(c(lineage1, lineage2), "order")
is_at_least_rank(c(lineage1, lineage2), "phylum")
is_at_least_rank(c(lineage1, lineage2), "order")
}
