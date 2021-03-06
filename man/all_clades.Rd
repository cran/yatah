% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{all_clades}
\alias{all_clades}
\title{Extract all clades present in the lineages}
\usage{
all_clades(lineage, simplify = TRUE)
}
\arguments{
\item{lineage}{string. Vector of lineages.}

\item{simplify}{logical. Should the output be a vector or a dataframe?}
}
\value{
The clades present in the lineage. Vector of ordered strings
or data.frame.
}
\description{
Extract all clades present in the lineages
}
\examples{
lineage1 <- "k__Bacteria|p__Verrucomicrobia|c__Verrucomicrobiae"
lineage2 <- "k__Bacteria|p__Firmicutes|c__Clostridia"
all_clades(c(lineage1, lineage2))
all_clades(c(lineage1, lineage2), simplify = FALSE)
}
