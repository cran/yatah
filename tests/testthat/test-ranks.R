lineage1 <- "k__Bacteria|p__Verrucomicrobia|c__Verrucomicrobiae"
lineage2 <- "k__Bacteria|p__Firmicutes|c__Clostridia"
lineage3 <- paste0("k__Bacteria|p__Proteobacteria|c__Betaproteobacteria",
                   "|o__Burkholderiales|f__Comamonadaceae",
                   "|g__Delftia|s__Delftia_unclassified")
lineage4 <- "k__Bac|p__Fir|c__Clos|o__Clost|f__Rumi|g__Subdo|s__Sub_su|t__X_56Z"
lineage5 <- "k__Viruses"
lineages <- c(lineage1, lineage2, lineage3, lineage4, lineage5)

lineage1bis <- "k__Bacteria;p__Verrucomicrobia;c__Verrucomicrobiae"
lineage2bis <- "k__Bacteria;p__Firmicutes;c__Clostridia"
lineagesbis <- c(lineage1bis, lineage2bis)

errormessdepth <- "Lineages don't have the same depth."

#### is_rank() ####

test_that("is_rank() is correct", {
  expect_equal(is_rank(lineages, "class"), c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(is_rank(lineages, "order"), c(FALSE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(is_rank(lineages, "sp"), c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(is_rank(lineages, "strain"), c(FALSE, FALSE, FALSE, TRUE, FALSE))
  options(yatah_sep = ";")
  expect_equal(is_rank(lineagesbis, "class"), c(TRUE, TRUE))
  options(yatah_sep = "\\|")
})

test_that("is_rank() throws error when needed", {
  expect_error(is_rank(lineages, "OTUs"))
})

#### is_at_least_rank() ####

test_that("is_at_least_rank() is correct", {
  expect_equal(is_at_least_rank(lineages, "kingdom"),
               c(TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(is_at_least_rank(lineages, "class"),
               c(TRUE, TRUE, TRUE, TRUE, FALSE))
  expect_equal(is_at_least_rank(lineages, "order"),
               c(FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(is_at_least_rank(lineages, "sp"),
               c(FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(is_at_least_rank(lineages, "strain"),
               c(FALSE, FALSE, FALSE, TRUE, FALSE))
  options(yatah_sep = ";")
  expect_equal(is_at_least_rank(lineagesbis, "class"), c(TRUE, TRUE))
  expect_equal(is_at_least_rank(lineagesbis, "phylum"), c(TRUE, TRUE))
  expect_equal(is_at_least_rank(lineagesbis, "genus"), c(FALSE, FALSE))
  options(yatah_sep = "\\|")
})

test_that("is_at_least_rank() throws error when needed", {
  expect_error(is_at_least_rank(lineages, "OTUs"))
})


#### is_clade() ####

test_that("is_clade() is correct", {
  expect_equal(is_clade(lineages, "Clostridia", "class"),
               c(FALSE, TRUE, FALSE, FALSE, FALSE))
  options(yatah_sep = ";")
  expect_equal(is_clade(lineagesbis, "Clostridia", "class"),
               c(FALSE, TRUE))
  options(yatah_sep = "\\|")
  expect_equal(is_clade(lineages, "Verrucomicrobia"),
               c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(is_clade(lineages, "Bacteria"),
               c(TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(is_clade(lineages, "Bacter"),
               c(FALSE, FALSE, FALSE, FALSE, FALSE))
})

test_that("is_clade() throws error when needed", {
  expect_error(is_clade(lineages, "Bacteria", "otu"))
  expect_error(is_clade(lineages, "Bacteria", c("class", "phylum")))
  expect_error(is_clade(lineages, c("Bacteria", "Clostridia")))
})


#### get_last_clade() ####

test_that("get_last_clade() is correct", {
  expect_equal(get_last_clade(lineages[1:2]),
               c("Verrucomicrobiae", "Clostridia"))
  options(yatah_sep = ";")
  expect_equal(get_last_clade(lineagesbis),
               c("Verrucomicrobiae", "Clostridia"))
  options(yatah_sep = "\\|")
  expect_equal(get_last_clade(lineages, same = FALSE),
               c("Verrucomicrobiae", "Clostridia",
                 "Delftia_unclassified", "X_56Z", "Viruses"))
})

test_that("get_last_clade() throws error when needed", {
  expect_error(get_last_clade(lineages, same = TRUE), errormessdepth)
})

#### get_clade() ####

test_that("get_clade() is correct", {
  expect_equal(get_clade(lineages[1:2], rank = "phylum"),
               c("Verrucomicrobia", "Firmicutes"))
  expect_equal(get_clade(lineages[1:2], rank = "class"),
               c("Verrucomicrobiae", "Clostridia"))
  options(yatah_sep = ";")
  expect_equal(get_clade(lineagesbis, rank = "phylum"),
               c("Verrucomicrobia", "Firmicutes"))
  expect_equal(get_clade(lineagesbis, rank = "class"),
               c("Verrucomicrobiae", "Clostridia"))
  options(yatah_sep = "\\|")
  expect_equal(get_clade(lineages, rank = "kingdom", same = FALSE),
               c("Bacteria", "Bacteria",
                 "Bacteria", "Bac", "Viruses"))
  expect_equal(get_clade(lineages, rank = "class", same = FALSE),
               c("Verrucomicrobiae", "Clostridia",
                 "Betaproteobacteria", "Clos", NA))
  expect_equal(get_clade(lineages, rank = "strain", same = FALSE),
               c(NA, NA, NA, "X_56Z", NA))
})

test_that("get_clade() throws error when needed", {
  expect_error(get_clade(lineages, same = TRUE), errormessdepth)
})


#### get_last_rank() ####

test_that("get_last_rank() is correct", {
  expect_equal(get_last_rank(lineages[1:2]), c("class", "class"))
  options(yatah_sep = ";")
  expect_equal(get_last_rank(lineagesbis), c("class", "class"))
  options(yatah_sep = "\\|")
  expect_equal(get_last_rank(lineages, same = FALSE),
               c("class", "class", "species", "strain", "kingdom"))
})

test_that("get_last_rank() throws error when needed", {
  expect_error(get_last_rank(lineages, same = TRUE), errormessdepth)
})

#### get_all_clades() ####

test_that("get_all_clades() have the correct output format", {
  expect_is(get_all_clades(lineages), "character")
  expect_is(get_all_clades(lineages, simplify = FALSE), "data.frame")
  expect_equal(dim(get_all_clades(lineages, simplify = FALSE)),
               c(length(get_all_clades(lineages)), 2))
  expect_equal(colnames(get_all_clades(lineages, simplify = FALSE)),
               c("clade", "rank"))
})

test_that("get_all_clades() is correct", {
  expect_equal(get_all_clades(c(lineage1, lineage2)),
               c("Bacteria", "Clostridia", "Firmicutes", "Verrucomicrobia",
                 "Verrucomicrobiae"))
  expect_equal(get_all_clades(lineage5), "Viruses")
  expect_equal(get_all_clades(c(lineage4, lineage5), simplify = FALSE)$clade,
               c("Bac", "Clos", "Clost", "Fir", "Rumi",
                 "Sub_su", "Subdo", "Viruses", "X_56Z"))
  expect_equal(get_all_clades(c(lineage4, lineage5), simplify = FALSE)$rank,
               c("kingdom", "class", "order", "phylum", "family",
                 "species", "genus", "kingdom", "strain"))
  expect_equal(get_all_clades(lineage5), "Viruses")
  temp_allclades <- get_all_clades(lineages[1:2])
  options(yatah_sep = ";")
  expect_equal(get_all_clades(lineagesbis), temp_allclades)
  options(yatah_sep = "\\|")
})
