lineage1 <- "k__Bacteria|p__Verrucomicrobia|c__Verrucomicrobiae"
lineage2 <- "k__Bacteria|p__Firmicutes|c__Clostridia"

lineages <- c(lineage1, lineage2)


test_that("last_clade() is correctly deprecated", {
  expect_warning(last_clade(lineages))
  expect_equal(last_clade(lineages),
               get_last_clade(lineages))
})

test_that("last_rank() is correctly deprecated", {
  expect_warning(last_rank(lineages))
  expect_equal(last_rank(lineages),
               get_last_rank(lineages))
})

test_that("all_clades() is correctly deprecated", {
  expect_warning(all_clades(lineages))
  expect_equal(all_clades(lineages),
               get_all_clades(lineages))
})
