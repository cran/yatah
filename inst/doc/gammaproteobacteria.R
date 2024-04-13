## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(dplyr)
library(yatah)

## -----------------------------------------------------------------------------
abundances <- as_tibble(yatah::abundances)
print(abundances, max_extra_cols = 2)

## ----data, message=FALSE------------------------------------------------------
taxonomy <- select(abundances, lineages)
taxonomy

## ----filter-------------------------------------------------------------------
gammap_genus <-
  taxonomy %>% 
  filter(is_clade(lineages, "Gammaproteobacteria"),
         is_rank(lineages, "genus")) %>% 
  mutate(genus = get_last_clade(lineages))
gammap_genus

## ----table--------------------------------------------------------------------
gammaprot_table <-
  gammap_genus %>% 
  pull(lineages) %>% 
  taxtable()
as_tibble(gammaprot_table)

## ----tree---------------------------------------------------------------------
gammaprot_tree <- taxtree(gammaprot_table)
gammaprot_tree

## ----ggtree, fig.width=7, fig.height=7----------------------------------------
plot(gammaprot_tree, show.node.label = TRUE, cex = 0.7, 
     main = "Taxonomy of Gammaproteobacteria")

