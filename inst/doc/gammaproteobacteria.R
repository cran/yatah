## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE------------------------------------------------
library(ggtree) # from Bioconductor
library(dplyr)
library(yatah)

## ------------------------------------------------------------------------
abundances <- as_tibble(yatah::abundances)
print(abundances, n_extra = 2)

## ----data, message=FALSE-------------------------------------------------
taxonomy <- select(abundances, lineages)
taxonomy

## ----filter--------------------------------------------------------------
gammap_genus <-
  taxonomy %>% 
  filter(is_clade(lineages, "Gammaproteobacteria"),
         is_rank(lineages, "genus")) %>% 
  mutate(genus = last_clade(lineages))
gammap_genus

## ----table---------------------------------------------------------------
gammaprot_table <-
  gammap_genus %>% 
  pull(lineages) %>% 
  taxtable()
as_tibble(gammaprot_table)

## ----tree----------------------------------------------------------------
gammaprot_tree <- taxtree(gammaprot_table)
gammaprot_tree

## ----ggtree, fig.width=7, fig.height=7-----------------------------------
ggtree(gammaprot_tree) +
  geom_tiplab(hjust = 1, geom = "label") +
  geom_nodelab(hjust = 0, size = 3)

