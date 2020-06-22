library(rvest)
library(tidyverse)
library(igraph)
library(ggnetwork)

# -- co-authorship ties between academic organizations -------------------------

e <- readr::read_tsv("data/affiliations.tsv", col_types = "ccc") %>%
  select(abstract, affiliation = affiliation_clean) %>%
  group_by(abstract) %>%
  summarise(edges = list(affiliation)) %>%
  mutate(edges = map(edges, ~ tidyr::crossing(i = .x, j = .x)))

# finalize edges
e <- tidyr::unnest(e, edges) %>%
  # de-duplication
  dplyr::rowwise() %>%
  dplyr::mutate(
    i = sort(c(i, j))[ 1 ], # not very efficient (sorting twice)
    j = sort(c(i, j))[ 2 ]
  ) %>%
  # remove duplicates
  distinct() %>%
  # remove self-ties
  filter(i != j) %>%
  # count edges per abstract (computed only for digression below)
  group_by(abstract) %>%
  mutate(w = n())

# -- digression re: weighting the edges ----------------------------------------

# For pseudo Newman 2001 weights, replace n() by:
# 1 / if_else(n() == 1, 1, n() - 1)

# Details and discussion:
# https://toreopsahl.com/tnet/two-mode-networks/projection/

# Very skewed edge 'weights'...
table(e$w) / as.integer(names(table(e$w))) # `1` ties = 2 authors

# ... because some authors have multiple affiliations: remember the true number
# of authors is lower, range 1--6, which is why `w` above does not and cannot
# compute 100% genuine Newman 2001 weights.

# [NOTE] single duplicated edge (abstracts 0088 and 0117, same authors)
ungroup(e) %>%
  count(i, j) %>%
  filter(n > 1) %>%
  inner_join(e, by = c("i", "j"))

# export
readr::write_tsv(e, "data/edges-1mode.tsv")

# -- unweighted, undirected one-mode network -----------------------------------

n <- cbind(e$i, e$j) %>%
  # remove duplicated edge
  unique() %>%
  # undirected graph
  igraph::graph_from_edgelist(directed = FALSE)

table(degree(n)) # range 1--9
sort(degree(n)[ degree(n) > 5 ]) # USA x 3, DEU x 3, GBR x 2, ITA, NOR, CHE

V(n)$hi_degree <- dplyr::if_else(degree(n) >= 4, V(n)$name, NA_character_)

ggplot(n, aes(x, y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_nodes() +
  geom_nodelabel(aes(label = hi_degree)) +
  theme_blank()

ggsave("example-unweighted-graph.png", width = 12, height = 10)

# kthxbye
