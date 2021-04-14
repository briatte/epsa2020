library(rvest)
library(tidyverse)
library(igraph)
library(tnet)

a <- readr::read_tsv("data/affiliations.tsv", col_types = "ccc") %>%
  select(abstract, affils = affiliation_clean)

# export
readr::write_tsv(a, "data/edges-2mode.tsv")

## add an unique ID from 1 to n for each unique affiliation involved

e <- a %>%
  distinct(abstract, affils, .keep_all = TRUE) %>% # truly needed?
  # numeric IDs for abstracts
  group_by(abstract) %>%
  mutate(artid = cur_group_id()) %>%
  # numeric IDs for affiliations
  group_by(affils) %>%
  mutate(affils_id = cur_group_id()) %>%
  ungroup()

net <- select(e, e = affils_id, p = artid)

# -- trying to do everything above with saved edge list from script 02 ---------

# one-mode edge list
e2 <- read_tsv("data/edges-1mode.tsv", col_types = "ccci")
# two-mode edge list
e2 <- bind_rows(
  select(e2, abstract, affiliation = i),
  select(e2, abstract, affiliation = j)
) %>%
  # get numeric id for abstracts (event)
  group_by(abstract) %>%
  mutate(abs_id = cur_group_id(), .before = 2) %>%
  # get numeric id for affiliations (actor)
  group_by(affiliation) %>%
  mutate(aff_id = cur_group_id()) %>%
  distinct() %>%
  ungroup()

# e2 and net are different in size...
nrow(net)
nrow(e2)

# ... and so return different projections
dim(projecting_tm(select(e2, abs_id, aff_id), method = "Newman"))
dim(projecting_tm(net, method = "Newman"))

# I think that's because your version (correctly) uses all abstract-affil edges,
# including when it's from a single author:
select(e, affils) %>% group_by(affils) %>% count(sort = TRUE)

# mine has e.g. Yale one less time than you because, I guess, there must be a
# single-authored paper from a Yale person, and that edge is not in edges.tsv
select(e2, affiliation) %>% group_by(affiliation) %>% count(sort = TRUE)

# this seems to confirms the "I guess" above, abstract 82 = single-authored Yale
filter(e, artid %in% c(1L, 31L, 38L, 79L, 80L, 82L, 84L, 135L))

# ------------------------------------------------------------------------------

# weighting step
# Newman's method or Netscity method according to the Whole Normalised Counting method used in NETSCITY

onemode <- projecting_tm(net, method = "Newman")
# or projecting_tm_twisted(net, method = "Netscity") if you wanna apply the Whole Normalised Counting method used in NETSCITY
# projecting_tm_twisted function available here: https://framagit.org/MarionMai/netscityr/-/blob/master/Twisted-Projecting_tm-Function.R

# transform the result into an igraph object

g <- graph_from_data_frame(cbind(onemode$i,onemode$j),directed = FALSE)
E(g)$weight <- as.double(onemode$w)

# remove multiple lines
g <- simplify(g, remove.multiple = TRUE, edge.attr.comb = c(weight = "first", type = "ignore"))

V(g)$name <- as.character(e$affils[match(V(g)$name,e$affils_id)]) # allows to replace numeric id_numbers by affiliations to name vertices
V(g)$label <- V(g)$name # If necessary add "str_to_title"

sum(E(g)$weight) # in case, you use the Netscity method, it would give you the number of articles (66!)
# with the Newman method, the edges' sum gives you 80.5

flows <- bind_cols(get.edgelist(g),W_collab = E(g)$weight) %>%
  rename(Affil_i = "...1", Affil_j = "...2", )

# export the flow table

write_tsv(flows, "flows.tsv")
