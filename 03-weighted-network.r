library(rvest)
library(tidyverse)
library(igraph)
library(tnet)

a <- readr::read_tsv("data/affiliations.tsv", col_types = "ccc") %>%
  select(abstract, affiliation = affiliation_clean)

# export
readr::write_tsv(a, "data/edges-2mode.tsv")

# build two-mode edge list
e <- a %>%
  # remove duplicate (abstract, affiliation) pairs
  # [NOTE] `.keep_all = TRUE` not required (?), passed for safety (?)
  distinct(abstract, affiliation, .keep_all = TRUE) %>%
  # numeric IDs for abstracts
  group_by(abstract) %>%
  mutate(abs_id = cur_group_id()) %>%
  # numeric IDs for affiliations
  group_by(affiliation) %>%
  mutate(aff_id = cur_group_id()) %>%
  ungroup()

<<<<<<< Updated upstream
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
=======
# (affiliation -> abstract) two-mode network
n <- select(e, aff_id, abs_id) %>%
  as.matrix() %>%
  # weighting with Newman's method
  projecting_tm(method = "Newman")

# [NOTE] Alternative weighting method: Netscity
#
# See Whole Normalised Counting method used in [1,2]
# [1] https://framagit.org/MarionMai/netscityr
# [2] https://www.irit.fr/netscity
#
# Function: `projecting_tm_twisted(net, method = "Netscity")`
# URL: https://framagit.org/MarionMai/netscityr/-/blob/master/Twisted-Projecting_tm-Function.R

# igraph object
g <- igraph::graph_from_data_frame(cbind(n$i, n$j), directed = FALSE)
E(g)$weight <- n$w

# remove multiple edges and loops
g <- igraph::simplify(
  g,
  remove.multiple = TRUE,
  edge.attr.comb = c(weight = "first", type = "ignore")
)

# [TOFIX] `match` produces NA values
V(g)$name <- e$affiliation[ match(V(g)$name, e$aff_id) ]
V(g)$label <- V(g)$name # if necessary, add `str_to_title`

# with the Newman method, the edges' sum gives you 80.5
# in case, you use the Netscity method, it would give you the number of articles (66!)
sum(E(g)$weight)

# export flow table
bind_cols(
  get.edgelist(g),
  W_collab = E(g)$weight
) %>%
  rename(Affil_i = "...1", Affil_j = "...2") %>%
  readr::write_tsv("data/flows.tsv")
>>>>>>> Stashed changes

# have a nice day
