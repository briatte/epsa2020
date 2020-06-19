library(rvest)
library(tidyverse)
library(igraph)
library(ggnetwork)

# -- parse abstracts -----------------------------------------------------------

d <- tibble()
for (i in list.files("abstract", pattern = "^\\d", full.names = TRUE)) {

  h <- read_html(i)

  d <- tibble(
    abstract = i,
    authors = html_node(h, ".authors") %>%
      html_text(trim = TRUE),
    affiliations = html_node(h, ".affiliations") %>%
      html_text(trim = TRUE)
  ) %>%
  bind_rows(d)

}

# -- co-authorship ties between authors ----------------------------------------
# (uninteresting: almost every author appears only once)

# a <- str_remove_all(d$authors, "\\s\\d+") %>%
#   str_replace_all("\\s+", " ")
#
# # edges (with duplicated edges)
# e <- map_df(a, ~ str_split(.x, ",\\s+") %>% unlist() %>% tidyr::crossing(i = ., j = .))
#
# # remove self-loops
# e <- filter(e, i != j)
#
# # undirected (and not properly weighted) graph
# n <- igraph::graph_from_edgelist(as.matrix(e), directed = FALSE)
#
# ggplot(n, aes(x, y, xend = xend, yend = yend)) +
#   geom_edges() +
#   geom_nodes() +
#   theme_blank()

# -- co-authorship ties between academic organizations -------------------------

# affiliations
a <- tibble(ids = str_extract_all(d$authors, "\\d"), affils = d$affiliations)

# keep only rows with multiples affiliations
a <- a[ which(purrr::map_int(a$ids, length) > 0), ]

# edge list
e <- a$affils %>%
  # remove spec chars and numbers that might hinder the `str_split` that follows
  str_remove_all("\\\n|\\\t|\\\r|\\d{2,}\\s") %>%
  str_split("\\d\\s") %>%
  # [NOTE]
  # - we do NOT remove de-duplicate edges (degree distribution will be x 2)
  # - we do NOT remove 'ghost' affiliations that are listed but not attached
  #   to a given author (see e.g. affiliation no. 2 in abstract 0002)
  purrr::map_df(~ tidyr::crossing(i = .x, j = .x)) %>%
  # remove empty ties to ""
  dplyr::filter(str_length(i) > 0, str_length(j) > 0) %>%
  # remove self-loops
  dplyr::filter(i != j)

str_clean <- function(x) {
  # first part before comma
  str_extract(x, ".*?,") %>%
    str_remove_all("Univ.*\\s|(\\sUniversity)?,$")
}

u <- unique(c(e$i, e$j))
# View(tibble(u, str_clean(u)))

# export (unclean)
readr::write_tsv(e, "edges.tsv")

# ambiguous
str_prepare <- function(x) {
  x %>%
    str_replace("University of California, Merced", "UC Merced") %>%
    str_replace("University of California, Los Angeles", "UCLA")
}

# fix and shorten
str_fix <- function(x) {
  x %>%
    str_replace("New York", "NYU") %>%
    str_replace("European Institute", "EUI") %>%
    str_replace("London School of Economics and Political Science", "LSE") %>%
    str_replace("Hertie School(Berlin's Berlin)?", "Hertie") %>%
    str_replace("George Washington", "GWU") %>%
    str_replace("Tokyo Science", "TUS Tokyo") %>%
    str_replace("^IAST$|Institute for Advanced Study in Toulouse", "IAST Toulouse") %>%
    str_replace("Science Po", "Sciences Po") %>%
    # different departments
    str_replace("UCSD\\s.*|^Diego$", "UCSD") %>%
    str_replace("Institut Barcelona d'Estudis Internacionals", "IBEI Barcelona") %>%
    str_replace("Columbia Science", "Columbia") %>%
    str_replace("Division of Social Science", "NYU Abu Dhabi") %>%
    str_replace("^Koc$", "Koç") %>%
    str_replace("Central European", "CEU") %>%
    str_replace("^2\\)$", "Paris 2") %>%
    str_replace("Department of Social and Political Sciences", "Bocconi") %>%
    str_replace("School of International Development", "UEA") %>%
    str_replace("Vienna Institute of Demography/.*", "VID Vienna") %>%
    str_replace("Queen Mary London", "QMUL") %>%
    str_replace("Simon Fraser", "SFU") %>%
    str_replace("WZB Berlin Social Science Center", "WZB") %>%
    str_replace("(BI )?Norwegian Business School", "NBS Oslo") %>%
    str_replace("VATT Institute for Economic Research", "VATT Helsinki") %>%
    str_replace("Texas A&M \\(USA\\)", "Texas A&M") %>%
    str_replace("Trinity College Dublin", "TCD") %>%
    str_replace("^Centre$", "Exeter") %>%
    str_replace("Humboldt-Berlin", "Humboldt") %>%
    str_replace("Mannheim Centre for European Social ResearchMannheim", "Mannheim") %>%
    str_replace("London", "UCL") %>%
    str_replace("Nuffield College", "Oxford") %>%
    str_replace("Washington", "WUSL") %>%
    str_replace("Carolina", "USC") %>%
    # for both UPenn and Penn State
    str_replace("Pennsylvania", "Penn") %>%
    str_replace("Goethe-Frankfurt", "G-U Frankfurt") %>%
    str_replace("Collegio Carlo Alberto", "Carlo Alberto") %>%
    str_replace("^ETH Zurich$", "ETH") %>%
    str_replace("^Fabra$", "UPF") %>%
    str_replace("Norwegian Institute of International Affairs", "NIIA Oslo") %>%
    str_replace("Norwegian Institute for Public Health", "NIPH Oslo") %>%
    str_replace("Center for Research and Social Progress", "CRSP Ponte dell'Olio") %>%
    str_replace("^International Institute for Applied Systems Analysis", "IIASA")
}

# View(tibble(from = u, to = str_fix(str_clean(u))))
# View(tibble(from = u, to = str_fix(str_clean(str_prepare(u)))))

e$i <- str_fix(str_clean(str_prepare(e$i)))
e$j <- str_fix(str_clean(str_prepare(e$j)))

# longest strings
u <- unique(c(e$i, e$j))
head(u[ order(str_length(u), decreasing = TRUE) ], 15)

# export (append clean)
readr::read_tsv("edges.tsv", col_types = "cc") %>%
  tibble::add_column(i_clean = e$i, .before = 2) %>%
  tibble::add_column(j_clean = e$j, .before = 4) %>%
  readr::write_tsv("edges.tsv")

n <- as.matrix(e) %>%
  # undirected (and not properly weighted, so duplicate edges) graph
  igraph::graph_from_edgelist(directed = FALSE)

table(degree(n)) # divide by 2 for actual
sort(degree(n)[ degree(n) > 14 ]) # German x 2, USA, ITA, NOR

V(n)$hi_degree <- dplyr::if_else(degree(n) >= 8, V(n)$name, NA_character_)

ggplot(n, aes(x, y, xend = xend, yend = yend)) +
  geom_edges() +
  geom_nodes() +
  geom_nodelabel(aes(label = hi_degree)) +
  theme_blank()

ggsave("example-graph.png", width = 12, height = 10)

# kthxbye
