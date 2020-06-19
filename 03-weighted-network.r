library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr) # for unnest
library(sjmisc) # for is_empty
library(tnet)
library(igraph)


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

# affiliations
a <- tibble(ids = stringr::str_extract_all(d$authors, "\\d"), affils = d$affiliations)

# keep only rows with multiples affiliations
a <- a[ which(purrr::map_int(a$ids, length) > 0), ]

#------ f functions -----------------------------------------------------------------------------------

str_clean <- function(x) {
  # first part before comma
  str_extract(x, ".*?,") %>%
    str_remove_all("Univ.*\\s|(\\sUniversity)?,$")
}

# export (unclean)
#readr::write_tsv(e, "edges.tsv")

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
    str_replace("Center for Research and Social Progress", "CRSP Ponte dell'Olio")
}
#----- back to the data -------------------------------------------------------------------------------------

# edge list
e <- a$affils %>%
  # remove spec chars and numbers that might hinder the `str_split` that follows
  str_remove_all("\\\n|\\\t|\\\r|\\d{2,}\\s") %>%
  str_split("\\d\\s") %>%
  tibble() %>%
  rename(affils = '.') %>%
  mutate(artid = row_number()) %>% 
  unnest(cols = affils)  %>% #keep_empty = F , name_repair = "unique"
  filter(!is_empty(affils, first.only = F)) 
  
# export (unclean)
readr::write_tsv(e, "bipartite.tsv")
  
e$affils <- str_fix(str_clean(str_prepare(e$affils)))

# export (append clean)
readr::read_tsv("bipartite.tsv", col_types = "cc") %>%
  tibble::add_column(affils_clean = e$affils, .before = 2) %>% #
  readr::write_tsv("bipartite.tsv")

## add an unique ID from 1 to n for each unique affiliation involved

e <- e  %>% 
  distinct(affils, artid, .keep_all= TRUE) %>%
  group_by(affils) %>%   
  mutate(affils_id = cur_group_id())%>%   
  ungroup() 

net <- select(e, e = affils_id, p = artid)

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

V(g)$name <- as.character(e$affils[match(V(g)$name,e$affils_id)])
V(g)$label <- V(g)$name # If necessary add "str_to_title"

sum(E(g)$weight) # in case, you use the Netscity method, it would give you the number of articles (66!)
                 # with the Newman method, the edges' sum gives you 80.5

flows <- bind_cols(get.edgelist(g),W_collab = E(g)$weight) %>%  
         rename(Affil_i = "...1", Affil_j = "...2", )

# export the flow table

write_tsv(flows, "flows.tsv")

