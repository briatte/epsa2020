library(rvest)
library(tidyverse)

dir.create("abstract", showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)

# -- Web scraping index pages won't work easily (JavaScript) -------------------

# u <- paste0(
#   "https://www.epsanet.org/programme/search/?zoom_query=&zoom_and=0",
#   "&presenters=&pres_type=-1&room=-1&date=", 0:1, "&zoom_sort=0#rslt"
# )
#
# for (i in u) {
#   paste0("index", which(u == i) - 1, ".html") %>%
#     download.file(i, ., mode = "wb", quiet = TRUE)
# }

# -- using index pages downloaded manually -------------------------------------

# Thanks to Stefan Müller for the URLs:
# https://twitter.com/ste_mueller/status/1272874116333346816

for (i in list.files("search-results", pattern = "^jun", full.names = TRUE)) {

  u <- read_html(i) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_subset("abstract_\\d+\\.html")

  cat(basename(i), ":", length(u), "URLs ...")

  for (j in rev(u)) {

    f <- str_replace(basename(j), "_", "/")

    if (!file.exists(f)) {
      download.file(j, f, mode = "wb", quiet = TRUE)
    }

    d <- which(j == u)
    if (!d %% 10) {
      cat("", d, "...")
    }

  }
  cat("\n")

}

# -- parse abstracts -----------------------------------------------------------

f <- list.files("abstract", pattern = "^\\d", full.names = TRUE)
d <- tibble()

cat("Parsing", length(f), "abstracts...")

for (i in f) {

  h <- read_html(i)

  # # detailed metadata
  # html_nodes(h, "meta") %>%
  #   map_chr(html_attr, "content")

  d <- tibble(
    panel = html_nodes(h, xpath = "//a[contains(@href, 'session')]") %>%
      html_attr("href"),
    abstract = i,
    authors = html_nodes(h, "meta[name='authors']") %>%
      html_attr("content"),
    affiliations = html_nodes(h, "meta[name='affiliations']") %>%
      html_attr("content")
  ) %>%
    bind_rows(d)

}

# authors have extra spaces, but are clean otherwise
# str_subset(d$authors, "\\.")
d$authors <- str_squish(d$authors)

# numeric identifiers but stored as character; `abstract` is 4-padded
d$panel <- str_remove_all(basename(d$panel), "\\D")
d$abstract <- str_remove_all(basename(d$abstract), "\\D")

cat("\n")

readr::write_tsv(arrange(d, abstract), "data/abstracts.tsv")

# -- simplify affiliations -----------------------------------------------------

d$affiliation_raw <- d$affiliations %>%
  # remove spec. chars and numbers that might hinder `str_split` that follows
  # (damages postal codes and a few other things: see e.g. [TOFIX] below)
  str_remove_all("\\\n|\\\t|\\\r") %>%
  str_split("(,\\s)?\\d\\.\\s") %>%
  # lose empty "" strings
  purrr::map(str_subset, "\\w+")

a <- select(d, abstract, affiliation_raw) %>%
  unnest(affiliation_raw)

# (1) pre-process
str_prepare <- function(x) {
  x %>%
    # ambiguous UCx
    str_replace("University of California, Merced", "UC Merced") %>%
    str_replace("University of California, Los Angeles", "UCLA")
}

# (2) take first part, removing (mostly) cities
str_clean <- function(x) {
  # first part before comma
  str_extract(x, ".*?,") %>%
    # remove 'university' keywords
    str_remove_all("^The\\s|Univ.*\\s|(\\sUniversity)?,$") %>%
    # get rid of useless punctuation
    str_remove_all("\\.")
}

# (3) fix and shorten
# [NOTE]
# - Composed against preliminary programme data, then appended with, strings
#   from the final one; some lines might not be useful anymore.
# - Includes affiliations from single authors, even though they are never used
#   in edge construction.
str_fix <- function(x) {
  x %>%
    str_replace("ASCoR", "Amsterdam") %>%
    str_replace("Johns Hopkins", "JHU") %>%
    str_replace("New York", "NYU") %>%
    str_replace("European Institute", "EUI") %>%
    str_replace("London School of Economics( and Political Science)?", "LSE") %>%
    str_replace("Department of Government", "LSE") %>%
    str_replace("Hertie School(Berlin's Berlin)?", "Hertie") %>%
    str_replace("George Washington", "GWU") %>%
    str_replace("Tokyo Science", "TUS Tokyo") %>%
    str_replace("^IAST$|Institute for Advanced Study in Toulouse", "IAST Toulouse") %>%
    str_replace("Science Po", "Sciences Po") %>%
    # different departments
    str_replace("UCSD\\s.*|^Diego$", "UCSD") %>%
    str_replace("Institut Barcelona d'Estudis Internacionals", "IBEI Barcelona") %>%
    str_replace("Institute of Political Science", "Zurich") %>%
    str_replace("Columbia Science", "Columbia") %>%
    str_replace("Division of Social Science", "NYU Abu Dhabi") %>%
    str_replace("^Koc$", "Koç") %>%
    str_replace("Central European", "CEU") %>%
    str_replace("2\\)$", "Paris 2") %>%
    str_replace("Paris.*Sorbonne$", "Paris 1") %>%
    str_replace("Department of Social and Political Sciences", "Bocconi") %>%
    str_replace("School of International Development", "UEA") %>%
    str_replace("Vienna Institute of Demography.*", "VID Vienna") %>%
    str_replace("Queen Mary London", "QMUL") %>%
    str_replace("Simon Fraser", "SFU") %>%
    str_replace("WZB Berlin Social Science Center", "WZB") %>%
    str_replace("(BI )?Norwegian Business School", "NBS Oslo") %>%
    str_replace("VATT Institute for Economic Research", "VATT Helsinki") %>%
    str_replace("Texas A&M \\(USA\\)", "Texas A&M") %>%
    str_replace("Trinity College Dublin", "TCD") %>%
    str_replace("^Centre$", "Exeter") %>%
    str_replace("Humboldt.*Berlin", "Humboldt") %>%
    str_replace("Mannheim Centre for European Social Research.*", "Mannheim") %>%
    str_replace("Department of Politics and International RelationsOxford", "Oxford") %>%
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
    str_replace("German Development Institute", "GDI Bonn") %>%
    str_replace("HHL Leipzig.*", "HHL Leipzig") %>%
    str_replace("Instituto de Ciências Sociais", "Lisbon") %>%
    str_replace("Institute of Social Sciences.*Lisbon", "Lisbon") %>%
    str_replace("Cologne Center for Comparative Politics", "Cologne") %>%
    str_replace("Institute for Advanced Studies", "IAS Vienna") %>%
    str_replace("Graduate Institute of International.*Studies \\(IHEID\\)", "IHEID") %>%
    str_replace("Institutions and Political Economy.*Barcelona", "Barcelona") %>%
    str_replace("^International Institute for Applied Systems Analysis \\(IIASA\\)", "IIASA")
}

# apply steps (1, 2, 3)
a$affiliation_clean <- str_prepare(a$affiliation_raw) %>%
  str_clean() %>%
  str_fix()

# sanity check
stopifnot(!is.na(a$affiliation_clean))

# longest strings
order(str_length(a$affiliation_clean), decreasing = TRUE) %>%
  a$affiliation_clean[ . ] %>%
  head(15)

# review full data

# distinct(select(a, -abstract)) %>%
#   arrange(affiliation_clean) %>%
#   View()

readr::write_tsv(arrange(a, abstract, affiliation_raw), "data/affiliations.tsv")

# -- final notes about the data ------------------------------------------------

# number of authors
table(str_count(d$authors, ",") + 1) # range 1--6

# 1. There are a few (n = 8) cases where two affiliations (never more) are
#    attached to a same person; we later build those as edges, even though they
#    do not really stand for a co-authorship ties (weird self-ties, perhaps).

str_extract_all(d$authors, "\\d,\\d") %>%
  unlist() %>%
  table()

# 2. The preliminary programme contained a few 'ghost' affiliations that were
#    listed but not attached to any listed author (an example was affiliation
#    no. 2 in abstract 0002). The issue seems gone in the final programme data.

# 3. There are a few odd cases:
#    - abstract 131, affiliation is PSE & Paris 1, gets simplified to latter
#    - abstract 134, affiliation is Nuffield & Yale, gets simplified to former

# kthxbye
