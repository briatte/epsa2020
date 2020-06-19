library(rvest)
library(tidyverse)

dir.create("abstract", showWarnings = FALSE)

# u <- paste0(
#   "https://www.epsanet.org/programme/search/?zoom_query=&zoom_and=0",
#   "&presenters=&pres_type=-1&room=-1&date=", 0:1, "&zoom_sort=0#rslt"
# )
#
# for (i in u) {
#   paste0("index", which(u == i) - 1, ".html") %>%
#     download.file(i, ., mode = "wb", quiet = TRUE)
# }

# Using index pages downloaded manually.
# Thanks to Stefan Müller for the URLs:
# https://twitter.com/ste_mueller/status/1272874116333346816
for (i in list.files("search-results", pattern = "^jun", full.names = TRUE)) {

  u <- read_html(i) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_subset("abstract_\\d+\\.html")

  cat(basename(i), ":", length(u), "URLs...")

  for (j in u) {

    str_replace(basename(j), "_", "/") %>%
      download.file(j, ., mode = "wb", quiet = TRUE)

  }
  cat("\n")

}

# kthxbye
