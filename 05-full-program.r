library(tidyverse)
library(rvest)

f <- fs::dir_ls("html/abstracts", glob = "*.html")

d <- map(f, read_html) %>%
  map_dfr(
    ~ tibble::tibble(
      # panels
      session_id = html_node(.x, "meta[name='session_id']") %>%
        html_attr("content"),
      # strictly identical to meta name = 'session_full_title'
      session_title = html_node(.x, "meta[name='full_title']") %>%
        html_attr("content"),
      # [NOTE] those columns hold nothing of interest
      # session_short_title = html_node(.x, "meta[name='session_short_title']") %>%
      #  html_attr("content"),
      # session_type = html_node(.x, "meta[name='session_type']") %>%
      #  html_attr("content"),
      # participants
      # [NOTE] missing affiliations for chairs...
      # [NOTE] missing affiliations for moderators...
      chairs1 = html_node(.x, "meta[name='chairs']") %>%
        html_attr("content"),
      # occasionally, chairs appear in HTML only, e.g. abstract_0038
      chairs2 = html_node(.x, xpath = "//div[starts-with(text(), 'Panel Chair') or starts-with(text(), 'Panel chair')]") %>%
        html_text2(preserve_nbsp = TRUE),
      # occasionally, moderators appear in HTML only, e.g. abstract_0084
      mods1 = html_node(.x, "meta[name='moderators']") %>%
        html_attr("content"),
      mods2 = html_node(.x, xpath = "//div[starts-with(text(), 'Moderator:')]") %>%
        html_text(trim = TRUE),
      authors = html_node(.x, "meta[name='authors']") %>%
        html_attr("content"),
      affiliations = html_node(.x, "meta[name='affiliations']") %>%
        html_attr("content"),
      # abstracts
      abstract_title = html_node(.x, "meta[name='abstracttitle']") %>%
        html_attr("content"),
      # [NOTE] actual `paper_ref` meta tag contains identifiers that repeat
      # over panels, and so are useless to uniquely identify papers
      abstract_ref = html_node(.x, "meta[name='ID']") %>%
        html_attr("content"),
      # always 'Oral presentation'
      # paper_type = html_node(.x, "meta[name='pres_type']") %>%
        # html_attr("content"),
      #
      # [NOTE] meta tag is incomplete: presenter is always a single individual,
      #        even when there is more than one underlined author, and is always
      #        the first one of those (e.g. abstract_0032, abstract_0033)
      # abstract_presenters = html_node(.x, "meta[name='presenters']") %>%
        # html_attr("content"),
      # correct extractor
      abstract_presenters = html_nodes(.x, "div.authors u") %>%
        html_text() %>%
        str_c(collapse = ", "),
      abstract_text = html_node(.x, ".abstracttext") %>%
        html_text(trim = TRUE)
    ),
    .id = "abstract_id"
  ) %>%
  mutate(
    # authors have extra spaces but are clean otherwise
    authors = str_squish(authors),
    abstract_presenters = str_squish(abstract_presenters),
    # same goes for affiliations and abstracts, where there are just a few \r\n
    affiliations = str_squish(affiliations),
    abstract_text = str_squish(abstract_text),
    # `abstract_id` is 4-padded
    abstract_id = str_remove_all(basename(abstract_id), "\\D"),
    chairs1 = dplyr::na_if(chairs1, ""),
    mods1 = dplyr::na_if(mods1, ""),
    # in 1 case, `chairs2` has an affiliation ("Elias Dinas, University of Oxford"),
    # but we remove it and hope to get it from the authors instead (we do)
    chairs2 = str_remove_all(chairs2, ".*:\\s|\\r\\n.*|,\\s.*"),
    mods2 = str_remove_all(mods2, ".*:\\s|\\r.*")
  )

# sanity checks
stopifnot(!is.na(d$session_id))
stopifnot(!duplicated(d$abstract_id))
stopifnot(!duplicated(d$abstract_ref))

# separate session titles and tracks --------------------------------------

# `session_title` sometimes repeats, in which case it should be `session_track`
group_by(d, session_title) %>%
  summarise(n_sessions = n_distinct(session_id)) %>%
  mutate(what = if_else(n_sessions == 1, "title", "track")) %>%
  group_by(what) %>%
  group_split()

d <- group_by(d, session_title) %>%
  mutate(
    session_track = n_distinct(session_id),
    session_track = if_else(session_track == 1, NA_character_, session_title)
  ) %>%
  relocate(session_track, .after = "session_title")

# collapse chairs and moderators into a single column ---------------------

# never both `chairs1` and `chairs2`
stopifnot((is.na(d$chairs1) + is.na(d$chairs2)) >= 1)

# never both `mods1` and `mods1`
stopifnot((is.na(d$chairs1) + is.na(d$chairs2)) >= 1)

# replace
d <- d %>%
  mutate(
    chair = if_else(is.na(chairs1), chairs2, chairs1),
    mod = if_else(is.na(mods1), mods2, mods1)
  )

# never both `chairs` and `mods`
stopifnot((is.na(d$chair) + is.na(d$mod)) >= 1)

# replace, and remove unused columns
d <- d %>%
  mutate(chair = str_squish(if_else(is.na(chair), mod, chair))) %>%
  select(-chairs1, -chairs2, -mods1, -mods2, -mod)

# no missing chairs
stopifnot(!length(d$abstract_id[ is.na(d$chair) ]))

# [NOTE] older raw data from 2020 had no chair for the following abstracts
# "0090" "0098" "0106" "0111" "0113" "0116" "0122" "0128"

# match authors and affiliations ------------------------------------------

d$authors <- d$authors %>%
  map(
    # split on comma-space (never found in names or affiliation numbers)
    ~ str_split(.x, ",\\s") %>%
      unlist() %>%
      as_tibble_col(column_name = "author") %>%
      mutate(
        aid = str_extract(author, "\\(.*?\\)") %>%
          str_split(",") %>%
          map(unlist) %>%
          map(str_remove_all, "\\D") %>%
          map(as.integer)
      ),
  ) %>%
  map(~ unnest(.x, aid)) %>%
  map(~ mutate(.x, aid = if_else(is.na(aid), 1L, aid)))

d$affiliations <- d$affiliations %>%
  map(
    ~ str_split(.x, "\\d\\.\\s?") %>%
      unlist() %>%
      str_subset("\\w{1,}") %>%
      as_tibble_col(column_name = "affiliation") %>%
      # re-add numbers (affiliations are always in numeric order)
      add_column(aid = 1:nrow(.), .before = 1),
  )

d$matched <- map2(d$authors, d$affiliations, ~ full_join(.x, .y, by = "aid"))

d <- tidyr::unnest(d, matched) %>%
  select(-authors, -affiliations, -aid) %>%
  # lose single case of a lone affiliation that wasn't matched to an author
  filter(!is.na(author)) %>%
  # clean some punctuation
  mutate(
    author = str_remove(author, "\\s\\(.*?\\)"),
    affiliation = str_remove(affiliation, ",\\s?$")
  ) %>%
  # collapse multiple affiliations
  group_by(abstract_id, author) %>%
  mutate(affiliation = str_flatten(affiliation, collapse = " && ")) %>%
  # remove duplicated rows
  distinct() %>%
  ungroup()

# n = 16 authors with multiple affiliations
sum(str_detect(d$affiliation, "&&"))

# sanity check: all abstract presenters are present in abstract authors
y <- unique(d$abstract_presenters) %>%
  str_split(",\\s") %>%
  unlist() %>%
  unique()

stopifnot(y %in% d$author)

# [NOTE] that happens because presenters are 'clipped' to the first presenter;
#        things might change if we parse presenters better

# check whether chairs exist as authors -----------------------------------

# chairs
y <- str_split(d$chair, ",\\s") %>%
  unlist() %>%
  unique() %>%
  sort()

# n = 22 cases for which affiliations cannot be retrieved from authors
# n = 7 cases where that's possible
table(y %in% unique(d$author))

# authors with multiple affiliations --------------------------------------

# n = 10 (5 cases with 2 affiliations each), very manually fixable
d %>%
  distinct(author, affiliation) %>%
  arrange(author) %>%
  group_by(author) %>%
  mutate(n_affiliations = n_distinct(affiliation)) %>%
  filter(n_affiliations > 1)

d$affiliation[ d$author %in% "Federica Izzo" ] <- "University of California San Diego, San Diego"
d$affiliation[ d$author %in% "Jon Fiva" ] <- "BI Norwegian Business School, Oslo"
d$affiliation[ d$author %in% "Michael Becher" ] <- "Institute for Advanced Study in Toulouse, Toulouse"
d$affiliation[ d$author %in% "Nathalie Giger" ] <- "University of Geneva, Geneva"
d$affiliation[ d$author %in% "Stefan Müller" ] <- "University of Zurich, Zurich && University College Dublin, Dublin"

# reformat ----------------------------------------------------------------

d <- bind_rows(
  # chairs
  select(d, starts_with("session_"), full_name = chair) %>%
    distinct() %>%
    # handle the single case with two chairs
    # filter(str_detect(full_name, ","))
    mutate(full_name = str_split(full_name, ",\\s")) %>%
    tidyr::unnest(full_name) %>%
    # get affiliations from authors rows when possible, `NA` otherwise
    left_join(distinct(d, author, affiliation), by = c("full_name" = "author")) %>%
    add_column(role = "c"),
  # authors
  select(d, -chair, full_name = author) %>%
    add_column(role = "p")
) %>%
  arrange(session_id, role)

# add missing affiliations ------------------------------------------------

# TODO: further affiliation fixes (which will modify the participant UIDs)

# initialize
filter(d, is.na(affiliation)) %>%
  distinct(full_name, affiliation) %>%
  arrange(full_name) %>%
  readr::write_tsv("data/affiliation-problems.tsv", na = "")

# use completed version to fill in missing values
d <- read_tsv("data/affiliation-fixes.tsv", col_types = "cc") %>%
  left_join(d, ., by = "full_name") %>%
  mutate(affiliation = if_else(is.na(affiliation.x), affiliation.y, affiliation.x)) %>%
  select(-affiliation.x, -affiliation.y) %>%
  relocate(affiliation, .after = "full_name")

# sanity checks
stopifnot(!is.na(d$full_name))
stopifnot(!is.na(d$affiliation))

# create unique participant identifiers -----------------------------------

# [NOTE] should be reproducible using R >= 3.5.0, serialization version 3
#        see ?rlang::hash -- 128-bit hashes

p <- distinct(d, full_name, affiliation) %>%
  add_column(conference = "epsa2020", .before = 1) %>%
  mutate(
    # `affiliation` is actually never missing here, so `str_replace_na` is
    # used only as a precaution here
    text = str_c(conference, full_name, str_replace_na(affiliation)),
    # create 32-length UIDs
    hash = map_chr(text, rlang::hash)
  )

# sanity checks: no duplicates
stopifnot(!duplicated(p$text))
stopifnot(!duplicated(p$hash))

# add hashes to master data
d <- select(p, full_name, pid = hash) %>%
  left_join(d, ., by = "full_name") %>%
  relocate(pid, .before = full_name)

# sanity check: no missing pid
stopifnot(!is.na(d$pid))

# export ------------------------------------------------------------------

stopifnot(!duplicated(d))

cat(
  n_distinct(d$session_id), "sessions,",
  n_distinct(d$abstract_id), "abstracts,",
  n_distinct(d$pid), "unique participant IDs,",
  n_distinct(d$full_name), "unique names.\n"
)

readr::write_tsv(d, "data/program.tsv")

# that was pretty epic
# kthxbye
