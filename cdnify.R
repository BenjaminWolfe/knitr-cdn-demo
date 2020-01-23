# TODO: currently removes dependencies but not empty directories (low priority)
# TODO: could be useful to note when the version number is not as expected.

library(tidyverse)
library(here) # relative filepaths
library(glue) # string manipulation
library(fs)   # file system utilities

semver <- "(\\d+\\.\\d+\\.\\d+)"

references <- tibble(
  pattern = c(
    "jquery-{semver}/jquery.min.js",
    "bootstrap-{semver}/css/bootstrap.min.css",
    "bootstrap-{semver}/js/bootstrap.min.js",
    "bootstrap-{semver}/shim/html5shiv.min.js",
    "bootstrap-{semver}/shim/respond.min.js",
    "highlightjs-{semver}/default.css",
    "highlightjs-{semver}/highlight.js",
    "dt-core-{semver}/css/jquery.dataTables.min.css",
    "dt-core-{semver}/js/jquery.dataTables.min.js"
  ),
  expected_vsn = c(
    "1.12.4",
    "3.3.5",
    "3.3.5",
    "3.3.5",
    "3.3.5",
    "9.12.0",
    "9.12.0",
    "1.10.19",
    "1.10.19"
  ),
  cdn_url = c(
    "https://code.jquery.com/jquery-1.12.4.min.js",
    "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css",
    "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js",
    "https://oss.maxcdn.com/html5shiv/3.7.3/html5shiv.min.js",
    "https://oss.maxcdn.com/respond/1.4.2/respond.min.js",
    "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@9.18.0/build/styles/default.min.css",
    "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release@9.18.0/build/highlight.min.js",
    "https://cdn.datatables.net/1.10.20/css/jquery.dataTables.min.css",
    "https://cdn.datatables.net/1.10.20/js/jquery.dataTables.min.js"
  )
) %>% 
  mutate(
    pattern = pattern %>% 
      str_replace_all("\\.", "\\\\.") %>% 
      map_chr(glue)
  )

cdnify <- function(html_file) { # without .html extension
  
  fullpath <- paste0(html_file, ".html")
  page_contents <- read_lines(fullpath)
  
  get_semver <- function(dependency_regex) {
    
    dependency_line <- str_which(page_contents, dependency_regex)
    if (!length(dependency_line)) return(NA_character_)
    
    str_match(page_contents[dependency_line], dependency_regex)[1, 2]
  }
  
  dependency_dir <- paste0(basename(html_file), "_files")
  
  references <- 
    references %>% 
    mutate(
      pattern = paste(dependency_dir, pattern, sep = "/"),
      actual_vsn = map_chr(pattern, get_semver)
    )
  
  page_contents <- 
    reduce2(
      .x = references$pattern,
      .y = references$cdn_url,
      .f = str_replace,
      .init = page_contents
    )
  
  get_remaining <- function(page_contents) {
    pattern <- paste0(dependency_dir, "/([^\"]+)")
    remaining_lines <- str_which(page_contents, pattern)
    if (!length(remaining_lines)) return(NA_character_)
    
    str_match(page_contents[remaining_lines], pattern)[, 2]
  }
  
  keep_these <- 
    html_file %>% 
    path_dir() %>% 
    path(dependency_dir, get_remaining(page_contents))
  
  remove_these <- 
    html_file %>% 
    paste0("_files") %>% 
    dir_ls(recurse = TRUE, type = "file") %>% 
    setdiff(keep_these)
  
  write_lines(page_contents, fullpath)
  file_delete(remove_these)
  
  # message what happened
  message(Sys.time())
  message(fullpath)
  message("references changed:")
  print(select(references, pattern, expected_vsn, actual_vsn, cdn_url))
  message("files retained:")
  print(keep_these)
  message("files removed:")
  print(remove_these)
  
  # just for my blog post
  write_csv(
    select(references, pattern, expected_vsn, actual_vsn, cdn_url),
    here("notes/replacements.csv")
  )
  write_lines(keep_these, here("notes/files_kept.txt"))
  write_lines(remove_these, here("notes/files_removed.txt"))
}

pages <- 
  here() %>% 
  dir_ls(recurse = T, regexp = "\\.html") %>% 
  str_replace("\\.html$", "")

are_from_markdown <- file_exists(paste0(pages, ".Rmd"))
have_dependencies <- dir_exists(paste0(pages, "_files"))

walk(
  pages[are_from_markdown & have_dependencies],
  cdnify
)
