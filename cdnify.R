# See README.md for usage.
# TODO: currently removes dependencies but doesn't tidy up empty directories.
# TODO: could be useful to asterisk when the version number is not as expected.

# load packages, installing if not present
load_packages <- function(packages) {
  
  on.exit(message("")) # buffer line after output
  
  # function to attempt loading, silently
  try_loading <- function(target_packages) {
    sapply(
      target_packages,
      function(package) {
        suppressPackageStartupMessages(
          require(
            package,
            quietly        = TRUE,
            warn.conflicts = FALSE,
            character.only = TRUE
          )
        )
      }
    )
  }
  
  # full report: what packages were required?
  message("")
  message(
    length(packages),
    " required R packages: ",
    paste(
      packages,
      collapse = " "
    )
  )
  
  # attempt to load all
  # if all succeeded, easy message.
  loaded_first <- try_loading(packages)
  if (all(loaded_first)) {
    message(
      "loaded all ",
      length(loaded_first), ": ",
      strrep(" ", 9), # spaces optional, to align with previous message
      paste0(
        packages,
        collapse = " "
      )
    )
    message("")
    message("finished loading: ", Sys.time())
    return(invisible())
  }
  
  # install remaining packages & message the installation attempts
  install_these <- packages[!loaded_first]
  install.packages(install_these)
  message("")
  message(
    "installed or attempted to install ",
    length(install_these), ": ",
    paste(
      install_these,
      collapse = " "
    )
  )
  message("")
  message("finished installing packages / attempting installs: ", Sys.time())
  
  # reattempt loading & message any successes from either attempt
  loaded_second <- try_loading(install_these)
  if (any(loaded_first) || any(loaded_second)) {
    message("")
    message(
      "loaded ",
      sum(loaded_first, na.rm = T) + sum(loaded_second, na.rm = T), ": ",
      paste(
        c(packages[loaded_first], install_these[loaded_second]),
        collapse = " "
      )
    )
    message("")
    message("finished loading: ", Sys.time())
  }
  
  # message any failures from second try
  if (any(!loaded_second)) {
    message("")
    message(
      "failed to load ",
      sum(!loaded_second, na.rm = T), ": ",
      paste(
        install_these[!loaded_second],
        collapse = " "
      )
    )
  }
  
  invisible()
}

message("")
message("stripping out unnecessary CSS & JS libraries...")
message("")
message("started script: ", Sys.time())

load_packages(
  c(
    "tibble",
    "dplyr",
    "stringr",
    "purrr",
    "readr",
    "glue",
    "here",
    "fs"
  )
)

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

# takes the path for a single html file, ideally without the ".html" extension
# style note: considered using fs::path(a, b) instead of paste(a, b, sep = "/")
# but html files generally use / regardless of OS; path may (?) use \ on windows
cdnify <- function(html_file) {
  
  html_file     <- html_file %>% str_replace("\\.html$", "") # just in case
  fullpath      <- paste0(html_file, ".html")
  page_contents <- read_lines(fullpath)
  
  # returns the semantic version for a given dependency (expressed as regex)
  get_semver <- function(dependency_regex) {
    
    dependency_line <- str_which(page_contents, dependency_regex)
    n_lines <- length(dependency_line)
    
    # expect only one reference to a given dependency?
    if (n_lines > 1) stop(dependency_regex, " occurs ", n_lines, " times")
    if (!n_lines) return(NA_character_)
    
    str_match(page_contents[dependency_line], dependency_regex)[1, 2]
  }
  
  # get the parent directory name for any references in the html file
  # if html_file is /home/benjamin/project/pagename, yields "pagename_files"
  dependency_dir <- paste0(path_file(html_file), "_files")
  
  # construct regex patterns for the full relative filepath
  # for reference, get the semantic version for each; useful for troubleshooting
  references <- 
    references %>% 
    mutate(
      pattern = paste(dependency_dir, pattern, sep = "/"),
      actual_vsn = map_chr(pattern, get_semver)
    )
  
  # actually replace internal references with content delivery network URLs
  page_contents <- 
    reduce2(
      .x = references$pattern,
      .y = references$cdn_url,
      .f = str_replace,
      .init = page_contents
    )
  
  # returns any remaining internal files still being referred to
  get_remaining <- function(page_contents) {
    
    # pattern is dependency_dir, then / and anything before the quotation mark
    # parenthetical capture group will capture everything past the /
    pattern <- paste0(dependency_dir, "/([^\"]+)")
    
    remaining_lines <- str_which(page_contents, pattern)
    if (!length(remaining_lines)) return(NA_character_)
    
    str_match(page_contents[remaining_lines], pattern)[, 2]
  }
  
  # get a vector of files that need to be retained (still used by the html file)
  keep_these <- 
    html_file %>% 
    path_dir() %>% 
    path(dependency_dir, get_remaining(page_contents))
  
  # any remaining libraries can now be removed
  remove_these <- 
    html_file %>% 
    paste0("_files") %>% 
    dir_ls(recurse = TRUE, type = "file") %>% 
    setdiff(keep_these)
  
  # actually rewrite the file contents and remove dead libraries
  write_lines(page_contents, fullpath)
  file_delete(remove_these)
  
  # message what happened, for troubleshooting
  patterns  <- references %>% pull(pattern)
  changes   <- references %>% 
    filter(!is.na(actual_vsn)) %>% 
    select(pattern, expected_vsn, actual_vsn, cdn_url)
  
  n_changed <- nrow(changes)
  n_kept    <- length(keep_these)
  n_removed <- length(remove_these)
  
  message("reviewed file: ", fullpath)
  message("finished at:   ", Sys.time())
  message("")
  
  message("checked for ", length(patterns), " patterns: ")
  print(patterns)
  message("")
  
  message(n_changed, " references externalized", if (n_changed) ":")
  if(n_changed) print(changes)
  message("")
  
  # keep_these is of class "fs_path"
  # cast to "character" before printing for the usual numbering ([1], [2], etc.)
  message(n_kept, " files retained", if (n_kept) ":")
  if (n_kept) print(as.character(keep_these))
  message("")
  
  message(n_removed, " files removed", if (n_removed) ":")
  if (n_removed) print(remove_these)
  message("")
}

# get current directory; depends whether coding or running pre-commit
current_dir <- 
  if (interactive()) {
    here()
  } else {
    as.character(commandArgs(trailingOnly = T)[1])
  }

# get all html pages
pages <- 
  current_dir %>% 
  dir_ls(recurse = T, regexp = "\\.html") %>% 
  str_replace("\\.html$", "")

# check whether they are from R markdown + have dependencies in the first place
are_from_markdown <- file_exists(paste0(pages, ".Rmd"))
have_dependencies <- dir_exists(paste0(pages, "_files"))
pages <- pages[are_from_markdown & have_dependencies]

# externalize dependencies for each, and message user
n_pages <- length(pages)
if (!n_pages) {
  message("no html files to check (from R Markdown, with libraries)")
  message("")
} else {
  message("checking ", n_pages, " html file", if (n_pages > 1) "s", ":")
  print(paste0(pages, ".html"))
  message("")
  message("started checking: ", Sys.time())
  message("")
  walk(pages, cdnify)
  
  message("finished checking ", n_pages, " page", if (n_pages > 1) "s", ":")
  print(paste0(pages, ".html"))
  message("")
}
