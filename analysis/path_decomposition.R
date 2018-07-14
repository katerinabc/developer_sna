standardize_path <- function (x = dir(), sep = c("/", "\\"), include_names = TRUE) 
{
  if (is_empty(x)) {
    if (include_names) {
      return(setNames(character(), character()))
    }
    else {
      return(character())
    }
  }
  sep <- match.arg(sep)
  x <- original_x <- coerce_to(x, "character")
  ok <- is_non_missing_nor_empty_character(x)
  is_slashless_windows_drive <- stri_detect_regex(x[ok], "^[a-zA-Z]:$")
  x[ok][is_slashless_windows_drive] <- paste0(x[ok][is_slashless_windows_drive], 
                                              "/")
  x[ok] <- ifelse(is.na(x[ok]), NA_character_, normalizePath(x[ok], 
                                                             "/", mustWork = FALSE))
  x[ok] <- ifelse(stri_detect_regex(x[ok], "^\\\\\\\\"), paste0("\\\\", 
                                                                stri_replace_all_fixed(substring(x[ok], 3), "\\", "/")), 
                  stri_replace_all_fixed(x[ok], "\\", "/"))
  if (is_unix()) {
    has_leading_double_slash <- stri_detect_regex(x[ok], 
                                                  "^/{2}")
    x[ok][has_leading_double_slash] <- substring(x[ok][has_leading_double_slash], 
                                                 2)
  }
  if (is_unix()) {
    is_absolute <- stri_detect_regex(x[ok], "^([/\\\\]|[a-zA-Z]:)")
    # x[ok][!is_absolute] <- file.path(getwd(), x[ok][!is_absolute], 
    #                                  fsep = "/")
  }
  is_root <- stri_detect_regex(x[ok], "^(/|[a-zA-Z]:[/\\\\]?)$")
  x[ok][!is_root] <- stri_replace_first_regex(x[ok][!is_root], 
                                              "/?$", "")
  needs_a_slash <- stri_detect_regex(x[ok][is_root], "[^/]$")
  x[ok][is_root][needs_a_slash] <- paste0(x[ok][is_root][needs_a_slash], 
                                          "/")
  x[ok] <- paste0(toupper(substring(x[ok], 1, 1)), substring(x[ok], 
                                                             2))
  x <- stri_replace_all_fixed(x, "/./", "/")
  x <- stri_replace_all_regex(x, "/[^/]+/\\.\\./", "/")
  if (sep == "\\") {
    x[ok] <- stri_replace_all_fixed(x[ok], "/", "\\")
  }
  if (include_names) {
    setNames(x, original_x)
  }
  else {
    unname(x)
  }
}

decompose_path_modified <- function (x = vector_names) 
{
  
  original_x <- x <- as.character(x)
  x <- standardize_path(x) # this introduces my folder structture
  not_missing <- strip_attributes(is_not_na(x))
  is_dir_x <- is_dir(x)
  basename_x <- ifelse(not_missing, ifelse(is_dir_x, "", basename(x)), 
                       NA_character_)
  has_extension <- stri_detect_fixed(basename_x, ".")
  rx <- "^([]\\[[:alnum:] `!@#$%^&()_=+{},.;'-]+?)\\.([[:alnum:].]+)$"
  filename_x <- ifelse(not_missing, basename_x, NA_character_)
  extension_x <- ifelse(not_missing, "", NA_character_)
  not_missing_and_has_extension <- not_missing & has_extension
  if (any(not_missing_and_has_extension)) {
    split_name <- stri_match_first_regex(basename_x[not_missing_and_has_extension], 
                                         rx)
    filename_x[not_missing_and_has_extension] <- split_name[, 
                                                            2L]
    extension_x[not_missing_and_has_extension] <- split_name[, 
                                                             3L]
  }
  decomposed_x <- data.frame(dirname = ifelse(not_missing, 
                                              ifelse(is_dir_x, x, standardize_path(dirname(x))), NA_character_), 
                             filename = filename_x, extension = extension_x, row.names = ifelse(is.na(original_x), 
                                                                                                "<NA>", original_x), stringsAsFactors = FALSE)
  structure(decomposed_x, class = c("decomposed_path", "data.frame"))
}