verify_scalar <- function(...) purrr::walk(list(...), ~ assertthat::assert_that(assertthat::is.scalar(.x)))
verify_no_na <- function(x) assertthat::assert_that(assertthat::noNA(x))
verify_in <- function(x, y) assertthat::assert_that(all(x %in% y), msg = paste0(deparse(x), " is NOT included in ", deparse(y), "."))
verify_file_exists <- function(x) assertthat::assert_that(fs::file_exists(x))
  
verify_path <- function(path, recursive) {
  if (recursive) {
    assertthat::assert_that(is.character(path))
    fs::dir_create(fs::path_dir(path))
  }else{
    assertthat::assert_that(fs::dir_exists(fs::path_dir(path)))
  }
}

verify_hgc <- function() {
  assertthat::assert_that(get_jobwatcher_mode() == "hgc", msg = "This function works only in HGC super computer.")
}
verify_uge <- function() {
  assertthat::assert_that(get_jobwatcher_mode() %in% c("hgc", "uge"), msg = "This function works only in UGE environment.")
}

.as_character <- function(x) vctrs::vec_cast(x, character())
.map_as_character <- function(...) purrr::map(rlang::list2(...), .as_character)

is_number <- function(x) stringr::str_detect(x, "^[:digit:]+$")

assign_oneof_default_arg_chr <- function(arg_vec) {
  default_args <- formals(sys.function(sys.parent()))
  arg_vec <- arg_vec[arg_vec %in% names(default_args)]
  purrr::walk(arg_vec, ~ {
    true_arg <- .as_character(eval(as.name(..1), envir = sys.frame(1L)))[1L]
    verify_in(true_arg, eval(default_args[[..1]]))
    assign(..1, true_arg, envir = sys.frame(1L))
  })
  invisible(arg_vec)
}

