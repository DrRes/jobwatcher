dots_parser <- function(..., sep_collapse = "\n") {
  dots <- rlang::list2(...)
  dots <- purrr::map(dots, vctrs::vec_cast, to = character())
  dots %>% purrr::map_chr(stringr::str_c, collapse = sep_collapse) %>% stringr::str_c(collapse = sep_collapse)
}

#' make a file suitable for \emph{qsub}
#' @param ... Your codes (default: \emph{bash} codes). Each argument should be a character vector. Multiple arguments and multiple elements will be separated with a line break.
#' @param name A character
#' @param first_line A character. It is written in the first line.
#' @param parallel A character
#' @param arrayjob A character
#' @param directory A character
#' @param use_bash_profile A logical. Whether \emph{source ~/.bash_profile} or not.
#' @param other_req A character. Other requirements for \emph{qsub}
#' @seealso \url{https://supcom.hgc.jp/internal/mediawiki/qsub_%E3%82%B3%E3%83%9E%E3%83%B3%E3%83%89}
make_qsubfile <- function(...,
                          name = "pipeline_child",
                          first_line = binbash(),
                          parallel = parallel_option(),
                          arrayjob = arrayjob_option(),
                          directory = directory_option(),
                          use_bash_profile = TRUE,
                          other_req = character(0)){#TODO docker file home directory check
  dots_parser(...) -> inputs
  list(name, first_line, parallel, arrayjob, directory, other_req) %>%
    purrr::walk(~ assertthat::assert_that(rlang::is_character(.x)))
  stringr::str_c(
    first_line,
    parallel,
    arrayjob,
    resource("-N", name),
    directory,
    other_req,
    dplyr::if_else(use_bash_profile, grov_env(), ""),
    "##########",
    inputs,
    sep = "\n")
}

verify_path <- function(path, recursive) {
  if (recursive) {
    assertthat::assert_that(is.character(path))
    path %>% fs::path_dir() %>% fs::dir_create()
  }else{
    assertthat::assert_that(path %>% fs::path_dir() %>% fs::dir_exists())
  }
}

# @return invisible. A list of the path you write your file to actually, and the time you execute this function.
write_job <- function(x, path, recursive, add_time) {
  assertthat::assert_that(is.character(x))
  verify_path(path, recursive)
  Sys.time() %>% format("%Y%m%d%H%M") -> time
  if (add_time) stringr::str_c(path, "_", time) -> path
  write(x, path, append = F)
  invisible(list(path, time))
}

#' write out and \emph{qsub}
#'
#' @param x A character. contents of file.
#' @param path A character. The path to write a file.
#' @param recursive A logical. Whether make parent directory recursively when it does NOT exist.
#' @param add_time A logical. Whether add the time you execute this function to path for unique naming.
#' @param qsub_args Additional arguments for \emph{qsub}.
#' @seealso \url{https://supcom.hgc.jp/internal/mediawiki/qsub_%E3%82%B3%E3%83%9E%E3%83%B3%E3%83%89}
#' @return Invisible. A list of Job ID, the path you write your file to, and the time you execute this function.
#' @export
write_and_qsub <- function(x, path, recursive = FALSE,
                           add_time = TRUE, qsub_args = "") {
  time <- character()
  c(path, time) %<-% write_job(x, path, recursive, add_time)
  assertthat::assert_that(is.character(qsub_args))
  qsubres <- processx::run(stringr::str_c("qsub ", path), qsub_args)
  message(qsubres$stdout)
  stringr::str_split(qsubres$stdout, " ")[[1]][3] -> ID
  invisible(list(ID, path, time))
}

#' write out and \emph{qrecall}
#' @param ... Paths to recall. Each argument should be a character vector. Multiple arguments and multiple elements will be separated with a line break.
#' @param path A character. The path to write a file for \emph{qrecall}.
#' @param log_path A character (optional). The path to write a stdout of \emph{qrecall}. Default, home directory.
#' @param recursive A logical. Whether make parent directory recursively when it does NOT exist.
#' @param add_time A logical. Whether add the time you execute this function to path for unique naming.
#' @return Invisible. A list of Job ID, the path you write your file to, and the time you execute this function.
#' @export
write_and_qrecall <- function(..., path = fs::path_home(), log_path = NULL, recursive = FALSE, add_time = TRUE) {
  inputs <- dots_parser(...)
  time <- character()
  c(path, time) %<-% write_job(inputs, path, recursive, add_time)
  if (!is.null(log_path)) verify_path(log_path, recursive)
  arg_stdout <- dplyr::if_else(is.null(log_path), "", paste0(" -o ", log_path))
  qrec <- processx::run(paste0("qrecall -file ", path, arg_stdout))
  message(qrec$stdout)
  stringr::str_split(qrec$stdout, " ")[[1]][3] -> ID
  invisible(list(ID, path, time))
}

seq_int_chr <- function(from, to, by){
  dplyr::if_else(is.na(from) || is.na(to) || is.na(by),
                 "undefined",
                 seq.int(from, to, by) %>% as.character())
}

#' \emph{qsub} a file
#' @param path A character. The path to a \emph{qsub/qrecall} file.
#' @param qsub_args A character. Additional arguments for \emph{qsub/qrecall}.
#' @param qrecall A logical. Whether use qrecall -file instead of qsub.
#' @return Invisible. A list of Job ID, the path you write your file to, and the time you execute this function.
#' @export
qsub <- function(path, qsub_args = "", qrecall = FALSE){
  assertthat::assert_that(fs::file_exists(path))
  assertthat::assert_that(is.character(qsub_args))
  time <- format(Sys.time(), "%Y%m%d%H%M")
  command <- dplyr::if_else(qrecall, "qrecall -file ", "qsub ")
  qsubres <- processx::run(paste0(command, path), args = qsub_args)
  message(qsubres$stdout)
  stringr::str_split(qsubres$stdout, " ")[[1]][3] -> ID
  invisible(list(ID, path, time))
}
