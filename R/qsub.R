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
#' @export
make_qsubfile <- function(...,
                          name = NA_character_,
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
    dplyr::if_else(is.na(name), "", resource("-N", name)) %>% character_1_0(),
    directory,
    dplyr::if_else(use_bash_profile, grov_env(), "") %>% character_1_0(),
    other_req,
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
  if (add_time) {
    ext <- fs::path_ext(path)
    if(ext == ""){
      stringr::str_c(fs::path_ext_remove(path), "_", time) -> path
    }else{
      stringr::str_c(fs::path_ext_remove(path), "_", time, ".", fs::path_ext(path)) -> path
    }
  }
  write(x, path, append = F)
  invisible(list(path, time))
}

#' save and \emph{qsub}
#'
#' @param x A character. contents of file.
#' @param script_path A character. The path to write a file.
#' @param script_dir A character. It will concatenated with file_path..
#' @param recursive A logical. Whether make parent directory recursively when it does NOT exist.
#' @param add_time A logical. Whether add the time you execute this function to path for unique naming.
#' @param qsub_args Additional arguments for \emph{qsub}.
#' @seealso \url{https://supcom.hgc.jp/internal/mediawiki/qsub_%E3%82%B3%E3%83%9E%E3%83%B3%E3%83%89}
#' @return Invisible. A list of Job ID, the path you write your file to, and the time you execute this function.
#' @export
save_and_qsub <- function(x, script_path, script_dir, recursive = FALSE,
                           add_time = TRUE, qsub_args = "") {
  time <- character()
  path <- dplyr::if_else(is.na(script_dir), script_path, fs::path(script_dir, script_path) %>% as.character()) %>% fs::path_abs()
  c(path, time) %<-% write_job(x, path, recursive, add_time)
  assertthat::assert_that(is.character(qsub_args))
  qsubres <- system(paste0("qsub ", path, " ", qsub_args), intern = TRUE)
  message(qsubres)
  stringr::str_split(qsubres, " ")[[1]][3] -> ID
  invisible(list(ID, path, time))
}

#' write and \emph{qsub}
#' 
#' @description shorthand of \code{\link{save_and_qsub}}(\code{\link{make_qsubfile}}())
#' @param ... Your codes (default: \emph{bash} codes). Each argument should be a character vector. Multiple arguments and multiple elements will be separated with a line break.
#' @param script_path A character. The path to write a file.
#' @param script_dir A character. It will concatenated with file_path..
#' @param name A character
#' @param first_line A character. It is written in the first line.
#' @param parallel A character
#' @param arrayjob A character
#' @param directory A character
#' @param use_bash_profile A logical. Whether \emph{source ~/.bash_profile} or not.
#' @param other_req A character. Other requirements for \emph{qsub}
#' @param recursive A logical. Whether make parent directory recursively when it does NOT exist.
#' @param add_time A logical. Whether add the time you execute this function to path for unique naming.
#' @param qsub_args Additional arguments for \emph{qsub}.
#' @seealso \url{https://supcom.hgc.jp/internal/mediawiki/qsub_%E3%82%B3%E3%83%9E%E3%83%B3%E3%83%89}
#' @return Invisible. A list of Job ID, the path you write your file to, and the time you execute this function.
#' @export
write_and_qsub <- function(...,
                          script_path, 
                          script_dir = NA_character_,
                          name = NA_character_,
                          first_line = binbash(),
                          parallel = parallel_option(),
                          arrayjob = arrayjob_option(),
                          directory = directory_option(),
                          use_bash_profile = TRUE,
                          other_req = character(0),
                          recursive = FALSE,
                          add_time = TRUE,
                          qsub_args = ""){
  NAME = FIRST_LINE = PARALLEL = ARRAYJOB = DIRECTORY = USE_BASH_PROFILE = OTHER_REQ = SCRIPT_PATH = SCRIPT_DIR = RECURSIVE = ADD_TIME = QSUB_ARGS = NA_character_
  c(NAME, FIRST_LINE, PARALLEL, ARRAYJOB, DIRECTORY, USE_BASH_PROFILE, OTHER_REQ, SCRIPT_PATH, SCRIPT_DIR, RECURSIVE, ADD_TIME, QSUB_ARGS) %<-% 
    list(name, first_line, parallel, arrayjob, directory, use_bash_profile, other_req, script_path, script_dir, recursive, add_time, qsub_args)
  make_qsubfile(..., name = NAME, first_line = FIRST_LINE, parallel = PARALLEL, arrayjob = ARRAYJOB, directory = DIRECTORY, use_bash_profile = USE_BASH_PROFILE, other_req = OTHER_REQ) %>% 
    save_and_qsub(script_path = SCRIPT_PATH, script_dir = SCRIPT_DIR, recursive = RECURSIVE, add_time = ADD_TIME, qsub_args = QSUB_ARGS)
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
  qrec <- system(paste0("qrecall -file ", path, arg_stdout), intern = TRUE)
  #cannot use process::run for unknown reasons
  message(qrec)
  stringr::str_split(qrec, " ")[[1]][3] -> ID
  invisible(list(ID, path, time))
}

seq_int_chr <- function(from_to_by){
  from = to = by = integer()
  c(from, to, by) %<-% (from_to_by %>% vctrs::vec_cast(integer()))
  if(is.na(from) || is.na(to) || is.na(by)) {
    "undefined"
  }else{
    seq.int(from, to, by) %>% as.character()
  }
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
  qsubres <- system(paste0(command, path, " ", qsub_args), intern = TRUE)
  rlang::inform(qsubres)
  stringr::str_split(qsubres, " ")[[1]][3] -> ID
  invisible(list(ID, path, time))
}
