

#' watch a \emph{qsub}bed job by using \emph{qreport}
#'
#' @param ID Your job ID or job name
#' @param path  A path of your qsub file (optional). If unspecified, max_repeat will be set as 0.
#' @param time A character of \strong{\%Y\%m\%d\%H\%M} format. The time you execute qsub or time before that (optional).
#' @param sys_sleep A numeric. \emph{qreport} interval in seconds
#' @param max_repeat A integer. Total times of trying \emph{qsub} the same file.
#' @param give_up One of "error", "warning", "message". Default is "error".
#' @param qsub_args A character. Additional arguments for re-\emph{qsub}/re-\emph{qrecall}. Arguments written in the original file will be ignored.
#' @param modify_req A logical. When re-qsubbing, whether to add recommended requests to qsub_args
#' @param as_qrecall A logical. Whether use \emph{qrecall -file} instead of \emph{qsub} when re-subbing your job.
#' @param verbose A logical.
#' @param debug A logical.
#' @return Invisible. A list of your final job ID, the path of your qsub file, and the time of final qsub.
#' @export
watch <- function(ID, path = NA, time = NA,
                  sys_sleep = 60L, max_repeat = 2L, 
                  give_up = c("error", "warning", "message"),
                  qsub_args = "", modify_req = TRUE,
                  as_qrecall = FALSE,
                  verbose = FALSE, debug = FALSE){
  verify_hgc()
  #perse ID and time
  if (debug) {verbose <- TRUE}
  verify_scalar(ID, path, time)
  verify_no_na(ID)
  c(ID, path, time) %<-% .map_as_character(ID, path, time)
  assertthat::assert_that(fs::file_exists(path))
  assertthat::assert_that(is.na(time) || stringr::str_length(time) == 12)
  assign_oneof_default_arg_chr("give_up")
  
  give_up_fun <- 
    switch(give_up,
           "error" = rlang::abort,
           "warning" = rlang::warn,
           "message" = rlang::inform,
           rlang::abort("function select error", "unexpected_error")
           )
  qsub_qrecall <- ifelse(as_qrecall, qrecall, qsub)
  
  ID_body = task = NULL
  c(ID_body, task) %<-% parse_id(ID)
  if (verbose) {
    todo(crayon::green(path))
    qsub_verbose(ID_body, task, time)
  }
  
  counter <- 0
  user <- get_user_name()
  while (TRUE) {
    Sys.sleep(sys_sleep)
    rep <- qreport(ID_body, time, user, type = "tibble")
    if (debug) {
      print("qreport: ")
      print(rep)
      }
    rep %>%
      tidyr::replace_na(list(failed_txt = "")) %>%
      dplyr::filter(!!sym("failed_txt") != "Rescheduling") -> rep_filt
    if (nrow(rep_filt) > 0) {
      rep_filt <-
        rep_filt %>% 
        dplyr::mutate_at(dplyr::vars("exit_status", "failed"), as.integer) %>% 
        dplyr::filter(!!sym("taskid") %in% task)
      if (debug) {
        print("filtered: ")
        print(rep_filt)
        print(paste0("setdiff: ", stringr::str_c(dplyr::setdiff(task, rep$taskid), collapse = ", ")))
        print(paste0("sum: ", sum(rep_filt$exit_status, rep_filt$failed)))
        }
      if (identical(dplyr::setdiff(task, rep$taskid), character(0))) {
        if (sum(rep_filt$exit_status, rep_filt$failed) == 0) {
          if (debug) print(as.data.frame(rep))#debug
          message(paste0("'", path, "' has been done.")) #message->stderr, inform->stdout
          if (verbose) rlang::inform(done("'", crayon::cyan(path), "' has been done.")) #message and print
          break
        }else{
          counter <- counter + 1
          if (verbose) {
            fail("The job with",
                 "\n ID: ", crayon::cyan(ID_body),
                 "\n path: ", crayon::cyan(path),
                 "\nhas failed.")
            as.data.frame(rep) %>% print()
            }#debug
          if (counter < max_repeat) {
            qsub_args_new <- qsub_args
            if (modify_req) {
              qsub_args_new <- paste0(qsub_args, " ", rep_filt$recommended_option[1])
              if (qsub_args == "" || length(qsub_args) == 0) {
                c(path, time) %<-%
                  write_job(c(readr::read_lines(path), qsub_args_new), path, recursive = TRUE, add_time = TRUE)
                qsub_args_new <- qsub_args 
               }
            }
            c(ID, path, time) %<-% qsub_qrecall(path, qsub_args_new)
            if (modify_req) {
              message(paste0("#", counter, " resub: ", path, "\nadditional args: ", qsub_args_new))
            }else{
              message(paste0("#", counter, " resub: ", path))
            }
            c(ID_body, task) %<-% parse_id(ID)
            if (verbose) {
              if (modify_req) {
                rlang::inform(todo("#", counter, " resub: ", crayon::cyan(path), "\nadditional args: ", qsub_args_new))
              }else{
                rlang::inform(todo("#", counter, " resub: ", crayon::cyan(path)))
              }
              qsub_verbose(ID_body, task, time)
              }
          }else{
            give_up_fun(paste0("'", path, "' has something to cause error or fail."), "qsub_contents_error")
          }
        }
      }
    }
  }
  invisible(list(ID = ID, path = path, time = time))
}

#' make function for qsub a job and watch progress
#' 
#' @description short hand of creating a function 
#'   doing fixed \code{\link{write_and_qsub}} and \code{\link{jobwatch}}
#'   regardless arguments of created function.
#'
#' @inheritParams write_and_qsub
#' @param jobwatch_args A list. Elements are passed to \code{\link{jobwatch}}
#' @export
qsub_function <- function(...,
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
                          qsub_args = "", 
                          jobwatch_args = list()){
  NAME = FIRST_LINE = PARALLEL = ARRAYJOB = DIRECTORY = USE_BASH_PROFILE = OTHER_REQ = SCRIPT_PATH = SCRIPT_DIR = RECURSIVE = ADD_TIME = QSUB_ARGS = NA_character_
  c(NAME, FIRST_LINE, PARALLEL, ARRAYJOB, DIRECTORY, USE_BASH_PROFILE, OTHER_REQ, SCRIPT_PATH, SCRIPT_DIR, RECURSIVE, ADD_TIME, QSUB_ARGS) %<-% 
    list(name, first_line, parallel, arrayjob, directory, use_bash_profile, other_req, script_path, script_dir, recursive, add_time, qsub_args)

  function(dammy_arg){
    write_and_qsub(...,
                   script_path = SCRIPT_PATH, script_dir = SCRIPT_DIR, name = NAME, first_line = FIRST_LINE, parallel = PARALLEL, arrayjob = ARRAYJOB, directory = DIRECTORY, 
                   use_bash_profile = USE_BASH_PROFILE, other_req = OTHER_REQ, recursive = RECURSIVE, add_time = ADD_TIME, qsub_args = QSUB_ARGS
                   ) -> jobs
    do.call(jobwatch, c(list(x = jobs), jobwatch_args))
  }
}

# qrecall files and watch progress
# @export
#qrecall_watch <- purrr::compose(purrr::partial(jobwatch, max_repeat = 1L, qrecall = TRUE), write_and_qrecall)
