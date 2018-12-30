vacant_tbl <- function(colname){
  matrix(nrow = 0, ncol = length(colname)) %>%
    tibble::as_tibble() %>%
    dplyr::mutate_all(as.character) %>%
    `colnames<-`(colname)
}

try_xml_to_tbl <- function(xml){
  tbl_colnames <- c("JB_owner","JB_job_number",
                    "taskid","slots","JB_pe_id","granted_pe",
                    "exit_status","failed","queue_name","host_name",
                    "JB_name","JAT_qsub_time","JAT_start_time","JAT_end_time",
                    "ru_wallclock","cpu","memory","maxvmem","r_mem","r_q","r_cpu",
                    "qdel","failed_txt","recommended_queue","recommended_memory","recommended_option")
  vacant_result <- vacant_tbl(tbl_colnames)
  tryCatch(
    {
      xml %>% XML::xmlToDataFrame(stringsAsFactors = F) %>% tibble::as_tibble() -> tbl
      dplyr::setdiff(tbl_colnames, colnames(tbl)) -> missing_colnames
      purrr::reduce(missing_colnames, ~ dplyr::mutate(.x, !!.y := NA_character_), .init = tbl) -> tbl
      tbl %>% dplyr::select(!!!tbl_colnames)
    },
    error = function(e) vacant_result,
    warning = function(e) vacant_result
  )
}

#' get \emph{qreport} results in xml fromat
#'
#' @param ID Job ID.
#' @param begin A character of \strong{\%Y\%m\%d\%H\%M} format.
#' @param user Your user ID. (optional)
#' @export
qreport_xml <- function(ID, begin, user = NA_character_){#ID must NOT be array type.
  purrr::map(list(ID, begin, user), ~ assertthat::assert_that(length(.x) == 1))
  c(ID, begin, user) %<-% purrr::map(list(ID, begin, user), vctrs::vec_cast, character())
  user_option <- dplyr::if_else(is.na(user), "", paste0(" -o ", user))
  suppressWarnings(system(
    paste0("qreport -j ", ID, user_option, " -b ", begin, " -x"),
    intern = TRUE
  ) -> result)
  result
}

qsub_verbose <- function(ID_body, task, time){
  stringr::str_glue("\nID: ", crayon::cyan(ID_body), 
       "\ntaskid: ", crayon::cyan(stringr::str_c(task, collapse = ", ")),
       "\ntime: ", crayon::cyan(time))
}

#' get \emph{qreport} results as a tibble
#'
#' @inheritParams qreport_xml
#' @export
qreport_tbl <- function(ID, begin, user = NA_character_){#ID must NOT be array type.
  qreport_xml(ID, begin, user) %>% try_xml_to_tbl()
}

#' watch a \emph{qsub} job via \emph{qreport}
#'
#' @param x A list of your job ID, the path of your qsub file, and the time you execute qsub or time before that.
#' @param sys_sleep A numeric. \emph{qreport} interval in seconds.
#' @param max_repeat A integer. Total times of trying \emph{qsub} the same file.
#' @param qsub_args A character. Additional arguments for \emph{qsub/qrecall}.
#' @param qrecall A logical. Whether use \emph{qrecall -file} instead of \emph{qsub} when re-subbing your job.
#' @param verbose A logical.
#' @param debug A logical.
#' @return Invisible. A list of your final job ID, the path of your qsub file, and the time of final qsub.
#' @export
jobwatch <- function(x, sys_sleep = 60L, max_repeat = 2L, qsub_args = "", qrecall = FALSE, verbose = FALSE, debug = FALSE){
  #perse ID and time
  if (debug) {verbose <- TRUE}
  x %>%
    purrr::walk(~ assertthat::assert_that(length(.x) == 1)) %>%
    purrr::map(vctrs::vec_cast, character()) -> x
  ID = path = time = NA_character_
  c(ID, path, time) %<-% x
  assertthat::assert_that(!is.na(ID))
  assertthat::assert_that(fs::file_exists(path))
  assertthat::assert_that(stringr::str_length(time) == 12)
  ID_vec <- stringr::str_split(ID, "\\.|-|:")[[1]] %>% as.integer()
  ID_body <- ID_vec[1]
  task <- ID_vec[2:4] %>% seq_int_chr()
  if (verbose) {
    todo(crayon::green(path))
    qsub_verbose(ID_body, task, time)
    }
  counter <- 0
  user = fs::path_home() %>% fs::path_file() %>% as.character()
  while (TRUE) {
    Sys.sleep(sys_sleep)
    rep <- qreport_tbl(ID_body, time, user)
    if (debug) {
      print("qreport: ")
      print(rep)
      }
    rep %>%
      tidyr::replace_na(list(failed_txt = "")) %>%
      dplyr::filter(!!sym("failed_txt") != "Rescheduling") -> rep_filt
    if (nrow(rep_filt) > 0) {
      rep_filt %>% dplyr::mutate_at(dplyr::vars("exit_status", "failed"), as.integer) -> rep_filt
      rep_filt %>% dplyr::filter(!!sym("taskid") %in% task) -> rep_filt
      if (debug) {
        print("filtered: ")
        print(rep_filt)
        print(paste0("setdiff: ", stringr::str_c(dplyr::setdiff(task, rep$taskid), collapse = ", ")))
        print(paste0("sum: ", sum(rep_filt$exit_status, rep_filt$failed)))
        }
      if (identical(dplyr::setdiff(task, rep$taskid), character(0))) {
        if (sum(rep_filt$exit_status, rep_filt$failed) == 0) {
          if (debug) as.data.frame(rep) %>% print()#debug
          rlang::inform(done("'", crayon::cyan(path), "' has been done."))
          if (verbose) done("'", crayon::cyan(path), "' has been done.") #message and print
          break
        }else{
          counter <- counter + 1
          if (counter < max_repeat) {
            c(ID, path, time) %<-% qsub(path, qsub_args, qrecall)
            ID_vec <- stringr::str_split(ID, "\\.|-|:")[[1]] %>% as.integer()
            ID_body <- ID_vec[1]
            task <- ID_vec[2:4] %>% seq_int_chr()
            if (verbose) qsub_verbose(ID_body, task, time)
          }else{
            if (verbose) as.data.frame(rep) %>% print()#debug
            rlang::abort(paste0("'", path, "' has something to cause error or fail."), "qsub_contents_error")
          }
        }
      }
    }
  }
  invisible(list(ID, path, time))
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
  c(
    list(...),
    list(script_path = SCRIPT_PATH, script_dir = SCRIPT_DIR, name = NAME, first_line = FIRST_LINE, parallel = PARALLEL, arrayjob = ARRAYJOB, directory = DIRECTORY, 
         use_bash_profile = USE_BASH_PROFILE, other_req = OTHER_REQ, recursive = RECURSIVE, add_time = ADD_TIME, qsub_args = QSUB_ARGS)
  ) -> args_list
  
  function(dammy_arg){
    do.call(write_and_qsub, args_list) -> jobs
    do.call(jobwatch, c(list(x = jobs), jobwatch_args))
  }
}

# qrecall files and watch progress
# @export
#qrecall_watch <- purrr::compose(purrr::partial(jobwatch, max_repeat = 1L, qrecall = TRUE), write_and_qrecall)
