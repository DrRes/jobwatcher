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
#' @param timeout waiting time for \emph{qreport}, in seconds, or as a difftime object.
#' @export
qreport_xml <- function(ID, begin, user = NA_character_, timeout = Inf){#ID must NOT be array type.
  TIMEOUT <- timeout
  purrr::map(list(ID, begin, user), ~ assertthat::assert_that(length(.x) == 1))
  c(ID, begin, user) %<-% purrr::map(list(ID, begin, user), vctrs::vec_cast, character())
  user_option <- dplyr::if_else(is.na(user), "", paste0(" -o ", user))
  processx::run(
    paste0("qreport -j ", ID, user_option, " -b ", begin, " -x"),
    timeout = TIMEOUT
  ) -> result
  result$stdout
}

#' get \emph{qreport} results as a tibble
#'
#' @inheritParams qreport_xml
#' @export
qreport_tbl <- function(ID, begin, user = NA_character_, timeout = Inf){#ID must NOT be array type.
  qreport_xml(ID, begin, user, timeout) %>% try_xml_to_tbl()
}

#' watch a \emph{qsub} job via \emph{qreport}
#'
#' @param x A list of your job ID, the path of your qsub file, and the time you execute qsub or time before that.
#' @param sys_sleep A numeric. \emph{qreport} interval in seconds.
#' @param max_repeat A integer. Total times of trying \emph{qsub} the same file.
#' @param qsub_args A character. Additional arguments for \emph{qsub/qrecall}.
#' @param qrecall A logical. Whether use \emph{qrecall -file} instead of \emph{qsub} when re-subbing your job.
#' @param verbose A logical.
#' @param ... Other paramaters for \code{\link{qreport_tbl}}. \strong{user} and \strong{timeout} can be specified.
#' @return Invisible. A list of your final job ID, the path of your qsub file, and the time of final qsub.
#' @export
jobwatch <- function(x, sys_sleep = 60L, max_repeat = 2L, qsub_args = character(), qrecall = FALSE, verbose = FALSE, ...){
  #perse ID and time
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
  counter <- 0
  while (TRUE) {
    Sys.sleep(sys_sleep)
    rep <- qreport_tbl(ID, time, ...)
    rep %>%
      tidyr::replace_na(list(failed_txt = "")) %>%
      dplyr::filter(!!sym("failed_txt") != "Rescheduling") -> rep_filt
    if (nrow(rep_filt) > 0) {
      rep_filt %>% dplyr::mutate_at(dplyr::vars("exit_status", "failed"), as.integer) -> rep_filt
      rep_filt %>% dplyr::filter(!!sym("taskid") %in% task) -> rep_filt
      if (dplyr::setdiff(task, rep$taskid) == character(0)) {
        if (sum(rep_filt$exit_status, rep_filt$failed) == 0) {
          if (verbose) as.data.frame(rep) %>% print()#debug
          rlang::inform(paste0("'", path, "' has been done."))
          break
        }else{
          counter <- counter + 1
          if (counter < max_repeat) {
            c(ID, path, time) %<-% qsub(path, qsub_args, qrecall)
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

# qsub a job and watch progress
# @export
#qsub_watch <- purrr::compose(jobwatch, write_and_qsub)

# qrecall files and watch progress
# @export
#qrecall_watch <- purrr::compose(purrr::partial(jobwatch, max_repeat = 1L, qrecall = TRUE), write_and_qrecall)
