###define functions for qsub###

#' shortcut for memory requirement
#' @param x A numeric scalar.
#' @export
#' @examples
#' mem(5)
#' resource("-l", mem(5.3))
mem <- function(x) {
  assertthat::assert_that(length(x) == 1)
  vctrs::vec_cast(x, numeric()) -> x
  paste0("s_vmem=", x, "G,mem_req=", x, "G")
  }


#' resource requirement
#' @param ... Requirements. Each argument should be a character vector. Multiple arguments and multiple elements will be separated with a space.
#' @export
#' @examples
#' resource("-l", c("def_slot", "1"))
resource <- function(...) {
  inputs <- dots_parser(..., sep_collapse = " ")
  paste0("#$ ", inputs)
}

character_1_0 <- function(x){
  if (x == "") {character(0)} else {x}
}

#' Arrayjob requirement
#' @description Generates "-t" argument for "qsub".
#' @param n An integer which represents number of job-array. If n is 1, arrayjob is not required.
#' @param tc An integer. Max mumber of tasks executed simultaneously.
#' @param stepsize An integer(option). SGE_TASK_STEPSIZE option of "qsub -t".
#' @examples
#' arrayjob_option(10L)
#' @export
arrayjob_option <- function(n = 1L, tc = 100L, stepsize = NULL) {
  assertthat::assert_that(length(n) == 1)
  n <- vctrs::vec_cast(n, integer()) %>% as.character()
  if (n == "1") {
    ""
  }else{
    assertthat::assert_that(length(tc) == 1)
    tc <- vctrs::vec_cast(tc, integer()) %>% as.character()
    one2n <- paste0("1-", n)
    if (!is.null(stepsize)) {
      assertthat::assert_that(length(stepsize) == 1)
      stepsize <- vctrs::vec_cast(stepsize, integer())
      one2n <- paste0(one2n, ":", stepsize)
    }
    resource("-t", one2n, "-tc", tc)
  }
}

## @usage parallel_option(env = c("def_slot", "mpi", "mpi-fillup", "mpi_4", "mpi_8", "mpi_16", "mpi_24"), slot = 1L, memory = 5.3, master_memory = NULL, ljob = FALSE, no_rerun = TRUE, special_que = c(NULL, "cp", "docker", "knl", "gpu", "groupname"), docker_images = NA_character_)

#' Parallel job requirement
#' @description Generates parallel-job related arguments for "qsub".
#' @param env A character. Choose one of "def_slot", "mpi", "mpi-fillup", "mpi_4", "mpi_8", "mpi_16", "mpi_24".
#' @param slot An integer. The number of slots. If env is either "mpi" or "mpi-fillup", 2-length integer vector representing minimun and maximun nubmer of slots is also accepted.
#' @param memory A double. Memory requirement(Gb).
#' @param master_memory A double (option). Memory requirement for the master que. If \emph{slot} is 1 or \emph{master_memory} is equal to \emph{memory}, this argument will be ignored.
#' @param ljob A logical. Whether need to run more than 2 days. if you require more than 128Gb in total, this option is automatically set as FALSE.
#' @param no_rerun A logical. Whether allow to run on rerun ques.
#' @param special_que A character (option). Choose one of "cp", "docker", "knl", "gpu", "\emph{groupname}". If specified, ljob option will be ignored.
#' @param docker_images A character (option). Valid only if special_que == "docker".
#' @examples
#' parallel_option(slot = 4L, memory = 10, master_memory = 5, ljob = TRUE)
#' @export
parallel_option <- function(env = "def_slot", slot = 1L, memory = 5.3, master_memory = NULL, ljob = FALSE, no_rerun = TRUE, special_que = NULL, docker_images = NA_character_){
  tibble::tibble(ENV = c("def_slot", "mpi", "mpi-fillup", "mpi_4", "mpi_8", "mpi_16", "mpi_24"),
                 BASE_SLOT = c(1L, 1L, 1L, 4L, 8L, 16L, 24L)) -> resource_df
  if (!is.null(special_que)) {
    #assertthat::assert_that(length(special_que) == 1)
    assertthat::assert_that(special_que %in% c("cp", "docker", "knl", "gpu", "groupname"))
    if (special_que == "docker") {
      #assertthat::assert_that(length(docker_images) == 1)
      assertthat::assert_that(!is.na(docker_images))
      vctrs::vec_cast(docker_images, character()) -> docker_images
    }
  }
  #assertthat::assert_that(length(env) == 1)
  assertthat::assert_that(env %in% resource_df$ENV)
  if (env %in% c("mpi", "mpi-fillup")) {
    assertthat::assert_that(length(slot) %in% c(1, 2))
    if (length(slot) == 1) slot <- rep(slot, 2)
    dplyr::if_else(env == "mpi", 1, slot[2]) -> max_slot
  }else{
    assertthat::assert_that(length(slot) == 1)
    slot -> max_slot
  }
  vctrs::vec_cast(slot, integer()) -> slot
  resource_df %>%
    dplyr::mutate(SLOT_NODE = c(max_slot, max_slot, max_slot, 4L, 8L, 16L, 24L)) -> resource_df
  assertthat::assert_that(length(memory) == 1)
  vctrs::vec_cast(memory, numeric()) -> memory
  if (is.null(master_memory)) {
    master_memory <- memory
  }else{
    assertthat::assert_that(length(master_memory) == 1)
    vctrs::vec_cast(master_memory, numeric()) -> master_memory
  }
  vctrs::vec_cast(ljob, logical()) -> ljob
  vctrs::vec_cast(no_rerun, logical()) -> no_rerun

  resource_df %>%
    dplyr::filter(!!dplyr::sym("ENV") == env) -> df
  df$SLOT_NODE -> slot_node
  df$BASE_SLOT -> base_slot
  assertthat::assert_that(slot %% base_slot == 0)
  slot_node * memory + dplyr::if_else(slot_node > 1L, master_memory - memory, 0) -> total_memory
  if (stringr::str_detect(env, "^mpi")) max(total_memory, slot_node * memory) -> total_memory

  lmem <- FALSE
  special_resource <- ""

  if (!is.null(special_que)) {
    if (ljob)  {
      ljob <- FALSE
      rlang::warn("Special que request. ljob option was set as FALSE.", "requirement_resource_warning")
    }
    if (special_que == "cp") {
      max_slot <- 12L
      max_memory <- 128L
    }else if (special_que == "docker") {
      max_slot <- 12L
      max_memory <- 128L
      special_que <- paste0('docker,docker_images="', docker_images, '"')
    }else if (special_que == "knl") {
      max_slot <- 64L
      max_memory <- 112L
      if (env != "def_slot") rlang::abort("env must be def_slot when knl que is specified.", "requirement_resource_error")
    }else if (special_que == "gpu") {
      max_slot <- Inf #FIXME
      max_memory <- 1000L
      if (env != "def_slot") rlang::abort("env must be def_slot when gpu que is specified.", "requirement_resource_error")
    }else if (special_que == "groupname") {
      max_slot <- 12L
      max_memory <- 128L
      system("id", intern = TRUE) -> id
      assertthat::assert_that(id$status == 0)
      stringr::str_split(id$stdout, ("\\)|\\("))[[1]][4] %>% paste0(".q") -> groupque
      special_resource <- resource("-q", groupque)
    }
    if (slot_node > max_slot) rlang::abort("Large number of slot request.", "requirement_resource_error")
    if (total_memory > max_memory) rlang::abort("Large memory request.", "requirement_resource_error")
    if (special_que != "groupname") special_resource <- resource("-l", special_que)
    stringr::str_c(
      resource("-pe", env, slot),
      special_resource,
      resource("-l", mem(memory)),
      dplyr::if_else(slot > 1L && master_memory != memory,
                     resource("-masterl", mem(master_memory)),
                     "") %>% character_1_0(),
      sep = "\n"
    ) -> result
  }else{
    if (slot_node > 24L) {
      rlang::abort("number of slots must be equal to or less than 24 per node.", "requirement_resource_error")
    }
    if (slot_node > 12L) {
      if (ljob) {
        ljob <- FALSE
        rlang::warn("Large number of slots request. lmem option was selected instead of ljob option. Running time is allowed up to 2 weeks.", "requirement_resource_warning")
      }else{
        rlang::inform("Large number of slots request. lmem option was selected.", "requirement_resource_message")
      }
      lmem <- TRUE
    }
    if (total_memory > 2000L) {
      rlang::abort("total memory must be equal to or less than 2Tb per node.", "requirement_resource_error")
    }
    if (total_memory > 128L) {
      if (ljob) {
        ljob <- FALSE
        rlang::warn("Large memory request. lmem option was selected instead of ljob option. Running time is allowed up to 2 weeks.", "requirement_resource_warning")
      }else{
        rlang::inform("Large memory request. lmem option was selected.", "requirement_resource_message")
      }
      lmem <- TRUE
    }

    stringr::str_c(
      resource("-pe", env, slot),
      resource("-l", mem(memory)),
      dplyr::if_else(ljob, resource("-l", "ljob"), "") %>% character_1_0(),
      dplyr::if_else(no_rerun, resource("-q", "'!mjobs_rerun.q'"), "") %>% character_1_0(),
      dplyr::if_else(lmem, resource("-l", "lmem"), "") %>% character_1_0(),
      dplyr::if_else(slot > 1L && master_memory != memory,
                     resource("-masterl", mem(master_memory)),
                     "") %>% character_1_0(),
      dplyr::if_else(slot > 1L && master_memory != memory && ljob,
                    resource("-masterl", "ljob"),
                    "") %>% character_1_0(),
      dplyr::if_else(slot > 1L && master_memory != memory && no_rerun,
                     resource("-masterq", "'!mjobs_rerun.q'"),
                     "") %>% character_1_0(),
      dplyr::if_else(slot > 1L && master_memory != memory && lmem,
                     resource("-masterl", "lmem"),
                     "") %>% character_1_0(),
      sep = "\n"
    ) -> result
  }
  result
}

#' first spell for bash
#' @export
binbash <- function() {
  "#!/bin/bash\n#$ -S /bin/bash"
}

#' use ~/.bash_profile
#' @export
grov_env <- function(){
  paste0("source ", fs::path_expand("~/.bash_profile"))
}

convert_to_array <- function(x) {
  x[is.na(x)] <- "" #escape NA in order not to return NA
  stringr::str_c("[", 1:length(x), ']="', x, '"', collapse = " ")
}

is_bash_name <- function(x) {
  rlang::is_character(x) &
    stringr::str_detect(x, "^([:alnum:]|_)+$") &
    !stringr::str_detect(x, "^[:digit:]")
}

#' convert lists, vectors, tibbles into \emph{bash-array}
#' @description Vectors or each column of tibbles will be interpretted as a bash-array when your output is read by bash. NA will be changed to "".
#' @param ... Lists, vectors, or tibbles. Elements with the same name will be overwritten by the last one.
#' @param option An option for declare function of bash. This argument is used for all arguments.
#' @export
as_bash_array <- function(..., option = "-a") {
  assertthat::assert_that(length(option) == 1)
  vctrs::vec_cast(option, character()) -> option
  dots <- rlang::list2(...)
  dots %>%
    rlist::list.flatten() %>%
    purrr::iwalk(~ assertthat::assert_that(is_bash_name(.y))) %>%
    purrr::imap_chr(~ stringr::str_c("declare ", option, " ", .y, "=(", convert_to_array(.x), ")")) %>%
    stringr::str_c(collapse = "\n")
}
#TODO always require names. e.g. hello = c(1,2);as_bash_array(hello = hello).

#' Directory requirements
#' @param cwd A logical. Whether set the directory where you run your code as current working directory. Otherwise, your home directory is set as current working directory.
#' @param out Path to write stdout
#' @param err Path to write stderr (option). If unspecified, \emph{out} argument will be used instead.
#' @export
directory_option <- function(cwd = FALSE, out = fs::path_home(), err = NA_character_) {
  assertthat::assert_that(length(out) == 1)
  assertthat::assert_that(length(err) == 1)
  if (is.na(err)) err <- out
  resource("-o", fs::path_abs(out)) -> out
  resource("-e", fs::path_abs(err)) -> err
  dplyr::if_else(cwd, "#$ -cwd", "") -> cwd
  stringr::str_c(cwd, out, err, sep = "\n")
}
