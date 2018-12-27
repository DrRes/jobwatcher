p_load_chr <- function(pkg) {
  paste0(
    dplyr::if_else(base::requireNamespace(pkg, quietly = TRUE),
                   "",
                   paste0("if (!requireNamespace('", pkg, "', quietly = TRUE)) install.packages('", pkg, "', )\n")),
    "library('", pkg, "')"
  )
}

p_load_github_chr <- function(pkg) {
  paste0(
    dplyr::if_else(base::requireNamespace("devtools", quietly = TRUE),
                   "", "if (!requireNamespace('devtools', quietly = TRUE)) install.packages('devtools')\n"),
    dplyr::if_else(base::requireNamespace(pkg, quietly = TRUE),
                   "",
                   paste0("if (!requireNamespace('", pkg, "', quietly = TRUE)) devtools::install_github('", pkg, "', )\n")),
    "library('", pkg, "')"
  )
}

p_ruler <- function(x) paste0("#", x, "=================================")

pipeline_preset <- function(pipe_name, pipe_dir, n_parallel, pipe_memory) {
  #assertthat::assert_that(is.integer(n_parallel))
  #assertthat::assert_that(is.numeric(pipe_memory))
  package_load <-
    paste0(
      #purrr::map_chr(c("tidyverse", "drake", "cowplot", "fs") ~ p_load_chr) %>%
      #  stringr::str_c(collapse = "\n"),
      #"\n",
      p_load_github_chr("jobwatcher")
    )

  pipe_dir <-
    fs::path_abs(pipe_dir)
  log_qrecall <-
    fs::path(pipe_dir, "log", "file_management")
  file_qrecall <-
    fs::path(pipe_dir, "config", "qrecall.txt")
  dir_output <-
    fs::path(pipe_dir, "output")
  log_output <-
    fs::path(pipe_dir, "log", "hello")
  file_helloworld <-
    fs::path(pipe_dir, "world", ext = "txt")
  log_pipeline <-
    fs::path(pipe_dir, "log")

  pipe_Rfile <-
    paste(sep = "\n",
          p_ruler("attach libraries"),
          package_load,
          "",
          p_ruler("declare variables"),
          "hello <- c('H', 'e', 'l', 'l', 'o')",
          "",
          p_ruler("parse config"),
          "#For config yaml files, we recommend fascinating {config} and {rlist} packages to parse them.",
          "",
          p_ruler("make directories"),
          paste0("fs::dir_create('", dir_output, "')"),
          paste0("fs::dir_create('", log_output, "')"),
          "",
          p_ruler("summarize in/out paths"),
          "your_qrecall_objects <- '/archive/data/hgc1043/snamba/.jobwatch/for_qrecall.txt'",
          "",
          p_ruler("intermediate file paths"),
          "",
          p_ruler("make functions of qscript files"),
          "qsub_watch <- purrr::compose(jobwatch, write_and_qsub)",
          paste0("dir_opt <- directory_option(out = '",log_output , "', err = '",log_output , "')"),
          "",
          "pl_makefile <- function(...) {",
          paste0("  qsub_watch('touch ",file_helloworld , "', name = 'makefile', directory = dir_opt)"),
          "}",
          "pl_hello <- function(...) {",
          "  qsub_watch(as_bash_array(hello),",
          paste0("             'echo ${hello[$SGE_TASK_ID]} > ", file_helloworld, "',"),
          "             name = 'hello',",
          "             arrayjob = arrayjob_option(length(hello)),",
          "             directory = dir_opt)",
          "}",
          paste0("pl_world <- function(...) qsub_watch('echo World > ", file_helloworld, "', name = 'world', directory = dir_opt)"),
          paste0("pl_helloworld <- function(...) qsub_watch('echo HelloWorld > ", file_helloworld, "', name = 'helloworld', directory = dir_opt)"),
          "",
          p_ruler("qrecall"),
          paste0("job_recall <- write_and_qrecall(your_qrecall_objects, path = '", file_qrecall, "', log_path = '", log_qrecall, "')"),
          paste0("jobwatch(job_recall, max_repeat = 1L)"),
          paste0("# if you set max_repeat >= 2, you should set additional arguments: qsub_args = '-o ", log_qrecall, "', qrecall = TRUE"),
          "",
          p_ruler("make pipeline"),
          "drake::drake_plan(",
          "  makefile = pl_makefile(),",
          "  hello = pl_hello(makefile),",
          "  world = pl_world(makefile),",
          "  helloworld = pl_helloworld(hello, world)",
          ") -> pipeline",
          "",
          p_ruler("run pipeline"),
          paste0("ggsave_pipeline(pipeline, '", fs::path(pipe_dir, "log","pipeline_pre.pdf"), "', width = 30, height = 10)"),
          paste0("drake::make(pipeline, jobs = ", n_parallel, "L)"),
          paste0("ggsave_pipeline(pipeline, '", fs::path(pipe_dir, "log","pipeline_post.pdf"), "', width = 30, height = 10)"),
          "")

  pipe_qsubfile <-
    make_qsubfile(
      paste0("Rscript ", fs::path(pipe_dir, pipe_name), ".R"),
      name = pipe_name,
      parallel = parallel_option(slot = n_parallel, memory = pipe_memory, ljob = TRUE),
      arrayjob = arrayjob_option(),
      directory = directory_option(out = log_pipeline, err = log_pipeline),
      use_bash_profile = TRUE
    )
  list(pipe_qsubfile, pipe_Rfile)
}

#' save a plan image
#'
#' @description wrapper of \code{drake::\link[drake]{drake_ggraph}} and \code{ggplot2::\link[ggplot2]{ggsave}}
#' @param plan A plan made with \code{drake::\link[drake]{make}}
#' @param path A path to write image
#' @param ... Additional arguments for \code{ggplot2::\link[ggplot2]{ggsave}}
#' @export
ggsave_pipeline <- function(plan, path, ...){
  g <- plan %>% drake::drake_config() %>% drake::drake_ggraph() + ggplot2::theme_void()
  ggplot2::ggsave(path, g, ...)

}

#' build a pre-build pipeline
#'
#' @param pipe_name A character. Your pipeline name.
#' @param pipe_dir A directory path where you intend to build a pipeline.
#'  If it does not exist, it will be made recursively.
#' @param n_parallel A integer. The number you run elements of your pipeline simultaneously.
#' @param pipe_memory A numeric. Request of memory size par slot for pipeline manager.
#' @param force Alogical. Whether to run this function when \emph{pipe_dir} exists and \emph{make_subdir} is TRUE.
#' @param make_subdir A logical. Whether to make minimum subdirectories under \emph{pipe_dir}
#' @export
build_pipeline <- function(pipe_name, pipe_dir, n_parallel = 2L, pipe_memory = 3L, force = FALSE, make_subdir = TRUE){
  file_list <- pipeline_preset(pipe_name, pipe_dir, n_parallel, pipe_memory)
  dir_exist <- fs::dir_exists(pipe_dir)
  if ((!force) && make_subdir && dir_exist) {
    rlang::abort(paste0(pipe_dir, "exists. If you'd like to continue, set force = TRUE."))
  }
  if (!dir_exist) {
    fs::dir_create(pipe_dir)
    done("Directory '", crayon::cyan(pipe_dir), "' has been created.")
  }
  if (make_subdir) {
    purrr::map(list("log", "script", "config"), ~ fs::path(pipe_dir, .x)) %>%
      purrr::walk(fs::dir_create) %>%
      purrr::walk(~ done("Directory '", crayon::cyan(.x), "' has been created."))
  }
  path_list <- purrr::map(c("sh", "R"), ~ fs::path(pipe_dir, pipe_name, ext = .x))
  purrr::walk2(path_list, file_list, ~ write(.y, .x, append = FALSE)) %>%
    purrr::walk(~ done("File '", crayon::cyan(.x), "' has been written."))
  todo("Please edit '", crayon::cyan(path_list[[2]]), "' for your own pipeline.")
  todo("Then, run ", crayon::green(paste0("qsub ", path_list[[1]])))
}

