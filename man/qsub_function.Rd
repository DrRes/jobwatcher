% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jobwatch.R
\name{qsub_function}
\alias{qsub_function}
\title{make function for qsub a job and watch progress}
\usage{
qsub_function(..., script_path, script_dir = NA_character_,
  name = NA_character_, first_line = binbash(),
  parallel = parallel_option(), arrayjob = arrayjob_option(),
  directory = directory_option(), use_bash_profile = TRUE,
  other_req = character(0), recursive = FALSE, add_time = TRUE,
  qsub_args = "", jobwatch_args = list())
}
\arguments{
\item{...}{Your codes (default: \emph{bash} codes). Each argument should be a character vector. Multiple arguments and multiple elements will be separated with a line break.}

\item{script_path}{A character. The path to write a file.}

\item{script_dir}{A character. It will concatenated with file_path..}

\item{name}{A character}

\item{first_line}{A character. It is written in the first line.}

\item{parallel}{A character}

\item{arrayjob}{A character}

\item{directory}{A character}

\item{use_bash_profile}{A logical. Whether \emph{source ~/.bash_profile} or not.}

\item{other_req}{A character. Other requirements for \emph{qsub}}

\item{recursive}{A logical. Whether make parent directory recursively when it does NOT exist.}

\item{add_time}{A logical. Whether add the time you execute this function to path for unique naming.}

\item{qsub_args}{Additional arguments for \emph{qsub}.}

\item{jobwatch_args}{A list. Elements are passed to \code{\link{watch}}}
}
\value{
A function which has a dammy argument
}
\description{
Short hand of creating a function
doing \code{\link{qsub}} with \code{watch = TRUE}.
The created function has a dammy argument which has no effect.
}
