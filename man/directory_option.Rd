% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qsub_helpers.R
\name{directory_option}
\alias{directory_option}
\title{Directory requirements}
\usage{
directory_option(cwd = FALSE, out = fs::path_home(),
  err = NA_character_)
}
\arguments{
\item{cwd}{A logical. Whether set the directory where you run your code as current working directory. Otherwise, your home directory is set as current working directory.}

\item{out}{Path to write stdout}

\item{err}{Path to write stderr (option). If unspecified, \emph{out} argument will be used instead.}
}
\description{
Directory requirements
}
