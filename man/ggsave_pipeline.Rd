% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/file_management.R
\name{ggsave_pipeline}
\alias{ggsave_pipeline}
\title{save a plan image}
\usage{
ggsave_pipeline(plan, path, ...)
}
\arguments{
\item{plan}{A plan made with \code{drake::\link[drake]{make}}}

\item{path}{A path to write image}

\item{...}{Additional arguments for \code{ggplot2::\link[ggplot2]{ggsave}}}
}
\description{
wrapper of \code{drake::\link[drake]{drake_ggraph}} and \code{ggplot2::\link[ggplot2]{ggsave}}
}
