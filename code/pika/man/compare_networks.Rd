% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_networks.R
\name{compare_networks}
\alias{compare_networks}
\title{Compare two networks}
\usage{
compare_networks(ref, comp, method = c("global", "local"), ...)
}
\arguments{
\item{ref}{Either a numeric or character vector, if \code{method = 'global'}, or an adjacency matrix, if \code{method = 'local'}.}

\item{comp}{Either a numeric or character vector, if \code{method = 'global'}, or an adjacency matrix, if \code{method = 'local'}.
Should be of the same size as \code{ref}.}

\item{...}{Any additional arguments.}
}
\value{
A SingleCellExperiment object with visualized comparison of graphs (\verb{$comparegraphs$vis}) and a coherency score (\verb{$comparegraphs$com_score}).
}
\description{
Compares two networks either globally (node clusters) or locally (exact shared edges).
}
\section{Additional arguments}{

\describe{
\item{log}{A logical indicating whether to log the computation times.}
\item{verbose}{A logical indicating whether to print out the computation steps.}
}
}

\author{
Elyas Heidari
}
