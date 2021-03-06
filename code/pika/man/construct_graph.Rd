% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/construct_graph.R
\name{construct_graph}
\alias{construct_graph}
\title{Construct and visualize the gene interaction networks for train and test.}
\usage{
construct_graph(sce, ...)
}
\arguments{
\item{sce}{A SingleCellExperiment object in which train and test subsets are tagged in the \code{colData}.}

\item{...}{Any additional arguments.}
}
\value{
A SingleCellExperiment object with gene interaction networks for train (\code{metadata(.)$train$graph}) and test (\code{metadata(.)$test$graph}).
}
\description{
Constructs a the gene interaction networks for train and test separately based on Gaussian graphical models, mutual information, or prior knowledge.
}
\section{Additional arguments}{

\describe{
\item{method}{The method to be used for the network construction (possible values = \code{c('ggm', 'tf', 'coexp')}).}
\item{ggm}{Only used if \code{'ggm'} is specified as a method. Parameters to be passed to \code{ggm_net} function.}
\item{tf}{Only used if \code{'tf'} is specified as a method. Parameters to be passed to \code{tf_net} function.}
\item{coexp}{Only used if \code{'coexp'} is specified as a method. Parameters to be passed to \code{coexp_net} function.}
\item{agg_fun}{If multiple methods are specified, how to aggregate them? if \code{'intersect'} intersects the edge sets of all methods, otherwise, takes a union.}
\item{vis}{Visualization parameters to be passed to \code{adj2graph} function.}
\item{log}{A logical indicating whether to log the computation times.}
\item{verbose}{A logical indicating whether to print out the computation steps.}
}
}

\author{
Elyas Heidari
}
