% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph_col_com.R
\name{graph_col_com}
\alias{graph_col_com}
\title{A wrapper for \code{igraph::plot.igraph} for node communities (discrete color values)}
\usage{
graph_col_com(graph, communities, labels = "", lay = NULL, title = NULL)
}
\arguments{
\item{graph}{An igraph object.}

\item{communities}{A character vector of the same length as the number of nodes indicating node communities.}

\item{labels}{A character vector of the same length as the number of nodes indicating node labels. default = \code{''}.}

\item{lay}{An igraph valid network layout. default = 'NULL'.}

\item{title}{A string indicating the title of the graph plot.}
}
\value{
An igraph plot.
}
\description{
Visualizes a graph and colors its nodes based on the communities.
}
\author{
Elyas Heidari
}
