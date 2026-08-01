\name{fpq_inspect_annotation_table}
\alias{fpq_inspect_annotation_table}
\title{Inspect an annotated MZmine feature table}
\usage{
fpq_inspect_annotation_table(path, mappings = list())
}
\arguments{
  \item{path}{Path to a CSV annotated feature table.}
  \item{mappings}{Named source-column mappings for required annotation roles.}
}
\value{
A list containing file metadata, a limited preview, detected columns, mappings,
and validation records. The source file is not modified.
}
\description{
Inspects an annotated MZmine feature table and validates explicit UI mappings
without changing the scientific source data.
}
