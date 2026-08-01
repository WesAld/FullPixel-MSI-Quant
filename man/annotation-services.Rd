\name{annotation-services}
\alias{fpq_dispatch_script04}
\alias{fpq_script04_type}
\alias{fpq_validate_script04_parent}
\title{Annotation-integration backend services}
\description{Validates registered quantitative and annotation parents and
dispatches the analysis-design-specific annotation adapter while enforcing
quantitative preservation.}
\usage{
fpq_dispatch_script04(project, request_path,
  repository_root = fpq_repo_root(), python = Sys.which("python"))

fpq_script04_type(x)

fpq_validate_script04_parent(project, x, type, request)
}
\arguments{
\item{project}{An open FullPixel MSI Quant project.}
\item{request_path}{Path to a schema-valid Script 04 execution request.}
\item{repository_root}{Resolved repository root.}
\item{python}{Python executable used for the adapter.}
\item{x}{A filename or registered artifact record.}
\item{type}{Required artifact type.}
\item{request}{Parsed execution request.}
}
\value{A dispatch result, mapped artifact type, or validated artifact record.}
