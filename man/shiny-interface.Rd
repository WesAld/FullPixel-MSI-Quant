\name{shiny-interface}
\alias{shiny-interface}
\alias{fpq_shiny_app}
\alias{fpq_new_app_state}
\alias{fpq_app_summary}
\alias{fpq_load_project_configuration}
\alias{fpq_badge}
\alias{fpq_empty_state}
\alias{fpq_error_ui}
\alias{fpq_read_jsonl}
\alias{fpq_preview_csv}
\alias{fpq_make_demo_configuration}
\alias{fpq_home_ui}
\alias{fpq_home_server}
\alias{fpq_project_ui}
\alias{fpq_project_server}
\alias{fpq_design_ui}
\alias{fpq_design_server}
\alias{fpq_dataset_ui}
\alias{fpq_dataset_server}
\alias{fpq_parse_pair_assignments}
\alias{fpq_inspect_target_table}
\alias{fpq_profile_ui}
\alias{fpq_profile_server}
\alias{fpq_validation_ui}
\alias{fpq_validation_server}
\alias{fpq_workflow_ui}
\alias{fpq_workflow_server}
\alias{fpq_runs_ui}
\alias{fpq_runs_server}
\alias{fpq_run_readiness}
\alias{fpq_list_runs}
\alias{fpq_artifacts_ui}
\alias{fpq_artifacts_server}
\alias{fpq_logs_ui}
\alias{fpq_logs_server}
\alias{fpq_provenance_ui}
\alias{fpq_provenance_server}
\alias{fpq_about_ui}
\alias{fpq_about_server}
\title{FullPixel MSI Quant Shiny interface}
\description{
Modular Shiny presentation functions that consume the persisted
FullPixelMSIQuant backend. They do not implement scientific calculations.
}
\usage{
fpq_shiny_app(repository_root = fpq_repo_root())
fpq_run_readiness(project, request, config, demonstration = FALSE,
  repository_root = fpq_repo_root())
fpq_make_demo_configuration()
}
\arguments{
  \item{repository_root}{Explicit authoritative repository root.}
  \item{project}{An open backend project handle.}
  \item{request}{An immutable execution-request record.}
  \item{config}{The generic dynamic project configuration.}
  \item{demonstration}{Whether the configuration is synthetic demonstration data.}
}
\details{
The application exposes project creation and reopening, quantify-only,
two-group, and multi-group designs, Positive and Negative ion-mode
registration, custom MZmine column mapping, scientific-profile inspection,
validation layers, resumable workflow execution, results downloads, runs,
artifacts, logs, and provenance.

Scientific calculations execute outside the Shiny session through registered
workers and adapters. The interface displays persisted validation and workflow
state rather than redefining scientific logic.
}
\value{
\code{fpq_shiny_app()} returns a Shiny application object. Other functions
return module UI/server definitions, display components, readiness results, or
synthetic demonstration configuration.
}
