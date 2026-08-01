\name{backend-services}
\alias{backend-services}
\alias{fpq_create_project}
\alias{fpq_open_project}
\alias{fpq_save_project}
\alias{fpq_archive_project}
\alias{fpq_close_project}
\alias{fpq_project_status}
\alias{fpq_save_configuration}
\alias{fpq_load_configuration}
\alias{fpq_add_condition}
\alias{fpq_update_condition}
\alias{fpq_remove_condition}
\alias{fpq_add_sample}
\alias{fpq_update_sample}
\alias{fpq_remove_sample}
\alias{fpq_add_comparison}
\alias{fpq_remove_comparison}
\alias{fpq_reorder_conditions}
\alias{fpq_reorder_comparisons}
\alias{fpq_register_dataset}
\alias{fpq_register_file}
\alias{fpq_validate_pair}
\alias{fpq_verify_registered_file}
\alias{fpq_list_profiles}
\alias{fpq_profile_aliases}
\alias{fpq_resolve_profile}
\alias{fpq_compare_profiles}
\alias{fpq_validate_structural}
\alias{fpq_validate_profile_compatibility}
\alias{fpq_workflow_stage_status}
\alias{fpq_register_artifact}
\alias{fpq_normalize_artifact}
\alias{fpq_list_artifacts}
\alias{fpq_inspect_artifact}
\alias{fpq_update_artifact_status}
\alias{fpq_verify_artifact}
\alias{fpq_artifact_parents}
\alias{fpq_artifact_consumers}
\alias{fpq_log_event}
\alias{fpq_store_validation}
\alias{fpq_store_provenance}
\alias{fpq_trace_provenance}
\alias{fpq_create_run}
\alias{fpq_load_run}
\alias{fpq_transition_run}
\alias{fpq_recover_project}
\alias{fpq_check_environment}
\alias{fpq_verify_scientific_core}
\alias{fpq_dispatch_adapter}
\alias{fpq_atomic_write_json}
\alias{fpq_hash_file}
\alias{fpq_hash_object}
\alias{fpq_id}
\alias{fpq_now}
\alias{fpq_project_paths}
\alias{fpq_read_collection}
\alias{fpq_read_json}
\alias{fpq_repo_root}
\alias{fpq_run_path}
\alias{fpq_run_transitions}
\alias{fpq_stop}
\alias{fpq_validate_dynamic_configuration}
\alias{fpq_validate_project_manifest}
\alias{fpq_validation_record}
\alias{fpq_with_project_lock}
\alias{fpq_write_collection}
\title{FullPixel MSI Quant backend services}
\description{
Non-interactive service functions for transactional project lifecycle,
configuration persistence, external dataset registration, scientific-profile
inspection, layered validation, durable run state, artifact registration,
structured logging, provenance, environment checks, frozen-source integrity,
and controlled dispatch of the regression-approved extraction adapter.
}
\usage{
fpq_create_project(project_root, project_id, project_name,
  profile_id = "pvl-fullpixel-reference-v1")
fpq_open_project(project_root)
fpq_save_project(project, updates = list())
fpq_validate_structural(config)
fpq_validate_profile_compatibility(config, profile_id)
fpq_workflow_stage_status(stage_id, ion_mode = "NEG", exposure = "Single")
fpq_check_environment(request = NULL, repository_root = fpq_repo_root())
}
\arguments{
  \item{project_root}{Explicit project directory outside the package source.}
  \item{project_id}{Unique project identifier beginning with \code{prj-}.}
  \item{project_name}{User-facing project name.}
  \item{profile_id}{Scientific-profile identifier.}
  \item{project}{An explicit project handle returned by \code{fpq_open_project}.}
  \item{updates}{Allowed project metadata changes.}
  \item{config}{A generic repeatable condition, sample, and comparison configuration.}
  \item{stage_id}{Descriptive workflow-stage identifier.}
  \item{ion_mode}{Registered ion mode.}
  \item{exposure}{Registered exposure label.}
  \item{request}{Optional execution request.}
  \item{repository_root}{Explicit FullPixel MSI Quant repository root.}
}
\details{
Services persist state in project-local files and do not depend on Shiny or
global project state. The generic configuration model preserves arbitrary
condition and sample records, while analysis-design validators enforce the
specific quantify-only, two-group, or multi-group contracts selected by the
user. Scientific execution is delegated to registered external workers and
frozen-source adapters.
}
\value{
Service-specific project handles, records, validation results, artifact
records, run records, environment reports, or dispatch results.
}
