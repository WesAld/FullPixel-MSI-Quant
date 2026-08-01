\name{execution-services}
\alias{fpq_worker_paths}
\alias{fpq_windows_process_info}
\alias{fpq_parent_pid}
\alias{fpq_register_worker}
\alias{fpq_load_worker_identity}
\alias{fpq_write_worker_terminal_markers}
\alias{fpq_touch_worker_heartbeat}
\alias{fpq_worker_identity_matches}
\alias{fpq_inspect_worker}
\alias{fpq_cancellation_requested}
\alias{fpq_request_cancellation}
\alias{fpq_terminate_worker_tree}
\alias{fpq_finalize_cancelled_run}
\alias{fpq_prepare_workspace_record}
\alias{fpq_set_workspace_state}
\alias{fpq_recover_interrupted_runs}
\alias{fpq_validate_script03_input}
\alias{fpq_script03_artifact_type}
\alias{fpq_script03_media_type}
\alias{fpq_dispatch_script03}
\alias{fpq_refresh_downstream_staleness}
\title{Worker-control and replicate-summary backend services}
\description{
Backend services for persisted worker identity, heartbeat, cancellation,
recovery, workspace lifecycle, upstream artifact staleness, and
analysis-design-specific replicate-summary dispatch. These functions do not
implement scientific calculations.
}
\details{
Process termination is permitted only after project, run, worker, PID, and
command-line identity checks. Replicate-summary dispatch requires an active
validated extraction artifact, matching hashes, compatible analysis design,
and passing frozen-source and adapter preflight checks.
}
\keyword{internal}
