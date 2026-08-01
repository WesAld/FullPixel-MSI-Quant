fpq_script04_type <- function(x) {
  if (grepl("_QuantifyOnly_Annotated_Master[.]csv$", x))
    return("annotated_three_readout_master")
  if (grepl("_MultiGroup_Annotated_Master[.]csv$", x))
    return("annotated_three_readout_master")
  if (grepl("_Annotated_ThreeReadout_Master[.]csv$", x))
    return("annotated_three_readout_master")
  if (grepl("_Annotated_ThreeReadout_Master[.]xlsx$", x))
    return("annotation_integration_workbook")
  switch(x,
  "PVL_Single_Negative_Annotated_ThreeReadout_Master.csv"="annotated_three_readout_master",
  "PVL_Single_Negative_Annotated_ThreeReadout_Master.xlsx"="annotation_integration_workbook",
  "analysis_validation_summary.csv"="annotation_integration_validation",
  "annotation_merge_status_summary.csv"="annotation_match_status",
  "PVL_Single_Negative_All_Annotated_Features.csv"="annotation_all_annotated_features",
  "PVL_Single_Negative_HighPriority_Annotated_Candidates.csv"="annotation_high_priority_candidates",
  "PVL_Single_Negative_Quantitative_LowerScore_Annotated_Candidates.csv"="annotation_lower_score_candidates",
  "PVL_Single_Negative_Quantitative_Unannotated_Candidates.csv"="annotation_unannotated_quantitative_candidates",
  "analysis_configuration_used.csv"="configuration_snapshot",
  "scientific_execution_record")
}

fpq_validate_script04_parent <- function(project, x, type, request) {
  if (is.null(x)) fpq_stop("script04.input_missing", paste(type, "is required."), "input")
  if (!identical(x$project_id, project$manifest$project_id) ||
      !identical(x$dataset_id, request$dataset_id))
    fpq_stop("script04.input_scope", paste(type, "belongs to another project or dataset."), "scientific")
  a <- fpq_inspect_artifact(project, x$artifact_id)
  if (!identical(a$artifact_type, type) || !identical(a$active_status, "active") ||
      !identical(a$validation_status, "PASS"))
    fpq_stop("script04.input_inactive", paste(type, "must be active and validated."), "scientific")
  if (!file.exists(a$path) || !identical(fpq_hash_file(a$path), a$sha256) ||
      !identical(a$sha256, x$sha256))
    fpq_stop("script04.input_hash", paste(type, "identity failed."), "scientific")
  a
}

fpq_dispatch_script04 <- function(project, request_path, repository_root=fpq_repo_root(),
                                  python=Sys.which("python")) {
  request <- fpq_read_json(request_path)
  q <- fpq_validate_script04_parent(project,
    request$registered_inputs$replicate_summary_artifact,
    "replicate_readout_summary", request)
  ann <- fpq_validate_script04_parent(project,
    request$registered_inputs$annotation_artifact, "annotated_feature_list", request)
  stage <- fpq_workflow_stage_status(request$stage_id, request$ion_mode, request$exposure_label)
  if (!stage$executable) fpq_stop("dispatch.scope_blocked", stage$reason, "scientific")
  if (fpq_verify_scientific_core(repository_root)$status != "compatible")
    fpq_stop("dispatch.integrity", "Frozen scientific-core integrity failed.", "scientific")
  run <- fpq_create_run(project, request)
  fpq_prepare_workspace_record(project, run$run_id, request$output_working_directory)
  for (s in c("preflight_running","ready","starting","running")) fpq_transition_run(project,run$run_id,s)
  test_delay <- suppressWarnings(as.numeric(Sys.getenv(
    "FULLPIXEL_MSI_QUANT_TEST_SCRIPT04_DELAY_SECONDS", unset = "0")))
  test_mode <- identical(Sys.getenv("FULLPIXEL_MSI_QUANT_TEST_MODE"), "true")
  fixture_workspace <- grepl("FullPixel-MSI-Quant-M6-Closure-Fixtures",
                             request$output_working_directory, fixed = TRUE)
  if (test_mode && fixture_workspace && is.finite(test_delay) && test_delay > 0)
    Sys.sleep(min(test_delay, 30))
  rp <- file.path(dirname(fpq_run_path(project,run$run_id)),"adapter_report.json")
  so <- file.path(dirname(rp),"adapter-process.stdout.log"); se <- file.path(dirname(rp),"adapter-process.stderr.log")
  adapter_file <- if (identical(request$execution_adapter %||% "",
      "generic_quantify_only")) "quantify_only_annotation_adapter.py"
    else if (identical(request$execution_adapter %||% "",
      "generic_two_group")) "generic_script04_adapter.py"
    else if (identical(request$execution_adapter %||% "",
      "generic_multi_group")) "multi_group_annotation_adapter.py"
    else "script04_adapter.py"
  p <- processx::process$new(python,c(file.path(repository_root,"tools",adapter_file),
    request_path,"--report",rp),wd=repository_root,stdout=so,stderr=se,cleanup=FALSE,supervise=FALSE)
  cancelled <- FALSE
  identity <- fpq_load_worker_identity(project,run$run_id)
  while (p$is_alive()) {
    if (!is.null(identity))
      fpq_touch_worker_heartbeat(project,run$run_id,"script04_process_active")
    if (fpq_cancellation_requested(project,run$run_id)) {
      current <- fpq_load_run(project,run$run_id)
      if (!identical(current$status,"cancelling"))
        fpq_transition_run(project,run$run_id,"cancelling")
      try(p$kill_tree(),silent=TRUE)
      p$wait(10000); cancelled <- TRUE; break
    }
    Sys.sleep(.1)
  }
  code <- p$get_exit_status() %||% 1L
  if (cancelled) return(list(run=fpq_finalize_cancelled_run(project,run$run_id,
    "cooperative_marker_process_tree",code,character()),artifact=NULL,artifacts=list()))
  report <- if(file.exists(rp)) fpq_read_json(rp) else NULL
  annotation_status <- report$annotation_equivalence$status %||% ""
  quantitative_status <- report$quantitative_preservation$status %||% ""
  candidate_annotation <-
    identical(annotation_status, "NOT_EVALUABLE") &&
    identical(quantitative_status, "PASS") &&
    ((request$execution_adapter %||% "") %in%
      c("generic_two_group", "generic_quantify_only", "generic_multi_group") ||
     identical(request$exposure_label, "Sequential") ||
     identical(request$ion_mode, "POS"))
  if (is.null(report) || code != 0 || report$status != "completed" ||
      !identical(quantitative_status, "PASS") ||
      (!identical(annotation_status, "PASS") &&
       !candidate_annotation)) {
    fpq_transition_run(project,run$run_id,"failed")
    fpq_stop("dispatch.script04_failed","Script 04 adapter failed; no output was activated.","scientific")
  }
  fpq_transition_run(project,run$run_id,"validating")
  validation_specs <- list(
    c("script04.upstream_quantitative","Script 03 parent identity passed."),
    c("script04.annotation_input","Annotation artifact identity passed."),
    c("script04.annotation_mapping","Explicit annotation mappings passed."),
    c("script04.annotation_types","Annotation m/z and score types passed."),
    c("script04.annotation_score","Annotation score consistency passed."),
    c("script04.feature_matching","Feature matching passed."),
    c("script04.row_retention","All quantitative rows were retained."),
    c("script04.quantitative_preservation","All quantitative columns were preserved exactly."),
    c("script04.annotation_equivalence","Annotation equivalence passed."),
    c("script04.output_schema","The annotated master schema passed."),
    c("script04.provenance","Separate quantitative and annotation parents will be recorded."),
    c("script04.activation","Activation is permitted after required checks pass; approved technical-candidate warnings are nonblocking."))
  validations <- lapply(validation_specs,function(s) {
    candidate_record <- candidate_annotation &&
      identical(s[[1]], "script04.annotation_equivalence")
    validation_status <- if (candidate_record) "WARNING" else "PASS"
    validation_severity <- if (candidate_record) "WARNING" else "INFO"
    validation_message <- if (candidate_record)
      paste("Annotation equivalence is not evaluable because no approved",
            if (identical(request$execution_adapter %||% "",
                          "generic_quantify_only"))
              "quantify-only dataset-specific"
            else if (identical(request$execution_adapter %||% "", "generic_two_group"))
              "dataset-specific"
            else if (identical(request$execution_adapter %||% "", "generic_multi_group"))
              "multi-group dataset-specific"
            else if (identical(request$ion_mode, "POS")) "Positive" else "Sequential",
            "annotation oracle is available; quantitative preservation passed.")
    else s[[2]]
    v <- fpq_validation_record(s[[1]],validation_status,
      validation_message,validation_severity,FALSE,
      observed=if (candidate_record) "NOT_EVALUABLE" else "PASS",
      expected=if (candidate_record) "PASS or approved nonblocking technical-candidate classification" else "PASS",
      consequence="The validated annotation-integration output may proceed.",
      remediation=if (candidate_record)
        "Retain the Technical Validation Candidate label until an approved annotation oracle is available."
      else "None.")
    v$run_id <- run$run_id; v$stage_id <- request$stage_id; v
  })
  fpq_store_validation(project,validations)
  parents <- list(q$artifact_id,ann$artifact_id)
  arts <- lapply(report$produced_outputs,function(o) {
    primary <- grepl(
      "(_Annotated_ThreeReadout_Master|_QuantifyOnly_Annotated_Master|_MultiGroup_Annotated_Master)[.]csv$",
      o$artifact_role)
    a <- list(artifact_id=paste0("art-",run$run_id,"-",gsub("[^a-z0-9]+","-",tolower(o$artifact_role))),
      artifact_type=fpq_script04_type(o$artifact_role),display_name=o$artifact_role,path=o$path,
      project_id=project$manifest$project_id,dataset_id=request$dataset_id,producing_run_id=run$run_id,
      sha256=o$sha256,file_size=as.integer(o$bytes),validation_status="PASS",
      ion_mode=request$ion_mode,exposure=request$exposure_label,producing_stage_id=request$stage_id,
      path_type="external_absolute",media_type=if(grepl("xlsx$",o$path)) "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" else "text/plain",
      row_count = o$rows, column_count = o$columns,
      schema_id = if (primary) {
        if (identical(request$execution_adapter %||% "",
                      "generic_quantify_only"))
          "quantify_only.annotated_sample_master.v1"
        else if (identical(request$execution_adapter %||% "", "generic_two_group"))
          "script04.generic_annotated_master.v1"
        else if (identical(request$execution_adapter %||% "", "generic_multi_group"))
          "script04.generic_multi_group_annotated_master.v1"
        else
          "script04.annotated_master.129_columns.v1"
      } else NULL,
      scientific_profile_id=request$scientific_profile$profile_id,
      scientific_core_version=request$scientific_core_version,parent_artifact_ids=parents,
      notes=paste("quantitative preservation", quantitative_status,
                  "; annotation equivalence", annotation_status,
                  if (candidate_annotation) "(approved nonblocking technical candidate);" else ";",
                  "execution copy",report$execution_copy_hash))
    fpq_register_artifact(project,a,activate=primary)
  })
  primary <- Filter(function(x)x$artifact_type=="annotated_three_readout_master",arts)[[1]]
  edges <- c(lapply(list(q,ann),function(x) list(schema_version="1.0.0",provenance_edge_id=fpq_id("prov"),
    project_id=project$manifest$project_id,subject_type="artifact",subject_id=x$artifact_id,
    relationship="used_by",object_type="run",object_id=run$run_id,recorded_at=fpq_now(),
    run_id=run$run_id,evidence_hash=x$sha256,notes="Separate quantitative/annotation parent")),
    lapply(arts,function(x) list(schema_version="1.0.0",provenance_edge_id=fpq_id("prov"),
      project_id=project$manifest$project_id,subject_type="artifact",subject_id=x$artifact_id,
      relationship="produced_by",object_type="run",object_id=run$run_id,recorded_at=fpq_now(),
      run_id=run$run_id,evidence_hash=x$sha256,notes="Frozen Script 04 integration")))
  fpq_store_provenance(project,edges)
  done <- fpq_transition_run(project,run$run_id,"completed")
  done$output_artifacts <- lapply(arts,`[[`,"artifact_id"); done$parent_run_id <- q$producing_run_id
  done$validation_records <- lapply(validations,`[[`,"validation_record_id")
  done$parent_artifact_ids <- parents; done$execution_copy_hash <- report$execution_copy_hash
  done$logs <- list(stdout=report$stdout_path,stderr=report$stderr_path,adapter_stdout=so,adapter_stderr=se)
  fpq_atomic_write_json(done,fpq_run_path(project,run$run_id))
  fpq_set_workspace_state(project,run$run_id,"completed_preserved")
  list(run=done,artifact=primary,artifacts=arts,adapter_report=report)
}
