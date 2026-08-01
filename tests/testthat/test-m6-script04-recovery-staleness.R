make_m6_run_fixture <- function(status="running",project_id="prj-m6-test") {
  root <- tempfile("fpq-m6-"); project <- fpq_create_project(root,project_id,"M6")
  request <- fpq_read_json(file.path(fpq_repo_root(),"tests","fixtures","reports",
                                     "m6_script04_execution_request.json"))
  request$project_id <- project_id; request$run_id <- fpq_id("run-m6")
  request$output_working_directory <- tempfile("fpq-m6-workspace-")
  path <- file.path(root,"config","snapshots",paste0(request$run_id,".json"))
  fpq_atomic_write_json(request,path); fpq_create_run(project,request)
  for(s in c("preflight_running","ready","starting","running","validating")) {
    if(status=="configured") break
    fpq_transition_run(project,request$run_id,s); if(s==status)break
  }
  identity <- fpq_register_worker(project,path,fpq_repo_root(),
    paste0("worker-",request$run_id),pid=424242L,parent_pid=111L)
  list(project=project,request=request,path=path,identity=identity)
}

m6_match <- function(identity,alive=TRUE,command=NULL) function(pid) list(
  alive=alive,pid=as.integer(pid),parent_pid=identity$parent_process_id,
  command_line=command %||% paste("Rscript fpq_dispatch_worker.R",
    identity$worker_id,identity$run_id))

test_that("Script 04 cancellation safety and terminal idempotency fail closed", {
  x <- make_m6_run_fixture()
  called <- FALSE; executor <- function(...) {called<<-TRUE;list(status=0L,stdout="",stderr="")}
  bad <- m6_match(x$identity,command="Rscript unrelated.R")
  expect_error(fpq_terminate_worker_tree(x$project,x$request$run_id,bad,executor),
               class="fpq_infrastructure_error")
  expect_false(called)
  good <- m6_match(x$identity)
  fpq_request_cancellation(x$project,x$request$run_id,good)
  expect_true(fpq_request_cancellation(x$project,x$request$run_id,good)$idempotent)
  fpq_finalize_cancelled_run(x$project,x$request$run_id,"test")
  expect_identical(fpq_request_cancellation(x$project,x$request$run_id,good)$status,
                   "cancelled")
  expect_error(fpq_request_cancellation(x$project,"run-unknown",good),
               class="fpq_input_error")
})

test_that("malformed and mismatched worker metadata require manual review", {
  x <- make_m6_run_fixture()
  paths <- fpq_worker_paths(x$project,x$request$run_id)
  id <- fpq_read_json(paths$identity); id$project_id <- "wrong"
  fpq_atomic_write_json(id,paths$identity)
  observed <- fpq_inspect_worker(x$project,x$request$run_id,
    m6_match(id,command="Rscript unrelated.R"))
  expect_identical(observed$outcome,"manual_review_required")
  expect_false(observed$identity_match)
  writeLines("{bad",paths$identity)
  expect_error(fpq_inspect_worker(x$project,x$request$run_id),
               class="fpq_configuration_error")
})

test_that("Script 04 recovery never infers success from files", {
  dead <- function(pid)list(alive=FALSE,pid=pid)
  x <- make_m6_run_fixture("running")
  expect_identical(fpq_recover_interrupted_runs(x$project,dead)[[1]]$outcome,
                   "worker_missing")
  y <- make_m6_run_fixture("validating")
  expect_identical(fpq_recover_interrupted_runs(y$project,dead)[[1]]$outcome,
                   "validation_interrupted")
  fpq_write_worker_terminal_markers(y$project,y$request$run_id,
    y$identity$worker_id,0L,"completed")
  expect_identical(fpq_recover_interrupted_runs(y$project,dead)[[1]]$outcome,
                   "worker_completed_pending_validation")
  expect_identical(fpq_load_run(y$project,y$request$run_id)$status,"validating")
})

test_that("either Script 04 parent recursively stales the annotated master", {
  root <- tempfile("fpq-m6-stale-"); project <- fpq_create_project(root,"prj-m6-stale","M6")
  mk <- function(id,type,parent=list()) {
    path <- tempfile(fileext=".csv"); writeLines("id,value\nA,1",path)
    fpq_register_artifact(project,list(artifact_id=id,artifact_type=type,path=path,
      project_id=project$manifest$project_id,dataset_id="dset",sha256=fpq_hash_file(path),
      file_size=as.integer(file.info(path)$size),validation_status="PASS",
      scientific_profile_id="pvl-fullpixel-reference-v1",scientific_core_version="1.0.0",
      ion_mode="NEG",exposure="Single",parent_artifact_ids=parent),activate=TRUE)
  }
  q <- mk("art-q","replicate_readout_summary"); a <- mk("art-ann","annotated_feature_list")
  out <- mk("art-out","annotated_three_readout_master",list(q$artifact_id,a$artifact_id))
  fpq_update_artifact_status(project,a$artifact_id,"superseded")
  expect_identical(fpq_inspect_artifact(project,out$artifact_id)$active_status,"stale")
  expect_match(fpq_inspect_artifact(project,out$artifact_id)$notes,"upstream artifact")
})

test_that("workspace collision and backup restoration are explicit", {
  x <- make_m6_run_fixture(); fpq_prepare_workspace_record(x$project,x$request$run_id,
    x$request$output_working_directory)
  second <- x$request; second$run_id <- fpq_id("run-m6-second")
  fpq_create_run(x$project,second)
  expect_error(fpq_prepare_workspace_record(x$project,second$run_id,
    x$request$output_working_directory),class="fpq_infrastructure_error")
})
