#!/usr/bin/env python3
"""Generic two-group adapter for the validated Script 03 calculations."""
from __future__ import annotations
import argparse, csv, hashlib, json, shutil, subprocess, sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
from normalization_routing import provenance, resolve_method, route_extraction_summary, summary_columns

ROOT = Path(__file__).resolve().parents[1]
SCRIPT_HASH = "3D3E2E7A6E81972FE8F6EA4D54F5BCD52F47D1B66C62E35F8E1CFF00ABB60A97"
BOUNDARY = '\n###############################################################################\n\nrequired_packages <- c("data.table", "openxlsx")'
UNRESOLVED = object()

def resolve_executable(configured):
    value=str(configured or "").strip()
    if not value: return None
    candidate=Path(value)
    if candidate.is_file(): return str(candidate.resolve())
    resolved=shutil.which(value)
    return str(Path(resolved).resolve()) if resolved else None

def sha(path: Path) -> str:
    h=hashlib.sha256()
    with path.open("rb") as f:
        for b in iter(lambda:f.read(1024*1024),b""): h.update(b)
    return h.hexdigest().upper()

def issue(code,message,category="configuration"):
    return {"check_id":code,"status":"FAIL","category":category,"message":message,"blocking":True}

def rquote(value): return '"'+str(value).replace('\\','/').replace('"','\\"')+'"'
def rvec(values): return 'c('+', '.join(rquote(v) for v in values)+')'

def read_csv(path: Path):
    with path.open(encoding="utf-8-sig",newline="") as f:
        r=csv.DictReader(f); rows=list(r); return r.fieldnames or [],rows

def validate_request(req,check_environment=True,r_executable=UNRESOLVED):
    problems=[]
    try: method=resolve_method(req)
    except ValueError as exc:
        method="spectrum_sum"; problems.append(issue("scientific.normalization_method",str(exc),"scientific"))
    if req.get("stage_id")!="summarize_replicate_readouts":
        problems.append(issue("stage.unsupported","Expected summarize_replicate_readouts.","scientific"))
    samples=req.get("samples",[])
    ctrl=[x for x in samples if x.get("condition_id")=="CTRL"]
    trt=[x for x in samples if x.get("condition_id")=="TRT"]
    if len(ctrl)<2 or len(ctrl)!=len(trt):
        problems.append(issue("design.replicates","Generic v1 requires equal replicate counts and at least two samples per condition.","scientific"))
    input_art=req.get("registered_inputs",{}).get("pixel_intensity_artifact",{})
    path=Path(input_art.get("path",""))
    if not path.is_file(): problems.append(issue("input.summary","Pixel-intensity artifact is missing.","input"))
    elif sha(path)!=str(input_art.get("sha256","")).upper():
        problems.append(issue("input.hash","Pixel-intensity artifact hash differs from its registered identity.","scientific"))
    elif input_art.get("active_status")!="active" or input_art.get("validation_status")!="PASS":
        problems.append(issue("input.inactive","Pixel-intensity artifact must be active and validated.","scientific"))
    else:
        header,rows=read_csv(path)
        overall,local=summary_columns(method)
        required={"target_id","mzmine_row_id","target_mz","sample_id",overall,local,
                  "normalized_mean_conservative_detected_pixels","conservative_detected_pixel_count",
                  "conservative_detection_fraction","primary_quantification_eligible","primary_qc_status"}
        missing=required-set(header)
        if missing: problems.append(issue("input.schema","Missing extraction columns: "+", ".join(sorted(missing)),"input"))
        actual={r.get("sample_id") for r in rows}
        expected={x.get("sample_id") for x in samples}
        if actual!=expected: problems.append(issue("input.samples","Extraction sample coverage does not match the request.","scientific"))
    frozen=ROOT/req.get("frozen_script",{}).get("repository_relative_path","")
    if not frozen.is_file() or sha(frozen)!=SCRIPT_HASH:
        problems.append(issue("integrity.frozen_script","Validated Script 03 identity failed.","infrastructure"))
    workspace=Path(req.get("output_working_directory",""))
    if workspace.exists() and any(workspace.iterdir()):
        problems.append(issue("workspace.not_empty","Workspace must be absent or empty.","infrastructure"))
    if check_environment:
        resolved=(resolve_executable(req.get("environment_requirements",{}).get("r_executable",""))
                  if r_executable is UNRESOLVED else r_executable)
        if resolved is None: problems.append(issue("environment.r","Configured Rscript is unavailable.","environment"))
    return problems

def make_copy(req,workspace:Path):
    source=ROOT/req["frozen_script"]["repository_relative_path"]
    original=source.read_text(encoding="utf-8")
    start=original.index('dataset_id <- "PVL_Single_Negative"')
    end=original.index(BOUNDARY)
    ctrl=[x["sample_id"] for x in req["samples"] if x["condition_id"]=="CTRL"]
    trt=[x["sample_id"] for x in req["samples"] if x["condition_id"]=="TRT"]
    workspace.mkdir(parents=True,exist_ok=False)
    method=resolve_method(req)
    routed=route_extraction_summary(
        Path(req["registered_inputs"]["pixel_intensity_artifact"]["path"]),
        workspace/"primary_normalization_input.csv", method)
    analysis=(workspace/"analysis").resolve()
    panel=f'''dataset_id <- {rquote(req["dataset_id"])}

input_csv <-
  {rquote(routed)}

analysis_root <- {rquote(analysis.as_posix())}

output_parent <- file.path(
  analysis_root,
  "03_Replicate_Summaries",
  dataset_id
)

control_sample_ids <- {rvec(ctrl)}

treatment_sample_ids <- {rvec(trt)}

control_condition_label <- "CTRL"
treatment_condition_label <- "TRT"
output_treatment_label <- "TRT"

high_cv_threshold_percent <- 30
strong_signal_threshold <- 1000
minimum_strong_pixels <- 10L
minimum_supported_replicates <- {min(2,len(ctrl))}L
absolute_log2fc_candidate_threshold <- 1

require_all_replicates_for_condition_statistics <- TRUE
write_excel_output <- TRUE

required_sample_ids <- c(
  control_sample_ids,
  treatment_sample_ids
)
'''
    generated=original[:start]+panel+original[end:]
    path=workspace/"execution_copy_generic_script03.R"
    path.write_text(generated,encoding="utf-8",newline="\n")
    record={"source_hash":sha(source),"execution_copy_hash":sha(path),"sample_count":len(ctrl)+len(trt),
            "replicates_per_condition":len(ctrl),"configuration_boundary":"request-driven","scientific_body_unchanged":True,
            "normalization":provenance(method,req.get("approved_scientific_configuration",{}).get("normalization_scale",1000000))}
    (workspace/"configuration_transformations.json").write_text(json.dumps(record,indent=2),encoding="utf-8")
    return path,record

def main():
    p=argparse.ArgumentParser(); p.add_argument("request"); p.add_argument("--report",required=True); a=p.parse_args()
    req=json.loads(Path(a.request).read_text(encoding="utf-8")); rp=Path(a.report)
    report={"schema_version":"1.0.0","run_id":req.get("run_id"),"stage_id":req.get("stage_id"),
            "status":"preflight_failed","validation_records":[],"produced_outputs":[]}
    configured_rex=req.get("environment_requirements",{}).get("r_executable","")
    resolved_rex=resolve_executable(configured_rex)
    problems=validate_request(req,r_executable=resolved_rex); report["validation_records"]=problems
    if problems:
        rp.parent.mkdir(parents=True,exist_ok=True); rp.write_text(json.dumps(report,indent=2),encoding="utf-8"); return 2
    workspace=Path(req["output_working_directory"]).resolve()
    try:
        copy,transform=make_copy(req,workspace)
        out,err=workspace/"stdout.log",workspace/"stderr.log"
        report.update(status="running",execution_started=True,frozen_script_hash=SCRIPT_HASH,
                      execution_copy_hash=sha(copy),configuration_transformations=transform,
                      stdout_path=str(out),stderr_path=str(err),
                      r_executable_configured=configured_rex,
                      r_executable_resolved=resolved_rex)
        rp.parent.mkdir(parents=True,exist_ok=True); rp.write_text(json.dumps(report,indent=2),encoding="utf-8")
        with out.open("w",encoding="utf-8") as o,err.open("w",encoding="utf-8") as e:
            proc=subprocess.run([resolved_rex,str(copy)],cwd=workspace,stdout=o,stderr=e)
        report["process_exit_status"]=proc.returncode
        if proc.returncode: raise RuntimeError("Generic Script 03 execution exited unsuccessfully.")
        parent=workspace/"analysis"/"03_Replicate_Summaries"/req["dataset_id"]
        dirs=[x for x in parent.glob("ThreeReadout_ReplicateSummary_*") if x.is_dir()]
        if len(dirs)!=1: raise RuntimeError(f"Expected one Script 03 output directory; found {len(dirs)}.")
        od=dirs[0]; primary=od/f'{req["dataset_id"]}_ThreeReadout_Summary.csv'
        if not primary.is_file(): raise RuntimeError("Generic Script 03 summary CSV is missing.")
        header,rows=read_csv(primary)
        input_header,input_rows=read_csv(Path(req["registered_inputs"]["pixel_intensity_artifact"]["path"]))
        target_count=len({r["target_id"] for r in input_rows})
        required={"dataset_id","target_id","mzmine_row_id","target_mz","log2FC_overall_TRT_vs_CTRL",
                  "local_intensity_log2FC_TRT_vs_CTRL","detection_fraction_difference_TRT_minus_CTRL"}
        for sample in [x["sample_id"] for x in req["samples"]]:
            required.add(sample+"_overall_normalized_signal")
            required.add(sample+"_local_strong_pixel_intensity")
            required.add(sample+"_conservative_detection_fraction")
        missing=sorted(required-set(header))
        if missing or len(rows)!=target_count or len({r["target_id"] for r in rows})!=target_count:
            raise RuntimeError("Generic Script 03 output dimensions or required columns failed validation.")
        outputs=[]
        for path in sorted(od.iterdir()):
            if not path.is_file(): continue
            nrows=ncols=None
            if path.suffix.lower()==".csv":
                h,r=read_csv(path); nrows=len(r); ncols=len(h)
            outputs.append({"artifact_role":path.name,"path":str(path),"sha256":sha(path),
                            "bytes":path.stat().st_size,"rows":nrows,"columns":ncols})
        report.update(status="completed",produced_outputs=outputs,
                      equivalence={"status":"NOT_EVALUABLE","reason":"No dataset-specific historical oracle; structural and calculation self-checks passed."},
                      output_validation={"status":"PASS","rows":len(rows),"columns":len(header),"targets":target_count},
                      validation_records=[{"check_id":"generic.script03","status":"PASS","category":"scientific",
                        "blocking":False,"message":"Dynamic replicate columns, target retention, and Script 03 internal calculation checks passed."}])
        rp.write_text(json.dumps(report,indent=2),encoding="utf-8"); return 0
    except Exception as exc:
        report.update(status="failed",error=str(exc),recommended_correction="Inspect preserved generic Script 03 diagnostics; no output was activated.")
        rp.parent.mkdir(parents=True,exist_ok=True); rp.write_text(json.dumps(report,indent=2),encoding="utf-8"); return 1

if __name__=="__main__": raise SystemExit(main())
