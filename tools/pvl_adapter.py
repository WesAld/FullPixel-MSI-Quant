#!/usr/bin/env python3
"""Execution boundary for the frozen PVL Script 01 reference workflow."""
import argparse, csv, difflib, hashlib, json, math, os, subprocess, sys
from datetime import datetime, timezone
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parent))
from normalization_routing import resolve_method

ROOT=Path(__file__).resolve().parents[1]
EXPECTED_INTERNAL={"CTRL_R1","CTRL_R2","CTRL_R3","TRT_R1","TRT_R2","TRT_R3"}
EXPECTED_HASH="D846E8BB354B3BED88A321B8D61FE0BE2F5FE42943C3365F12AAFD6C47F96CB0"
END_MARKER="\n###############################################################################\n\n###############################################################################\n# PACKAGE CHECKING AND RUN STATE"

def sha(path):
    h=hashlib.sha256()
    with open(path,"rb") as f:
        for b in iter(lambda:f.read(1024*1024),b""): h.update(b)
    return h.hexdigest().upper()
def rquote(value): return '"'+str(value).replace("\\","/").replace("\\","\\\\").replace('"','\\"')+'"'
def issue(code,message,category="configuration",correction="Correct the execution request and validate again."):
    return {"check_id":code,"status":"FAIL","category":category,"message":message,"blocking":True,"recommended_correction":correction}

def validate_request(req, check_environment=True):
    problems=[]
    if req.get("stage_id")!="extract_pixel_intensities": problems.append(issue("stage.unsupported","This adapter supports the Extract Pixel Intensities stage only."))
    if req.get("exposure_label") not in {"Single","Sequential"}: problems.append(issue("profile.exposure","Exposure must be Single or Sequential.","scientific"))
    if req.get("ion_mode") not in {"NEG","POS"}: problems.append(issue("profile.ion_mode","Ion mode must be NEG or POS.","scientific"))
    profile=req.get("scientific_profile",{})
    if profile.get("profile_id") not in {"pvl-fullpixel-reference-v1","fullpixel-40um-1600px-v1","pvl-positive-technical-candidate-v1"}: problems.append(issue("profile.id","The request does not select an approved reference or technical-validation profile."))
    conditions=req.get("conditions",[])
    if len(conditions)!=2 or {x.get("analytical_role") for x in conditions}!={"reference","comparison"}: problems.append(issue("profile.conditions","PVL frozen core requires exactly one reference and one comparison condition; supplied generic configuration is preserved.","scientific"))
    samples=req.get("samples",[])
    ids=[x.get("sample_id") for x in samples]
    if len(ids)!=len(set(ids)): problems.append(issue("mapping.duplicate_sample","Duplicate sample ID detected."))
    internal={x.get("internal_value") for x in req.get("internal_role_mappings",[])}
    if len(samples)!=6 or internal!=EXPECTED_INTERNAL: problems.append(issue("profile.samples","PVL frozen core requires an explicit one-to-one mapping to six internal roles; extra samples are not discarded.","scientific"))
    seen=set()
    for x in samples:
        key=(x.get("condition_id"),x.get("replicate_id"))
        if key in seen: problems.append(issue("mapping.duplicate_replicate",f"Duplicate replicate mapping {key}."))
        seen.add(key)
        for role in ("imzml_path","ibd_path"):
            p=x.get(role)
            if not p or not Path(p).is_file(): problems.append(issue(f"input.missing_{role}",f"Missing {role} for {x.get('sample_id')}: {p}"))
        if x.get("imzml_path") and x.get("ibd_path") and Path(x["imzml_path"]).stem.lower()!=Path(x["ibd_path"]).stem.lower():
            problems.append(issue("input.pair_mismatch",f"imzML/IBD stems differ for {x.get('sample_id')}."))
    cfg=req.get("approved_scientific_configuration",{})
    try: resolve_method(req)
    except ValueError as exc: problems.append(issue("scientific.normalization_method",str(exc),"scientific"))
    try:
        abs_tol=float(cfg.get("absolute_tolerance"))
        ppm_tol=float(cfg.get("ppm_tolerance"))
    except (TypeError,ValueError):
        abs_tol=float("nan"); ppm_tol=float("nan")
    tolerance_rule=cfg.get(
        "tolerance_combination_rule",
        "maximum_of_absolute_da_and_target_mz_times_ppm")
    if (not math.isfinite(abs_tol) or not math.isfinite(ppm_tol) or
        abs(abs_tol-0.002)>1e-15 or abs(ppm_tol-10)>1e-12 or
        tolerance_rule!="maximum_of_absolute_da_and_target_mz_times_ppm"):
        problems.append(issue(
            "profile.mass_tolerance",
            "The PVL reference adapter requires the validated max(10 ppm, 0.002 Da) tolerance.",
            "scientific"))
    acq=req.get("acquisition_metadata",{})
    expected_matrix="DHB" if req.get("ion_mode")=="POS" else "9AA"
    if acq!={"matrix":expected_matrix,"pixel_size_um":40,"full_pixel":True}: problems.append(issue("profile.acquisition",f"PVL {req.get('ion_mode')} technical profile requires {expected_matrix}, 40 um, full-pixel acquisition.","scientific"))
    if req.get("pixel_metadata",{}).get("expected_coordinate_count")!=1600: problems.append(issue("profile.coordinates","PVL reference requires the validated 1600-coordinate compatibility check.","scientific"))
    target=req.get("registered_inputs",{}).get("aligned_target_table",{})
    tp=target.get("path")
    if not tp or not Path(tp).is_file(): problems.append(issue("input.target_missing",f"Aligned target table is missing: {tp}"))
    else:
        if target.get("sha256","").upper()!=sha(tp): problems.append(issue("input.target_hash","Aligned target-table hash mismatch."))
        with open(tp,encoding="utf-8-sig",newline="") as f: header=next(csv.reader(f),[])
        for role,col in req.get("column_mappings",{}).items():
            if col not in header: problems.append(issue(f"column.{role}",f"Required mapped column '{col}' is missing."))
    registry=req.get("registered_inputs",{}).get("raw_file_hash_registry",{})
    registry_path=ROOT/registry.get("path","")
    if not registry_path.is_file() or sha(registry_path)!=registry.get("sha256","").upper():
        problems.append(issue("input.registry_hash","Raw-file hash registry is missing or modified.","infrastructure"))
    else:
        records=json.loads(registry_path.read_text(encoding="utf-8")).get("files",[])
        raw_records=[x for x in records if x["role"].endswith(("_imzML","_IBD"))]
        for record in raw_records:
            p=Path(record["path"])
            if not p.is_file() or sha(p)!=record["sha256"].upper():
                problems.append(issue("input.raw_hash",f"Raw input hash mismatch: {record['role']}"))
    frozen=ROOT/req.get("frozen_script",{}).get("repository_relative_path","")
    if not frozen.is_file() or sha(frozen)!=EXPECTED_HASH or req.get("frozen_script",{}).get("sha256","").upper()!=EXPECTED_HASH:
        problems.append(issue("integrity.frozen_script","Frozen Script 01 hash mismatch.","infrastructure","Restore the registered frozen source; never update the expected hash automatically."))
    out=ROOT/req.get("output_working_directory","")
    if out.exists() and any(out.iterdir()): problems.append(issue("output.not_empty",f"Output workspace must be absent or empty: {out}","infrastructure"))
    try: out.parent.mkdir(parents=True,exist_ok=True)
    except OSError as e: problems.append(issue("output.inaccessible",str(e),"environment"))
    if check_environment:
        rex=Path(req.get("environment_requirements",{}).get("r_executable",""))
        if not rex.is_file(): problems.append(issue("environment.r","Configured Rscript is unavailable.","environment"))
        elif not problems:
            pkgs=req["environment_requirements"]["required_packages"]
            expr="q(status=if(all(vapply(c("+",".join(repr(x) for x in pkgs)+"),requireNamespace,logical(1),quietly=TRUE)))0 else 41)"
            result=subprocess.run([str(rex),"-e",expr],capture_output=True,text=True)
            if result.returncode: problems.append(issue("environment.packages","Required packages are unavailable; runtime installation is prohibited.","environment"))
    return problems

def make_copy(req, workspace):
    frozen=ROOT/req["frozen_script"]["repository_relative_path"]
    text=frozen.read_text(encoding="utf-8")
    start_token='dataset_id <- "PVL_Single_Negative"'
    if text.count(start_token)!=1 or text.count(END_MARKER)!=1: raise RuntimeError("unexpected generated execution-copy structure")
    start=text.index(start_token); end=text.index(END_MARKER)
    scientific_body=text[end:]
    analysis=(workspace/"analysis").resolve()
    cfg=req["approved_scientific_configuration"]
    positive=req.get("ion_mode")=="POS"
    exposure=req.get("exposure_label","Single")
    dataset_id=f"PVL_{exposure}_{'Positive' if positive else 'Negative'}"
    mode="Positive" if positive else "Negative"
    matrix="DHB" if positive else "9AA"
    target_range="c(2500L, 2600L)" if positive else "c(1000L, 1500L)"
    panel=f'''dataset_id <- "{dataset_id}"\n\nraw_data_root <-\n  {rquote(req["registered_inputs"]["raw_data_root"])}\n\nmzmine_csv <-\n  {rquote(req["registered_inputs"]["aligned_target_table"]["path"])}\n\nanalysis_root <-\n  {rquote(analysis)}\n\nexpected_exposure <- "{exposure}"\nexpected_ion_mode <- "{mode}"\nexpected_matrix <- "{matrix}"\nexpected_pixel_size_um <- 40\nexpected_full_pixel <- TRUE\nexpected_coordinate_pixels <- 1600L\nmzmine_expected_row_range <- {target_range}\n\nexpected_sample_ids <- c(\n  "CTRL_R1",\n  "CTRL_R2",\n  "CTRL_R3",\n  "TRT_R1",\n  "TRT_R2",\n  "TRT_R3"\n)\n\nabsolute_tolerance <- {cfg["absolute_tolerance"]}\nppm_tolerance <- {cfg["ppm_tolerance"]}\n\ninclusive_detection_threshold <- {cfg["inclusive_detection_threshold"]}\nconservative_detection_threshold <- {cfg["conservative_detection_threshold"]}\n\nnormalization_scale <- {cfg["normalization_scale"]}\n\nsave_pixel_level_output <- {"TRUE" if req["retention_options"]["save_pixel_level_output"] else "FALSE"}\n\nprimary_detection_threshold <- {cfg["primary_detection_threshold"]}\nprimary_minimum_detected_pixels <- {cfg["primary_minimum_detected_pixels"]}L\n'''
    panel=panel.replace(
        f'normalization_scale <- {cfg["normalization_scale"]}\n',
        f'normalization_scale <- {cfg["normalization_scale"]}\nprimary_normalization_method <- {rquote(resolve_method(req))}\n', 1)
    generated=text[:start]+panel+scientific_body
    extension=ROOT/"tools"/"normalization_execution_extension.R"
    install=f'\nsource({rquote(extension.as_posix())}, local = .GlobalEnv)\nfpq_v2_install_normalization_extension(primary_normalization_method)\n\nmain()\n'
    if not generated.endswith("main()\n"): raise RuntimeError("validated Script 01 main-call boundary was not found")
    generated=generated[:-len("main()\n")]+install
    path=workspace/"execution_copy.R"; path.write_text(generated,encoding="utf-8",newline="")
    changes=list(difflib.unified_diff(text[:end].splitlines(),generated[:generated.index(END_MARKER)].splitlines(),fromfile="frozen-config",tofile="execution-config",lineterm=""))
    (workspace/"configuration_transformations.json").write_text(json.dumps(changes,indent=2),encoding="utf-8")
    return path,changes

def classify_process_outputs(exit_status, created_directories, expected_output_exists):
    """Pure fail-closed policy used by tests and the execution boundary."""
    if exit_status != 0:
        return {"status":"failed","partial_outputs_exist":bool(created_directories),"outputs_registered":False}
    if len(created_directories) != 1 or not expected_output_exists:
        return {"status":"validation_failed","partial_outputs_exist":bool(created_directories),"outputs_registered":False}
    return {"status":"completed","partial_outputs_exist":False,"outputs_registered":True}

def execute(request_path, prepare_only=False):
    req=json.loads(Path(request_path).read_text(encoding="utf-8"))
    workspace=(ROOT/req["output_working_directory"]).resolve()
    report={"schema_version":"1.0.0","run_id":req.get("run_id","unknown"),"status":"blocked","category":"configuration","execution_started":False,"partial_outputs_exist":False,"outputs_registered":False,"frozen_script_hash":EXPECTED_HASH,"execution_copy_hash":None,"configuration_transformations":[],"stdout_path":None,"stderr_path":None,"exit_status":None,"produced_outputs":[],"validation_records":[],"provenance_edges":[],"recommended_correction":None}
    problems=validate_request(req)
    if problems:
        report["validation_records"]=problems; report["category"]=problems[0]["category"]; report["recommended_correction"]=problems[0]["recommended_correction"]
        return report
    workspace.mkdir(parents=True,exist_ok=False)
    try:
        copy,changes=make_copy(req,workspace); report["execution_copy_hash"]=sha(copy); report["configuration_transformations"]=changes
    except Exception as e:
        report.update(status="blocked",category="infrastructure",recommended_correction=str(e)); return report
    if prepare_only:
        report.update(status="completed",category="none",recommended_correction=None); return report
    stdout=workspace/"stdout.log"; stderr=workspace/"stderr.log"; report.update(execution_started=True,stdout_path=str(stdout),stderr_path=str(stderr))
    rex=req["environment_requirements"]["r_executable"]
    with stdout.open("w",encoding="utf-8") as out, stderr.open("w",encoding="utf-8") as err:
        p=subprocess.run([rex,str(copy)],cwd=workspace,stdout=out,stderr=err)
    report["exit_status"]=p.returncode
    dataset_id=f"PVL_{req.get('exposure_label','Single')}_{'Positive' if req.get('ion_mode')=='POS' else 'Negative'}"
    analysis=workspace/"analysis"/"02_Pixel_Quantification"/dataset_id
    dirs=list(analysis.glob("SixSample_*")) if analysis.exists() else []
    report["partial_outputs_exist"]=bool(dirs)
    if p.returncode or len(dirs)!=1:
        report.update(status="failed",category="infrastructure",recommended_correction="Inspect captured streams and quarantined partial outputs; outputs were not registered."); return report
    expected=dirs[0]/"six_sample_combined_feature_summary.csv"
    if not expected.is_file():
        report.update(status="validation_failed",category="scientific",recommended_correction="Required exact output is missing; do not activate partial files."); return report
    artifact={"artifact_id":"art-"+req["run_id"]+"-combined","path":str(expected),"sha256":sha(expected),"bytes":expected.stat().st_size}
    report["produced_outputs"]=[artifact]; report["outputs_registered"]=True; report["status"]="completed"; report["category"]="none"
    report["validation_records"]=[{"check_id":"adapter.output.required","status":"PASS","category":"infrastructure","blocking":False,"message":"Exact required output exists."}]
    report["provenance_edges"]=[{"subject_id":req["registered_inputs"]["aligned_target_table"]["artifact_id"],"relationship":"used_by","object_id":req["run_id"]},{"subject_id":artifact["artifact_id"],"relationship":"produced_by","object_id":req["run_id"]}]
    return report

def main():
    ap=argparse.ArgumentParser(); ap.add_argument("request"); ap.add_argument("--prepare-only",action="store_true"); ap.add_argument("--report")
    a=ap.parse_args(); report=execute(a.request,a.prepare_only)
    text=json.dumps(report,indent=2); print(text)
    if a.report: Path(a.report).write_text(text,encoding="utf-8")
    return 0 if report["status"]=="completed" else 2
if __name__=="__main__": raise SystemExit(main())
