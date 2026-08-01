#!/usr/bin/env python3
"""Strict frozen-source adapter for the approved Script 04 boundary."""
from __future__ import annotations
import argparse, csv, hashlib, json, math, re, subprocess, time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SCRIPT03_HASH = "3CC6A181094350E5E481A045600E6B2DA7BB49B0AF171315111F8BDA1064C73F"
ANNOTATION_HASH = "CB4384A1B52238CF87F46BC2993A07A81092246B9EB1845E1FB94A2C1041A640"
SCRIPT04_HASH = "1150E5F10565B7C25784A9DD875380ACC748C95A8D5BE7B0431C1C5767C587D1"
REFERENCE_HASH = "57BA707C22BAA01A6528613182A017F60FD7AE9A08639FB208D7EFA61194DC11"
EXPECTED_SAMPLES = ["CTRL_R1", "CTRL_R2", "CTRL_R3", "TRT_R1", "TRT_R2", "TRT_R3"]
REQUIRED_ANNOTATION_COLUMNS = [
    "id", "mz", "preferred_annotation:compound_name",
    "preferred_annotation:mol_formula", "preferred_annotation:adduct",
    "preferred_annotation:score", "preferred_annotation:precursor_mz",
    "preferred_annotation:annotation_method", "preferred_annotation:comment",
    "preferred_annotation:neutral_mass", "preferred_annotation:inchi",
    "preferred_annotation:inchi_key", "preferred_annotation:isomeric_smiles",
    "preferred_annotation:smiles",
    "preferred_annotation:classyfire_superclass",
    "preferred_annotation:classyfire_class",
    "preferred_annotation:classyfire_subclass",
    "preferred_annotation:classyfire_parent",
    "preferred_annotation:pubchem_cid",
    "preferred_annotation:compound_annotation_score",
    "preferred_annotation:database_name", "preferred_annotation:mz_diff",
    "preferred_annotation:mz_diff_ppm",
]
EXPECTED_FILES = [
    "analysis_configuration_used.csv", "analysis_validation_summary.csv",
    "annotation_merge_report.txt", "annotation_merge_status_summary.csv",
    "PVL_Single_Negative_All_Annotated_Features.csv",
    "PVL_Single_Negative_Annotated_ThreeReadout_Master.csv",
    "PVL_Single_Negative_Annotated_ThreeReadout_Master.xlsx",
    "PVL_Single_Negative_HighPriority_Annotated_Candidates.csv",
    "PVL_Single_Negative_Quantitative_LowerScore_Annotated_Candidates.csv",
    "PVL_Single_Negative_Quantitative_Unannotated_Candidates.csv",
    "sessionInfo.txt", "04_merge_annotations_three_readouts_used.R",
]

def sha(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for block in iter(lambda: f.read(1024 * 1024), b""):
            h.update(block)
    return h.hexdigest().upper()

def resolve_reference(req):
    """Resolve the approved Script 04 reference without weakening hash validation."""
    reference_value = req.get("regression_reference", {}).get("path") or ""
    if reference_value:
        return Path(reference_value)

    if req.get("ion_mode") != "NEG" or req.get("exposure_label") != "Single":
        return None

    reference_name = "PVL_Single_Negative_Annotated_ThreeReadout_Master.csv"
    search_root = ROOT / "generated-outputs"
    if not search_root.is_dir():
        return None

    matches = []
    for candidate in search_root.rglob(reference_name):
        try:
            if candidate.is_file() and sha(candidate) == REFERENCE_HASH:
                matches.append(candidate)
        except OSError:
            continue

    if not matches:
        return None

    return sorted(matches, key=lambda path: path.as_posix().lower())[0]


def issue(code, message, category="configuration"):
    return {"check_id": code, "status": "FAIL", "message": message,
            "category": category, "blocking": True}

def inspect_annotation(path: Path, ion_mode="NEG"):
    with path.open(encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)
        rows = list(reader); columns = reader.fieldnames or []
    ids = [r.get("id", "") for r in rows]
    mz = [r.get("mz", "") for r in rows]
    names = [r.get("preferred_annotation:compound_name", "") for r in rows]
    scores = [r.get("preferred_annotation:score", "") for r in rows]
    compound_scores = [r.get("preferred_annotation:compound_annotation_score", "") for r in rows]
    adducts = [r.get("preferred_annotation:adduct", "").strip() for r in rows]
    def finite(values):
        result = []
        for value in values:
            if not value.strip(): continue
            try:
                number = float(value)
                if math.isfinite(number): result.append(number)
            except ValueError: pass
        return result
    numeric_mz, numeric_scores, numeric_compound = finite(mz), finite(scores), finite(compound_scores)
    score_disagreement = sum(1 for a, b in zip(scores, compound_scores)
        if a.strip() and b.strip() and abs(float(a)-float(b)) > 1e-6)
    return {"rows": len(rows), "columns": len(columns), "column_names": columns,
        "duplicate_feature_ids": len(ids)-len(set(ids)),
        "duplicate_mz": len(mz)-len(set(mz)),
        "empty_feature_ids": sum(not x.strip() for x in ids),
        "nonnumeric_mz": len([x for x in mz if x.strip()])-len(numeric_mz),
        "missing_annotation_name": sum(not x.strip() for x in names),
        "missing_score": sum(not x.strip() for x in scores),
        "nonnumeric_score": len([x for x in scores if x.strip()])-len(numeric_scores),
        "score_min": min(numeric_scores) if numeric_scores else None,
        "score_max": max(numeric_scores) if numeric_scores else None,
        "score_disagreement_count": score_disagreement,
        "adduct_values": sorted(set(x for x in adducts if x)),
        "unexpected_adduct_count": sum(bool(x) and x not in (
            {"[M+H]+","[M+Na]+","[M+K]+"} if ion_mode=="POS" else {"[M-H]-"})
            for x in adducts),
        "encoding": "UTF-8 compatible (read as UTF-8-SIG)", "delimiter": ",",
        "missing_required_columns": sorted(set(REQUIRED_ANNOTATION_COLUMNS)-set(columns))}

def validate_request(req, check_environment=True):
    problems = []
    for field, expected in {"stage_id":"integrate_mzmine_annotations",
        "scientific_core_version":"1.0.0"}.items():
        if req.get(field) != expected:
            problems.append(issue(f"scope.{field}", f"{field} must equal {expected}.", "scientific"))
    if req.get("exposure_label") not in ("Single","Sequential"):
        problems.append(issue("scope.exposure_label",
            "exposure_label must equal Single or Sequential.", "scientific"))
    if req.get("ion_mode") not in ("NEG","POS"):
        problems.append(issue("scope.ion_mode", "ion_mode must equal NEG or POS.", "scientific"))
    if req.get("scientific_profile", {}).get("profile_id") not in (
        "pvl-fullpixel-reference-v1", "fullpixel-40um-1600px-v1",
        "pvl-positive-technical-candidate-v1"):
        problems.append(issue("profile.unsupported", "A registered PVL technical profile is required.", "scientific"))
    if [x.get("sample_id") for x in req.get("samples", [])] != EXPECTED_SAMPLES:
        problems.append(issue("profile.samples", "Exact ordered six-sample mapping is required.", "scientific"))
    if len(req.get("conditions", [])) != 2:
        problems.append(issue("profile.conditions", "Exactly two conditions are required.", "scientific"))
    if len(req.get("replicates", [])) != 6:
        problems.append(issue("profile.replicates", "Exactly six replicate mappings are required.", "scientific"))
    frozen = ROOT / req.get("frozen_script", {}).get("repository_relative_path", "")
    if not frozen.is_file() or sha(frozen) != SCRIPT04_HASH or \
       req.get("frozen_script", {}).get("sha256", "").upper() != SCRIPT04_HASH:
        problems.append(issue("integrity.frozen_script", "Frozen Script 04 identity failed.", "scientific"))
    q = req.get("registered_inputs", {}).get("replicate_summary_artifact", {})
    qp = Path(q.get("path", ""))
    if q.get("project_id") != req.get("project_id") or q.get("dataset_id") != req.get("dataset_id"):
        problems.append(issue("input.script03_scope", "Script 03 artifact must belong to the same project and dataset.", "scientific"))
    if q.get("stale_status", False):
        problems.append(issue("input.script03_stale", "Script 03 artifact is stale.", "scientific"))
    if not qp.is_file():
        problems.append(issue("input.script03_missing", "Script 03 artifact is missing.", "input"))
    elif sha(qp) != q.get("sha256", "").upper():
        problems.append(issue("input.script03_hash", "Script 03 artifact hash differs from its registered identity.", "scientific"))
    elif req.get("ion_mode")=="NEG" and req.get("exposure_label")=="Single" and sha(qp) != SCRIPT03_HASH:
        problems.append(issue("input.script03_hash", "Negative Script 03 artifact hash is not approved.", "scientific"))
    if q.get("active_status") != "active" or q.get("validation_status") != "PASS":
        problems.append(issue("input.script03_inactive", "Script 03 artifact must be active and validated.", "scientific"))
    ann = req.get("registered_inputs", {}).get("annotation_artifact", {})
    ap = Path(ann.get("path", ""))
    if ann.get("project_id") != req.get("project_id") or ann.get("dataset_id") != req.get("dataset_id"):
        problems.append(issue("input.annotation_scope", "Annotation artifact must belong to the same project and dataset.", "scientific"))
    if ann.get("active_status") != "active" or ann.get("validation_status") != "PASS" or ann.get("stale_status", False):
        problems.append(issue("input.annotation_inactive", "Annotation artifact must be active, validated, and not stale.", "scientific"))
    if not ap.is_file():
        problems.append(issue("input.annotation_missing", "Annotated MZmine input is missing.", "input"))
    else:
        if sha(ap) != ann.get("sha256", "").upper():
            problems.append(issue("input.annotation_hash", "Annotation input hash differs from its registered identity.", "scientific"))
        elif req.get("ion_mode")=="NEG" and req.get("exposure_label")=="Single" and sha(ap) != ANNOTATION_HASH:
            problems.append(issue("input.annotation_hash", "Negative annotation input hash differs.", "scientific"))
        profile = inspect_annotation(ap, req.get("ion_mode"))
        for name in profile["missing_required_columns"]:
            problems.append(issue("annotation.column_missing", f"Required annotation column missing: {name}", "input"))
        for key, code in [("duplicate_feature_ids","annotation.duplicate_id"),
                          ("empty_feature_ids","annotation.empty_id"),
                          ("nonnumeric_mz","annotation.mz_numeric"),
                          ("nonnumeric_score","annotation.score_numeric"),
                          ("score_disagreement_count","annotation.score_consistency"),
                          ("unexpected_adduct_count","annotation.adduct")]:
            if profile[key]:
                problems.append(issue(code, f"Annotation validation failed: {key}={profile[key]}", "scientific"))
    mappings = req.get("annotation_column_mappings", {})
    required_roles = {"feature_id":"id", "mz":"mz",
        "annotation_name":"preferred_annotation:compound_name",
        "formula":"preferred_annotation:mol_formula",
        "adduct":"preferred_annotation:adduct",
        "score":"preferred_annotation:score",
        "compound_score":"preferred_annotation:compound_annotation_score",
        "database":"preferred_annotation:database_name"}
    for role, expected in required_roles.items():
        if mappings.get(role) != expected:
            problems.append(issue(f"mapping.{role}", f"{role} must explicitly map to {expected}.", "input"))
    assigned = list(mappings.values())
    if len(assigned) != len(set(assigned)):
        problems.append(issue("mapping.duplicate_role", "One column cannot fill multiple restricted roles.", "input"))
    reference = resolve_reference(req)
    expected_reference_hash = (
        req.get("regression_reference", {}).get("sha256") or
        (REFERENCE_HASH
         if req.get("ion_mode") == "NEG" and req.get("exposure_label") == "Single"
         else "")
    ).upper()
    if req.get("ion_mode")=="NEG" and req.get("exposure_label")=="Single" and (reference is None or not reference.is_file()):
        problems.append(issue("reference.missing", "Script 04 reference is unavailable.", "input"))
    elif reference is not None and reference.is_file() and expected_reference_hash and sha(reference) != expected_reference_hash:
        problems.append(issue("reference.hash", "Script 04 reference hash differs.", "scientific"))
    workspace = (ROOT / req.get("output_working_directory", "")).resolve()
    if workspace.exists() and any(workspace.iterdir()):
        problems.append(issue("workspace.not_empty", "Workspace must be absent or empty.", "infrastructure"))
    if check_environment:
        rex = Path(req.get("environment_requirements", {}).get("r_executable", ""))
        if not rex.is_file():
            problems.append(issue("environment.r", "Configured Rscript is unavailable.", "environment"))
        else:
            expr = "quit(status=if(all(vapply(c('data.table','openxlsx'),requireNamespace,logical(1),quietly=TRUE)))0 else 9)"
            result = subprocess.run([str(rex),"-e",expr],capture_output=True,text=True)
            if result.returncode:
                problems.append(issue("environment.packages", "Required packages are unavailable.", "environment"))
    return problems

def replace_once(text, pattern, replacement, label):
    updated, count = re.subn(pattern, replacement, text, flags=re.MULTILINE)
    if count != 1:
        raise RuntimeError(f"Approved source anchor {label!r} occurred {count} times.")
    return updated, {"field":label,"occurrences":count}

def make_copy(req, workspace):
    source = ROOT / req["frozen_script"]["repository_relative_path"]
    original = source.read_text(encoding="utf-8"); generated = original; changes=[]
    dataset_id=f"PVL_{req.get('exposure_label','Single')}_{'Positive' if req.get('ion_mode')=='POS' else 'Negative'}"
    substitutions=[
      (r'^dataset_id <- "PVL_Single_Negative"$',f'dataset_id <- "{dataset_id}"',"dataset_id"),
      (r'^quantitative_csv <-\r?\n  "[^"]+"$',
       f'quantitative_csv <-\n  "{Path(req["registered_inputs"]["replicate_summary_artifact"]["path"]).as_posix()}"',"quantitative_csv"),
      (r'^annotation_csv <-\r?\n  "[^"]+"$',
       f'annotation_csv <-\n  "{Path(req["registered_inputs"]["annotation_artifact"]["path"]).as_posix()}"',"annotation_csv"),
      (r'^analysis_root <- "[^"]+"$',f'analysis_root <- "{workspace.as_posix()}"',"analysis_root"),
      (r'^expected_ion_mode <- "Negative"$',
       f'expected_ion_mode <- "{"Positive" if req.get("ion_mode")=="POS" else "Negative"}"',"expected_ion_mode"),
      (r'^accepted_adducts <- c\(.*\)$',
       'accepted_adducts <- c("[M+H]+", "[M+Na]+", "[M+K]+")' if req.get("ion_mode")=="POS" else 'accepted_adducts <- c("[M-H]-")',"accepted_adducts")]
    for pattern,replacement,label in substitutions:
        generated,record=replace_once(generated,pattern,replacement,label); changes.append(record)
    boundary='required_packages <- c("data.table", "openxlsx")'
    if original.count(boundary)!=1 or generated.count(boundary)!=1:
        raise RuntimeError("Scientific-body boundary is not unique.")
    ob=original[original.index(boundary):]; gb=generated[generated.index(boundary):]
    if ob!=gb: raise RuntimeError("Scientific body changed outside configuration.")
    workspace.mkdir(parents=True,exist_ok=False)
    path=workspace/"execution_copy_script04.R"; path.write_text(generated,encoding="utf-8",newline="")
    record={"schema_version":"1.0.0","source_hash":sha(source),
      "execution_copy_hash":sha(path),"substitutions":changes,
      "scientific_body_hash":hashlib.sha256(ob.encode()).hexdigest().upper(),
      "scientific_body_unchanged":True}
    (workspace/"configuration_transformations.json").write_text(json.dumps(record,indent=2))
    return path,record

def read_csv(path):
    with path.open(encoding="utf-8-sig",newline="") as f:
        r=csv.DictReader(f); rows=list(r); return r.fieldnames or [],rows

def number(value):
    if value=="": return None
    try:return float(value)
    except ValueError:return None

def compare_fields(actual, expected, fields, key, abs_tol=1e-12, rel_tol=1e-12):
    ah,ar=read_csv(actual); eh,er=read_csv(expected)
    amap={r[key]:r for r in ar}; emap={r[key]:r for r in er}
    failed=[]; max_abs=max_rel=0.0; missing=0
    if set(amap)!=set(emap): failed.append({"reason":"row identity mismatch"})
    for rid in sorted(set(amap)&set(emap)):
      for field in fields:
        x,y=amap[rid].get(field),emap[rid].get(field)
        if x==y: continue
        nx,ny=number(x),number(y)
        if nx is None or ny is None:
            if (x=="") != (y==""): missing+=1
            failed.append({"row":rid,"column":field,"actual":x,"expected":y}); continue
        if math.isnan(nx) or math.isnan(ny): equal=math.isnan(nx) and math.isnan(ny); d=rel=0
        elif math.isinf(nx) or math.isinf(ny): equal=nx==ny; d=rel=0
        else:
            d=abs(nx-ny); rel=d/max(abs(ny),1e-300)
            max_abs=max(max_abs,d); max_rel=max(max_rel,rel)
            equal=d<=abs_tol+rel_tol*abs(ny)
        if not equal and len(failed)<200:
            failed.append({"row":rid,"column":field,"absolute_difference":d,"relative_difference":rel})
    return {"status":"PASS" if not failed else "FAIL","fields_tested":len(fields),
      "maximum_absolute_difference":max_abs,"maximum_relative_difference":max_rel,
      "changed_cells":failed,"missingness_differences":missing,
      "absolute_tolerance":abs_tol,"relative_tolerance":rel_tol}

def csv_shape(path):
    h,r=read_csv(path); return len(r),len(h),h

def main():
    p=argparse.ArgumentParser(); p.add_argument("request"); p.add_argument("--report",required=True); args=p.parse_args()
    req=json.loads(Path(args.request).read_text()); rp=Path(args.report)
    report={"schema_version":"1.0.0","stage_id":req.get("stage_id"),
      "run_id":req.get("run_id"),"status":"preflight_failed","execution_started":False,
      "validation_records":[],"produced_outputs":[]}
    problems=validate_request(req); report["validation_records"]=problems
    if problems: rp.parent.mkdir(parents=True,exist_ok=True); rp.write_text(json.dumps(report,indent=2)); return 2
    workspace=(ROOT/req["output_working_directory"]).resolve()
    try:
      copy,trans=make_copy(req,workspace); report.update(frozen_script_hash=SCRIPT04_HASH,
        execution_copy_hash=sha(copy),configuration_transformations=trans)
      out,err=workspace/"stdout.log",workspace/"stderr.log"
      report.update(execution_started=True,stdout_path=str(out),stderr_path=str(err),
        status="running",started_at=time.strftime("%Y-%m-%dT%H:%M:%SZ",time.gmtime()))
      rp.parent.mkdir(parents=True,exist_ok=True); rp.write_text(json.dumps(report,indent=2))
      rex=req["environment_requirements"]["r_executable"]
      with out.open("w") as o,err.open("w") as e:
        proc=subprocess.run([rex,str(copy)],cwd=workspace,stdout=o,stderr=e)
      report["process_exit_status"]=proc.returncode
      if proc.returncode: raise RuntimeError("Frozen Script 04 exited unsuccessfully.")
      dataset_id=f"PVL_{req.get('exposure_label','Single')}_{'Positive' if req.get('ion_mode')=='POS' else 'Negative'}"
      expected_files=[name.replace("PVL_Single_Negative",dataset_id) for name in EXPECTED_FILES]
      parent=workspace/"04_Annotation_Merge"/dataset_id
      dirs=[x for x in parent.glob("AnnotationMerge_*") if x.is_dir()]
      if len(dirs)!=1: raise RuntimeError(f"Expected one Script 04 output directory; found {len(dirs)}.")
      od=dirs[0]; missing=[n for n in expected_files if not (od/n).is_file()]
      if missing: raise RuntimeError("Missing expected outputs: "+", ".join(missing))
      master=od/f"{dataset_id}_Annotated_ThreeReadout_Master.csv"
      qpath=Path(req["registered_inputs"]["replicate_summary_artifact"]["path"])
      qfields, qrows=read_csv(qpath)
      preservation=compare_fields(master,qpath,qfields,"mzmine_row_id")
      mh,mrows=read_csv(master)
      ref = resolve_reference(req)
      annotation_fields=[x for x in mh if x not in qfields]
      annotation_eq=compare_fields(master,ref,annotation_fields,"mzmine_row_id") if ref is not None and ref.is_file() else {"status":"NOT_EVALUABLE","reason":"No approved Positive annotation oracle.","changed_cells":[],"maximum_absolute_difference":None,"maximum_relative_difference":None}
      reference_all=compare_fields(master,ref,mh,"mzmine_row_id") if ref is not None and ref.is_file() else {"status":"NOT_EVALUABLE","reason":"No approved Positive complete oracle.","changed_cells":[],"maximum_absolute_difference":None,"maximum_relative_difference":None}
      expected_rows=(2558 if req.get("ion_mode")=="POS" else 1244) if req.get("exposure_label")=="Single" else len(qrows)
      row_retention={"status":"PASS" if len(mrows)==len(qrows)==expected_rows and
        {x["mzmine_row_id"] for x in mrows}=={x["mzmine_row_id"] for x in qrows} else "FAIL",
        "quantitative_rows":len(qrows),"output_rows":len(mrows),
        "unique_output_rows":len({x["mzmine_row_id"] for x in mrows})}
      report.update(quantitative_preservation=preservation,
        annotation_equivalence=annotation_eq,file_equivalence=reference_all,
        row_retention=row_retention,
        schema={"status":"PASS" if len(mh)==129 else "FAIL",
                "rows":len(mrows),"columns":len(mh),"column_names":mh})
      outputs=[]
      for name in expected_files:
        path=od/name; rows=cols=None
        if path.suffix.lower()==".csv":rows,cols,_=csv_shape(path)
        outputs.append({"artifact_role":name,"path":str(path),"sha256":sha(path),
          "bytes":path.stat().st_size,"rows":rows,"columns":cols})
      report["produced_outputs"]=outputs
      gates=[preservation["status"],row_retention["status"],report["schema"]["status"]]
      if req.get("ion_mode")=="NEG" and req.get("exposure_label")=="Single": gates.extend([annotation_eq["status"],reference_all["status"]])
      if any(x!="PASS" for x in gates): raise RuntimeError("Script 04 equivalence gate failed.")
      report.update(status="completed",completed_at=time.strftime("%Y-%m-%dT%H:%M:%SZ",time.gmtime()),
        recommended_correction=None,validation_records=[{"check_id":"script04.regression",
        "status":"PASS","message":"Quantitative preservation, row retention, and schema pass; complete Positive historical equivalence remains separately classified." if req.get("ion_mode")=="POS" else "Quantitative preservation, annotation equivalence, rows and schema pass.",
        "category":"scientific","blocking":False}])
      rp.write_text(json.dumps(report,indent=2)); return 0
    except Exception as exc:
      report.update(status="failed",error=str(exc),
        recommended_correction="Inspect preserved Script 04 diagnostics; do not activate outputs.")
      rp.parent.mkdir(parents=True,exist_ok=True); rp.write_text(json.dumps(report,indent=2)); return 1
if __name__=="__main__": raise SystemExit(main())
