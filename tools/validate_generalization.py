#!/usr/bin/env python3
"""Infrastructure-only tests for generalized dataset configuration."""
import copy, json, sys
from pathlib import Path
from validate_contracts import load, validate

ROOT = Path(__file__).resolve().parents[1]
SCHEMA = load(ROOT / "config" / "schemas" / "dataset_registration.schema.json")

def make_case(case_id, groups, exposure="User exposure", pixel=75, ion="NEG"):
    conditions, samples, pairs, comparisons = [], [], [], []
    order = 0
    reference_id = next(iter(groups))
    for ci, (condition_id, count) in enumerate(groups.items(), 1):
        conditions.append({"condition_id":condition_id,"condition_label":condition_id.replace("-"," ").title(),
          "analytical_role":"reference" if ci == 1 else "comparison","display_order":ci,
          "is_reference":ci == 1,"treatment_label":None if ci == 1 else condition_id,"dose":None})
        for ri in range(1, count + 1):
            order += 1
            sample_id = f"{condition_id}-sample-{ri}"
            replicate_id = f"bio-{ri}"
            samples.append({"sample_id":sample_id,"condition_id":condition_id,"replicate_id":replicate_id,
              "display_label":sample_id,"inclusion_status":"included","sample_order":order,"acquisition_metadata":{}})
            pairs.append({"sample_id":sample_id,"imzml_path":f"X:/{case_id}/{sample_id}.imzML","ibd_path":f"X:/{case_id}/{sample_id}.ibd"})
        if ci > 1:
            comparisons.append({"comparison_id":f"cmp-{condition_id}","comparison_label":f"{condition_id} versus {reference_id}",
              "reference_condition_id":reference_id,"comparison_condition_id":condition_id,"enabled":True,
              "display_order":ci-1,"profile_compatibility_status":"unresolved"})
    return {"schema_version":"1.0.0","dataset_id":f"dset-{case_id}","project_id":"prj-generalization",
      "dataset_name":case_id,"ion_mode":ion,"exposure_label":exposure,
      "raw_data_root":{"path":f"X:/{case_id}","path_type":"external_absolute"},"raw_data_pairs":pairs,
      "aligned_feature_list":{"path":f"X:/{case_id}/targets.csv","path_type":"external_absolute","role":"aligned_target_mz_list","sha256":None},
      "annotated_feature_list":None,"conditions":conditions,"samples":samples,"comparisons":comparisons,
      "acquisition_metadata":{"matrix":"user-matrix","pixel_size_um":pixel,"declared_coordinate_count":None,"observed_coordinate_count":None,"image_dimensions":None},
      "column_mappings":{"target_mz":"custom_mz","feature_id":"custom_feature","annotation":None,"additional":{}},
      "output_location":{"path":f"analysis/{case_id}","path_type":"project_relative"},"artifact_retention":{},
      "registration_status":"draft","validation_status":"NOT_EVALUABLE","scientific_profile_compatibility":"unresolved",
      "source_artifact_hashes":{},"path_portability":"machine_specific"}

def semantic_errors(d):
    errors = []
    condition_ids = [x["condition_id"] for x in d.get("conditions",[])]
    sample_ids = [x["sample_id"] for x in d.get("samples",[])]
    if len(condition_ids) != len(set(condition_ids)): errors.append("duplicate condition_id")
    if len(sample_ids) != len(set(sample_ids)): errors.append("duplicate sample_id")
    references = [x for x in d.get("conditions",[]) if x.get("is_reference")]
    if len(references) != 1: errors.append("exactly one reference condition required")
    condition_set = set(condition_ids)
    pair_samples = {x["sample_id"] for x in d.get("raw_data_pairs",[])}
    seen_replicates = set()
    counts = {x:0 for x in condition_ids}
    for sample in d.get("samples",[]):
        cid, rid = sample.get("condition_id"), sample.get("replicate_id")
        if cid not in condition_set: errors.append(f"sample {sample.get('sample_id')} references missing condition")
        else: counts[cid] += 1
        if not rid: errors.append(f"sample {sample.get('sample_id')} missing replicate_id")
        key = (cid, rid)
        if key in seen_replicates: errors.append(f"duplicate replicate_id {rid} in condition {cid}")
        seen_replicates.add(key)
        if sample.get("sample_id") not in pair_samples: errors.append(f"sample {sample.get('sample_id')} missing raw pair")
    for cid, count in counts.items():
        if count == 0: errors.append(f"condition {cid} has no samples")
    for comparison in d.get("comparisons",[]):
        ref, comp = comparison.get("reference_condition_id"), comparison.get("comparison_condition_id")
        if ref not in condition_set: errors.append(f"comparison {comparison.get('comparison_id')} references missing reference condition")
        if comp not in condition_set: errors.append(f"comparison {comparison.get('comparison_id')} references missing comparison condition")
        if ref == comp: errors.append(f"comparison {comparison.get('comparison_id')} compares a condition with itself")
    return errors

def pvl_compatibility(d):
    wanted_conditions = {"CTRL","TRT"}
    wanted_samples = {"CTRL_R1","CTRL_R2","CTRL_R3","TRT_R1","TRT_R2","TRT_R3"}
    observed_conditions = {x["condition_id"] for x in d["conditions"]}
    observed_samples = {x["sample_id"] for x in d["samples"]}
    return "compatible" if observed_conditions == wanted_conditions and observed_samples == wanted_samples and len(d["samples"]) == 6 else "incompatible"

def main():
    cases = {
      "A_multiple_treatments": make_case("multi-treatment",{"control":3,"treatment-a":3,"treatment-b":3,"treatment-c":3}),
      "B_more_replicates": make_case("six-replicates",{"control":6,"treatment-a":6}),
      "C_unequal_replicates": make_case("unequal",{"control":4,"treatment-a":5,"treatment-b":3}),
      "D_custom_names": make_case("custom-names",{"vehicle":2,"lino-phe":2,"ole-dop":2,"a-lino-etha":2,"tug-891":2},exposure="Pulse train",pixel=25),
      "E_three_conditions_four_each": make_case("three-by-four",{"healthy":4,"disease":4,"disease-treated":4},exposure="Longitudinal batch",pixel=60)
    }
    failed = []
    for name, case in cases.items():
        structural = validate(case, SCHEMA, SCHEMA)
        semantic = semantic_errors(case)
        if structural or semantic: failed.append(f"{name}: {structural + semantic}")
        else:
            comparisons = len(case["comparisons"])
            print(f"PASS {name}: {len(case['conditions'])} conditions, {len(case['samples'])} samples, {comparisons} comparisons; PVL compatibility={pvl_compatibility(case)}")
    base = cases["A_multiple_treatments"]
    invalids = {}
    x=copy.deepcopy(base); x["samples"][1]["sample_id"]=x["samples"][0]["sample_id"]; invalids["duplicate sample IDs"]=(x,"duplicate sample_id")
    x=copy.deepcopy(base); empty=x["conditions"][1]["condition_id"]; x["samples"]=[s for s in x["samples"] if s["condition_id"]!=empty]; invalids["treatment with no samples"]=(x,"has no samples")
    x=copy.deepcopy(base); x["samples"][0]["condition_id"]="absent"; invalids["sample without condition"]=(x,"references missing condition")
    x=copy.deepcopy(base); x["samples"][1]["replicate_id"]=x["samples"][0]["replicate_id"]; invalids["duplicate replicate IDs"]=(x,"duplicate replicate_id")
    x=copy.deepcopy(base); x["comparisons"][0]["comparison_condition_id"]="absent"; invalids["comparison missing condition"]=(x,"references missing comparison condition")
    x=copy.deepcopy(base); x["samples"][0]["replicate_id"]=""; invalids["missing replicate mapping"]=(x,"missing replicate_id")
    for name,(case,expected) in invalids.items():
        errors=semantic_errors(case)
        if not any(expected in e for e in errors): failed.append(f"invalid {name} did not report {expected}: {errors}")
        else: print(f"PASS invalid {name}: precise failure detected")
    pvl = load(ROOT / "config" / "examples" / "pvl_single_reference_dataset.example.json")
    if validate(pvl,SCHEMA,SCHEMA) or semantic_errors(pvl) or pvl_compatibility(pvl)!="compatible":
        failed.append("PVL reference case failed structural/profile test")
    else: print("PASS PVL reference: structurally valid and PVL-profile compatible")
    if failed:
        print("\n".join(failed),file=sys.stderr); return 1
    print("PASS: generic schemas impose no fixed condition, treatment, replicate, sample, exposure, pixel-size, or name limit.")
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
