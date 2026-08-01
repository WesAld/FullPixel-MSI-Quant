#!/usr/bin/env python3
"""Validate contract examples without executing scientific code."""
import json, re, sys
from datetime import datetime
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SCHEMAS = ROOT / "config" / "schemas"
EXAMPLES = ROOT / "config" / "examples"
INVALID = ROOT / "tests" / "contract_examples" / "invalid"

def resolve_ref(schema, ref):
    node = schema
    for part in ref.removeprefix("#/").split("/"):
        node = node[part]
    return node

def validate(value, rule, root, path="$"):
    errors = []
    if "$ref" in rule:
        return validate(value, resolve_ref(root, rule["$ref"]), root, path)
    if "anyOf" in rule:
        branches = [validate(value, r, root, path) for r in rule["anyOf"]]
        return [] if any(not x for x in branches) else [f"{path}: no anyOf branch matched"]
    types = rule.get("type")
    if types:
        types = [types] if isinstance(types, str) else types
        checks = {"object":lambda x:isinstance(x,dict),"array":lambda x:isinstance(x,list),
                  "string":lambda x:isinstance(x,str),"integer":lambda x:isinstance(x,int) and not isinstance(x,bool),
                  "number":lambda x:isinstance(x,(int,float)) and not isinstance(x,bool),
                  "boolean":lambda x:isinstance(x,bool),"null":lambda x:x is None}
        if not any(checks[t](value) for t in types):
            return [f"{path}: expected {types}, got {type(value).__name__}"]
    if "const" in rule and value != rule["const"]: errors.append(f"{path}: const mismatch")
    if "enum" in rule and value not in rule["enum"]: errors.append(f"{path}: not in controlled vocabulary")
    if isinstance(value, dict):
        for key in rule.get("required", []):
            if key not in value: errors.append(f"{path}: missing required field {key}")
        props = rule.get("properties", {})
        if rule.get("additionalProperties") is False:
            for key in value:
                if key not in props: errors.append(f"{path}: unexpected field {key}")
        for key, child in props.items():
            if key in value: errors += validate(value[key], child, root, f"{path}.{key}")
    if isinstance(value, list):
        if len(value) < rule.get("minItems", 0): errors.append(f"{path}: too few items")
        if rule.get("uniqueItems") and len({json.dumps(x,sort_keys=True) for x in value}) != len(value):
            errors.append(f"{path}: items are not unique")
        if "items" in rule:
            for i, child in enumerate(value): errors += validate(child, rule["items"], root, f"{path}[{i}]")
    if isinstance(value, str):
        if len(value) < rule.get("minLength", 0): errors.append(f"{path}: string too short")
        if "pattern" in rule and not re.search(rule["pattern"], value): errors.append(f"{path}: pattern mismatch")
        if rule.get("format") == "date-time":
            try: datetime.fromisoformat(value.replace("Z","+00:00"))
            except ValueError: errors.append(f"{path}: invalid date-time")
    if isinstance(value, (int,float)) and not isinstance(value,bool) and "minimum" in rule and value < rule["minimum"]:
        errors.append(f"{path}: below minimum")
    return errors

def load(path):
    with path.open(encoding="utf-8") as handle:
        return json.load(handle)

def main():
    failures = []
    schemas = {}
    for path in sorted(SCHEMAS.glob("*.schema.json")):
        try: schemas[path.name.removesuffix(".schema.json")] = load(path)
        except Exception as exc: failures.append(f"SCHEMA SYNTAX FAIL {path}: {exc}")
    pairs = {
        "project_manifest":"project_manifest", "dataset_registration":"dataset_registration",
        "run_manifest":"run_manifest", "artifact":"artifact",
        "validation_record":"validation_record", "log_event":"log_event",
        "provenance_edge":"provenance_edge"
    }
    for example, schema_name in pairs.items():
        schema = schemas[schema_name]
        valid_path = EXAMPLES / f"{example}.example.json"
        invalid_path = INVALID / f"{example}.invalid.json"
        valid_errors = validate(load(valid_path), schema, schema)
        invalid_errors = validate(load(invalid_path), schema, schema)
        if valid_errors: failures.append(f"VALID EXAMPLE FAIL {valid_path}: {'; '.join(valid_errors)}")
        else: print(f"PASS valid:   {valid_path.relative_to(ROOT)}")
        if not invalid_errors: failures.append(f"INVALID EXAMPLE UNEXPECTEDLY PASSED {invalid_path}")
        else: print(f"PASS invalid: {invalid_path.relative_to(ROOT)} ({len(invalid_errors)} expected errors)")
    for doc in [ROOT/"docs"/"WORKFLOW_CONTRACTS.md", ROOT/"docs"/"PATH_POLICY.md", ROOT/"docs"/"SUBPROCESS_BOUNDARY.md"]:
        if not doc.exists(): failures.append(f"MISSING REPOSITORY REFERENCE {doc}")
    if failures:
        print("\n".join(failures), file=sys.stderr)
        return 1
    print(f"PASS: {len(schemas)} schemas parsed; all paired examples behaved as expected.")
    return 0

if __name__ == "__main__":
    raise SystemExit(main())

