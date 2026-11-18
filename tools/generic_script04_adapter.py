#!/usr/bin/env python3
"""Generic two-group adapter for the validated Script 04 annotation merge."""
from __future__ import annotations
import argparse, csv, hashlib, json, math, subprocess
from pathlib import Path
from subprocess_runtime import run_external_process

ROOT=Path(__file__).resolve().parents[1]
SCRIPT_HASH="1150E5F10565B7C25784A9DD875380ACC748C95A8D5BE7B0431C1C5767C587D1"
BOUNDARY='\n###############################################################################\n\nrequired_packages <- c("data.table", "openxlsx")'
REQUIRED_ANNOTATION_COLUMNS=[
 "id","mz","preferred_annotation:compound_name","preferred_annotation:mol_formula",
 "preferred_annotation:adduct","preferred_annotation:score","preferred_annotation:precursor_mz",
 "preferred_annotation:annotation_method","preferred_annotation:comment","preferred_annotation:neutral_mass",
 "preferred_annotation:inchi","preferred_annotation:inchi_key","preferred_annotation:isomeric_smiles",
 "preferred_annotation:smiles","preferred_annotation:classyfire_superclass","preferred_annotation:classyfire_class",
 "preferred_annotation:classyfire_subclass","preferred_annotation:classyfire_parent","preferred_annotation:pubchem_cid",
 "preferred_annotation:compound_annotation_score","preferred_annotation:database_name",
 "preferred_annotation:mz_diff","preferred_annotation:mz_diff_ppm"]

def sha(path:Path)->str:
 h=hashlib.sha256()
 with path.open('rb') as f:
  for b in iter(lambda:f.read(1024*1024),b''): h.update(b)
 return h.hexdigest().upper()
def issue(code,message,category='configuration'):
 return {'check_id':code,'status':'FAIL','category':category,'message':message,'blocking':True}
def rquote(value): return '"'+str(value).replace('\\','/').replace('"','\\"')+'"'
def rvec(values): return 'c('+', '.join(rquote(v) for v in values)+')'
def read_csv(path):
 with Path(path).open(encoding='utf-8-sig',newline='') as f:
  r=csv.DictReader(f); rows=list(r); return r.fieldnames or [],rows
def number(value):
 try:
  if value is None or str(value).strip()=='': return None
  x=float(value); return x if math.isfinite(x) else None
 except (TypeError,ValueError): return None

def validate_request(req,check_environment=True):
 problems=[]
 if req.get('stage_id')!='integrate_mzmine_annotations':
  problems.append(issue('stage.unsupported','Expected integrate_mzmine_annotations.','scientific'))
 q=req.get('registered_inputs',{}).get('replicate_summary_artifact',{})
 qp=Path(q.get('path',''))
 if not qp.is_file(): problems.append(issue('input.script03','Script 03 summary is missing.','input'))
 elif sha(qp)!=str(q.get('sha256','')).upper(): problems.append(issue('input.script03_hash','Script 03 summary hash differs.','scientific'))
 elif q.get('active_status')!='active' or q.get('validation_status')!='PASS': problems.append(issue('input.script03_inactive','Script 03 summary must be active and validated.','scientific'))
 ann=req.get('registered_inputs',{}).get('annotation_artifact',{})
 ap=Path(ann.get('path',''))
 if not ap.is_file(): problems.append(issue('input.annotation','Annotated MZmine table is missing.','input'))
 elif sha(ap)!=str(ann.get('sha256','')).upper(): problems.append(issue('input.annotation_hash','Annotation input hash differs.','scientific'))
 elif ann.get('active_status')!='active' or ann.get('validation_status')!='PASS': problems.append(issue('input.annotation_inactive','Annotation input must be active and validated.','scientific'))
 else:
  header,rows=read_csv(ap); missing=sorted(set(REQUIRED_ANNOTATION_COLUMNS)-set(header))
  if missing: problems.append(issue('input.annotation_schema','Required canonical MZmine columns are missing: '+', '.join(missing),'input'))
  ids=[r.get('id','') for r in rows]
  if any(not str(x).strip() for x in ids) or len(ids)!=len(set(ids)):
   problems.append(issue('input.annotation_ids','Annotation feature IDs must be nonblank and unique.','scientific'))
 frozen=ROOT/req.get('frozen_script',{}).get('repository_relative_path','')
 if not frozen.is_file() or sha(frozen)!=SCRIPT_HASH: problems.append(issue('integrity.frozen_script','Validated Script 04 identity failed.','infrastructure'))
 workspace=Path(req.get('output_working_directory',''))
 if workspace.exists() and any(workspace.iterdir()): problems.append(issue('workspace.not_empty','Workspace must be absent or empty.','infrastructure'))
 if check_environment:
  rex=Path(req.get('environment_requirements',{}).get('r_executable',''))
  if not rex.is_file(): problems.append(issue('environment.r','Configured Rscript is unavailable.','environment'))
 return problems

def make_copy(req,workspace):
 source=ROOT/req['frozen_script']['repository_relative_path']; original=source.read_text(encoding='utf-8')
 start=original.index('dataset_id <- "PVL_Single_Negative"'); end=original.index(BOUNDARY)
 analysis=(workspace/'analysis').resolve(); mode='Positive' if req['ion_mode']=='POS' else 'Negative'
 ap=Path(req['registered_inputs']['annotation_artifact']['path']); header,rows=read_csv(ap)
 adducts=sorted({str(r.get('preferred_annotation:adduct','')).strip() for r in rows if str(r.get('preferred_annotation:adduct','')).strip()})
 if not adducts: adducts=['[M+H]+'] if req['ion_mode']=='POS' else ['[M-H]-']
 panel=f'''dataset_id <- {rquote(req["dataset_id"])}

quantitative_csv <-
  {rquote(req["registered_inputs"]["replicate_summary_artifact"]["path"])}

annotation_csv <-
  {rquote(ap.as_posix())}

analysis_root <- {rquote(analysis.as_posix())}

output_parent <- file.path(
  analysis_root,
  "04_Annotation_Merge",
  dataset_id
)

expected_ion_mode <- {rquote(mode)}

control_condition_label <- "CTRL"
treatment_condition_label <- "TRT"
output_treatment_label <- "TRT"

accepted_adducts <- {rvec(adducts)}

mz_alignment_tolerance_da <- 0.00001
annotation_score_consistency_tolerance <- 0.000001

annotation_score_thresholds <- c(
  0.6,
  0.7,
  0.8
)

minimum_annotation_score_for_high_priority <- 0.6

supporting_local_abs_log2fc_threshold <- 1
supporting_detection_fraction_difference_threshold <- 0.10

write_excel_output <- TRUE
'''
 generated=original[:start]+panel+original[end:]
 workspace.mkdir(parents=True,exist_ok=False); path=workspace/'execution_copy_generic_script04.R'
 path.write_text(generated,encoding='utf-8',newline='\n')
 rec={'source_hash':sha(source),'execution_copy_hash':sha(path),'configuration_boundary':'request-driven',
      'accepted_adducts':adducts,'scientific_body_unchanged':True}
 (workspace/'configuration_transformations.json').write_text(json.dumps(rec,indent=2),encoding='utf-8')
 return path,rec

def compare_quantitative(master_path,q_path):
 mh,mrows=read_csv(master_path); qh,qrows=read_csv(q_path)
 if not qrows or len(mrows)!=len(qrows): return {'status':'FAIL','reason':'row count mismatch'}
 mmap={r.get('mzmine_row_id'):r for r in mrows}; qmap={r.get('mzmine_row_id'):r for r in qrows}
 if set(mmap)!=set(qmap): return {'status':'FAIL','reason':'row identity mismatch'}
 changed=[]; max_abs=0.0
 for key in qmap:
  for col in qh:
   a=mmap[key].get(col,''); b=qmap[key].get(col,'')
   if a==b: continue
   na,nb=number(a),number(b)
   if na is not None and nb is not None and abs(na-nb)<=1e-12+1e-12*abs(nb):
    max_abs=max(max_abs,abs(na-nb)); continue
   if len(changed)<100: changed.append({'row':key,'column':col,'actual':a,'expected':b})
 return {'status':'PASS' if not changed else 'FAIL','fields_tested':len(qh),'changed_cells':changed,'maximum_absolute_difference':max_abs}

def main():
 p=argparse.ArgumentParser(); p.add_argument('request'); p.add_argument('--report',required=True); a=p.parse_args()
 req=json.loads(Path(a.request).read_text(encoding='utf-8')); rp=Path(a.report)
 report={'schema_version':'1.0.0','run_id':req.get('run_id'),'stage_id':req.get('stage_id'),
         'status':'preflight_failed','validation_records':[],'produced_outputs':[]}
 problems=validate_request(req); report['validation_records']=problems
 if problems:
  rp.parent.mkdir(parents=True,exist_ok=True); rp.write_text(json.dumps(report,indent=2),encoding='utf-8'); return 2
 workspace=Path(req['output_working_directory']).resolve()
 try:
  copy,transform=make_copy(req,workspace); out,err=workspace/'stdout.log',workspace/'stderr.log'
  report.update(status='running',execution_started=True,frozen_script_hash=SCRIPT_HASH,execution_copy_hash=sha(copy),
                configuration_transformations=transform,stdout_path=str(out),stderr_path=str(err))
  rp.parent.mkdir(parents=True,exist_ok=True); rp.write_text(json.dumps(report,indent=2),encoding='utf-8')
  rex=req['environment_requirements']['r_executable']
  with out.open('w',encoding='utf-8') as o,err.open('w',encoding='utf-8') as e:
   proc=run_external_process([rex,str(copy)],scientific_script=copy,
                             project_path=workspace,stdout=o,stderr=e)
  report['process_exit_status']=proc.returncode
  if proc.returncode: raise RuntimeError('Generic Script 04 execution exited unsuccessfully.')
  parent=workspace/'analysis'/'04_Annotation_Merge'/req['dataset_id']; dirs=[x for x in parent.glob('AnnotationMerge_*') if x.is_dir()]
  if len(dirs)!=1: raise RuntimeError(f'Expected one Script 04 output directory; found {len(dirs)}.')
  od=dirs[0]; master=od/f'{req["dataset_id"]}_Annotated_ThreeReadout_Master.csv'
  if not master.is_file(): raise RuntimeError('Generic annotated master CSV is missing.')
  preservation=compare_quantitative(master,Path(req['registered_inputs']['replicate_summary_artifact']['path']))
  if preservation['status']!='PASS': raise RuntimeError('Quantitative columns were not preserved value-for-value.')
  mh,mrows=read_csv(master); qh,qrows=read_csv(Path(req['registered_inputs']['replicate_summary_artifact']['path']))
  if len(mrows)!=len(qrows) or 'annotation_name' not in mh or 'annotation_score' not in mh:
   raise RuntimeError('Generic annotation output schema or row retention failed.')
  outputs=[]
  for path in sorted(od.iterdir()):
   if not path.is_file(): continue
   nr=nc=None
   if path.suffix.lower()=='.csv': h,r=read_csv(path); nr=len(r); nc=len(h)
   outputs.append({'artifact_role':path.name,'path':str(path),'sha256':sha(path),'bytes':path.stat().st_size,'rows':nr,'columns':nc})
  report.update(status='completed',produced_outputs=outputs,quantitative_preservation=preservation,
                annotation_equivalence={'status':'NOT_EVALUABLE','reason':'No dataset-specific historical annotation oracle.'},
                row_retention={'status':'PASS','quantitative_rows':len(qrows),'output_rows':len(mrows)},
                schema={'status':'PASS','rows':len(mrows),'columns':len(mh),'column_names':mh},
                validation_records=[{'check_id':'generic.script04','status':'PASS','category':'scientific','blocking':False,
                 'message':'Quantitative preservation, annotation mapping, row retention, and dynamic schema checks passed.'}])
  rp.write_text(json.dumps(report,indent=2),encoding='utf-8'); return 0
 except Exception as exc:
  report.update(status='failed',error=str(exc),recommended_correction='Inspect preserved generic Script 04 diagnostics; no output was activated.')
  rp.parent.mkdir(parents=True,exist_ok=True); rp.write_text(json.dumps(report,indent=2),encoding='utf-8'); return 1

if __name__=='__main__': raise SystemExit(main())
