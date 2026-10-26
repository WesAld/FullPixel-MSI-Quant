#!/usr/bin/env python3
"""Create a dynamic five-sheet Generic FullPixel MSI Quant workbook."""
from __future__ import annotations
import argparse, hashlib, json, math, re, sys
from pathlib import Path
import pandas as pd
from openpyxl import Workbook, load_workbook
from openpyxl.styles import Font, PatternFill, Alignment
from openpyxl.utils import get_column_letter
sys.path.insert(0, str(Path(__file__).resolve().parent))
from normalization_routing import provenance, resolve_method


def sha(path:Path)->str:
 h=hashlib.sha256()
 with path.open('rb') as f:
  for b in iter(lambda:f.read(1024*1024),b''): h.update(b)
 return h.hexdigest().upper()
def safe(value,fallback='Project'):
 x=re.sub(r'[^A-Za-z0-9]+','_',str(value or '')).strip('_')
 return (x or fallback)[:64]
def unique_safe_labels(labels,internal_ids):
 base=[safe(label,internal) for label,internal in zip(labels,internal_ids)]
 counts={x:base.count(x) for x in set(base)}
 return [x if counts[x]==1 else safe(f'{x}_{internal}',internal)
         for x,internal in zip(base,internal_ids)]
def finite_mean(frame,cols):
 values=frame[cols].apply(pd.to_numeric,errors='coerce')
 return values.mean(axis=1,skipna=False)
def log2_ratio(target,reference):
 out=pd.Series(float('nan'),index=target.index,dtype='float64')
 valid=target.notna() & reference.notna() & (target>0) & (reference>0)
 out.loc[valid]=(target.loc[valid]/reference.loc[valid]).map(math.log2)
 return out
def max_difference(a,b):
 aa=pd.to_numeric(a,errors='coerce'); bb=pd.to_numeric(b,errors='coerce')
 both=aa.notna() & bb.notna()
 missing=(aa.isna()!=bb.isna()).sum()
 diff=(aa[both]-bb[both]).abs()
 return (float(diff.max()) if len(diff) else 0.0),int(missing)
def style_sheet(ws):
 ws.freeze_panes='A2'; ws.auto_filter.ref=ws.dimensions
 for cell in ws[1]:
  cell.font=Font(bold=True); cell.fill=PatternFill('solid',fgColor='D9EAF7'); cell.alignment=Alignment(wrap_text=True)
 for idx,col in enumerate(ws.columns,1):
  width=min(max(len(str(c.value)) if c.value is not None else 0 for c in list(col)[:200])+2,45)
  ws.column_dimensions[get_column_letter(idx)].width=max(width,10)
def write_df(ws,df):
 ws.append(list(df.columns))
 for row in df.itertuples(index=False,name=None):
  ws.append([None if pd.isna(x) else x for x in row])
 style_sheet(ws)

def main():
 p=argparse.ArgumentParser(); p.add_argument('request'); p.add_argument('--report',required=True); a=p.parse_args()
 req=json.loads(Path(a.request).read_text(encoding='utf-8')); rp=Path(a.report)
 report={'schema_version':'1.0.0','workflow_id':req.get('workflow_id'),'status':'FAIL'}
 try:
  method=resolve_method(req)
  normalization=provenance(method,req.get('display_metadata',{}).get('normalization',{}).get('normalization_scale',1000000))
  masters=req.get('masters',[])
  if not masters: raise RuntimeError('At least one validated annotated master is required.')
  frames=[]; source_rows=0
  for item in masters:
   path=Path(item['path'])
   if not path.is_file() or sha(path)!=item['sha256'].upper(): raise RuntimeError(f'Master identity failed: {path}')
   df=pd.read_csv(path,low_memory=False); source_rows+=len(df)
   if 'ion_mode' not in df.columns: df['ion_mode']=item['ion_mode']
   frames.append(df)
  master=pd.concat(frames,ignore_index=True,sort=False)
  if len(master)!=source_rows: raise RuntimeError('Combined row retention failed.')
  mappings=req['display_metadata']['sample_mappings']
  ctrl=[x for x in mappings if x.get('internal_value','').startswith('CTRL_')]
  trt=[x for x in mappings if x.get('internal_value','').startswith('TRT_')]
  # Deduplicate mode-repeated mappings while preserving order.
  def unique_map(rows):
   seen=set(); out=[]
   for x in rows:
    key=x['internal_value']
    if key not in seen: seen.add(key); out.append(x)
   return out
  ctrl=unique_map(ctrl); trt=unique_map(trt)
  if len(ctrl)<2 or len(ctrl)!=len(trt): raise RuntimeError('Display mapping does not contain equal two-group replicate sets.')
  ctrl_ids=[x['internal_value'] for x in ctrl]; trt_ids=[x['internal_value'] for x in trt]
  ctrl_display=[x.get('user_facing_label') or x.get('user_facing_value') or x['internal_value'] for x in ctrl]
  trt_display=[x.get('user_facing_label') or x.get('user_facing_value') or x['internal_value'] for x in trt]
  display_tokens=unique_safe_labels(ctrl_display+trt_display,ctrl_ids+trt_ids)
  ctrl_tokens=display_tokens[:len(ctrl_ids)]; trt_tokens=display_tokens[len(ctrl_ids):]
  ref_label=req['display_metadata'].get('reference_condition','Reference')
  cmp_label=req['display_metadata'].get('comparison_condition','Comparison')
  meta_candidates=['dataset_id','ion_mode','mzmine_row_id','target_mz','annotation_name','annotation_formula','annotation_adduct','annotation_score','annotation_database']
  meta=[x for x in meta_candidates if x in master.columns]
  if 'annotation_name' not in master.columns: master['annotation_name']=''
  if 'annotation_formula' not in master.columns: master['annotation_formula']=''
  master['GraphPad_label']=master.apply(lambda r: f"{r.get('annotation_name') or 'Unannotated'} | m/z {float(r.get('target_mz')):.4f}",axis=1)
  meta=[x for x in meta if x not in ('GraphPad_label',)]+['GraphPad_label']
  overall_ctrl=[x+'_overall_normalized_signal' for x in ctrl_ids]; overall_trt=[x+'_overall_normalized_signal' for x in trt_ids]
  local_ctrl=[x+'_local_strong_pixel_intensity' for x in ctrl_ids]; local_trt=[x+'_local_strong_pixel_intensity' for x in trt_ids]
  frac_ctrl=[x+'_conservative_detection_fraction' for x in ctrl_ids]; frac_trt=[x+'_conservative_detection_fraction' for x in trt_ids]
  count_cols=[x+'_conservative_detected_pixel_count' for x in ctrl_ids+trt_ids]
  required=(overall_ctrl+overall_trt+local_ctrl+local_trt+frac_ctrl+frac_trt+count_cols+
   ['log2FC_overall_TRT_vs_CTRL','local_intensity_log2FC_TRT_vs_CTRL','detection_fraction_difference_TRT_minus_CTRL'])
  missing=[x for x in required if x not in master.columns]
  if missing: raise RuntimeError('Required master columns are missing: '+', '.join(missing[:20]))
  # Independent numerical audit.
  audit_overall=log2_ratio(finite_mean(master,overall_trt),finite_mean(master,overall_ctrl))
  audit_local=log2_ratio(finite_mean(master,local_trt),finite_mean(master,local_ctrl))
  audit_detection=finite_mean(master,frac_trt)-finite_mean(master,frac_ctrl)
  d1,m1=max_difference(audit_overall,master['log2FC_overall_TRT_vs_CTRL'])
  d2,m2=max_difference(audit_local,master['local_intensity_log2FC_TRT_vs_CTRL'])
  d3,m3=max_difference(audit_detection,master['detection_fraction_difference_TRT_minus_CTRL'])
  if max(d1,d2,d3)>1e-10 or max(m1,m2,m3)>0: raise RuntimeError('Independent numerical audit failed.')
  rename={}
  for internal,display in zip(ctrl_ids+trt_ids,ctrl_tokens+trt_tokens):
   for suffix in ['overall_normalized_signal','local_strong_pixel_intensity','conservative_detection_fraction','conservative_detected_pixel_count']:
    rename[internal+'_'+suffix]=display+'_'+suffix
  overall=master[meta+overall_ctrl+overall_trt+['log2FC_overall_TRT_vs_CTRL','overall_direction','preliminary_quantitative_candidate']].copy()
  overall.insert(len(meta),'primary_normalization_method',method)
  overall.rename(columns=rename,inplace=True)
  overall.rename(columns={'log2FC_overall_TRT_vs_CTRL':f'log2FC_{safe(cmp_label)}_vs_{safe(ref_label)}'},inplace=True)
  all_pixels=master[meta+frac_ctrl+frac_trt+count_cols+['detection_fraction_difference_TRT_minus_CTRL']].copy()
  for c in frac_ctrl+frac_trt: all_pixels[c]=pd.to_numeric(all_pixels[c],errors='coerce')*100
  all_pixels['detection_fraction_difference_TRT_minus_CTRL']=pd.to_numeric(all_pixels['detection_fraction_difference_TRT_minus_CTRL'],errors='coerce')*100
  all_pixels.rename(columns=rename,inplace=True)
  all_pixels.rename(columns={'detection_fraction_difference_TRT_minus_CTRL':f'ion_positive_pixel_change_{safe(cmp_label)}_minus_{safe(ref_label)}_pp'},inplace=True)
  local=master[meta+local_ctrl+local_trt+['local_intensity_log2FC_TRT_vs_CTRL']].copy(); local.rename(columns=rename,inplace=True)
  local.insert(len(meta),'primary_normalization_method',method)
  local.rename(columns={'local_intensity_log2FC_TRT_vs_CTRL':f'local_intensity_log2FC_{safe(cmp_label)}_vs_{safe(ref_label)}'},inplace=True)
  qc=pd.DataFrame([
   ['source_rows_retained','PASS',len(master),source_rows],
   ['overall_numerical_audit','PASS',d1,'<=1e-10'],
   ['local_numerical_audit','PASS',d2,'<=1e-10'],
   ['detection_numerical_audit','PASS',d3,'<=1e-10'],
   ['two_group_replicate_mapping','PASS',f'{len(ctrl)} + {len(trt)}','equal and >=2'],
   ['technical_validation_classification','WARNING','Technical Validation Candidate','dataset-specific oracle not supplied']
  ],columns=['check','status','observed','expected'])
  readme=pd.DataFrame({'Field':['Application version','Project','Exposure','Reference condition','Comparison condition','Ion modes','Replicates per condition','Primary normalization method','Normalization definition version','Normalization','Detection threshold','Statistical interpretation','Validation status'],
   'Value':['2.0.0-dev',req['display_metadata'].get('project_name'),req.get('exposure_label'),ref_label,cmp_label,', '.join(x['ion_mode'] for x in masters),len(ctrl),method,normalization['normalization_definition_version'],
   normalization['normalization_definition'],'Conservative raw intensity >= 1000; minimum 10 pixels','Descriptive two-group readouts; no p-values, pairing, pseudocount, or imputation','Technical Validation Candidate']})
  outdir=Path(req['output_working_directory']); outdir.mkdir(parents=True,exist_ok=False)
  filename=f"{safe(req['display_metadata'].get('project_name'))}_{safe(req.get('exposure_label'))}_MALDI_MSI_Final_Results_TECHNICAL_VALIDATION_CANDIDATE.xlsx"
  path=outdir/filename
  wb=Workbook(); wb.remove(wb.active)
  intensity_sheet='TIC_normalized_intensity' if method=='spectrum_sum' else f'Primary_{method.upper()}_Intensity'
  for name,df in [('Mean_Log2FC',overall),('All_Pixels',all_pixels),(intensity_sheet,local),('QC',qc),('README',readme)]:
   ws=wb.create_sheet(name); write_df(ws,df)
  wb.calculation.fullCalcOnLoad=True; wb.calculation.forceFullCalc=True; wb.calculation.calcMode='auto'
  wb.save(path)
  # Re-open validation.
  checked=load_workbook(path,read_only=True,data_only=False)
  sheet_rows={ws.title:ws.max_row-1 for ws in checked.worksheets}; checked.close()
  if any(sheet_rows.get(x,0)<1 for x in ['Mean_Log2FC','All_Pixels',intensity_sheet,'QC','README']):
   raise RuntimeError('Final workbook sheet validation failed.')
  report.update(status='PASS',final_workbook={'path':str(path),'sha256':sha(path),'bytes':path.stat().st_size},
                source_rows=source_rows,sheet_rows=sheet_rows,normalization=normalization,numerical_audit={'overall_max_abs':d1,'local_max_abs':d2,'detection_max_abs':d3})
  rp.parent.mkdir(parents=True,exist_ok=True); rp.write_text(json.dumps(report,indent=2),encoding='utf-8'); return 0
 except Exception as exc:
  report.update(status='FAIL',error=str(exc),recommended_correction='Inspect the preserved generic export request and source masters.')
  rp.parent.mkdir(parents=True,exist_ok=True); rp.write_text(json.dumps(report,indent=2),encoding='utf-8'); return 1
if __name__=='__main__': raise SystemExit(main())
