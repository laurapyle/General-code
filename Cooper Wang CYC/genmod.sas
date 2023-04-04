

/**********************************************************************
 *   PRODUCT:   SAS
 *   VERSION:   9.4
 *   CREATOR:   External File Interface
 *   DATE:      29MAR23
 *   DESC:      Generated SAS Datastep Code
 *   TEMPLATE SOURCE:  (None Specified.)
 ***********************************************************************/
    data WORK.ALLDATA    ;
    %let _EFIERR_ = 0; /* set the ERROR detection macro variable */
    infile 'W:\Shared Projects\Claire\Active\Cooper, Jennifer (1st project)\Resubmission\longdata_03072023 SAS.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
       informat record_id best32. ;
       informat time best32. ;
       informat CRR best32. ;
       informat CRRPRR best32. ;
       informat Prednisone best32. ;
       informat IVsteroids best32. ;
       informat ACEARB best32. ;
       informat Ritux best32. ;
       informat dose $16. ;
       informat age_cyc1_yrs best32. ;
       informat gender $6. ;
       informat race5 $33. ;
       informat diagnosis_GFR10 best32. ;
       informat upc_ratio best32. ;
       informat cyclophosphamide_use_2 $46. ;
       informat diagnosis_hypertension $3. ;
       informat duration_ln_to_cyc1 best32. ;
       informat duration_sle_to_cyc1 best32. ;
       informat lupus_nephritis_class___5 $9. ;
       informat lupus_nephritis_IIIvsIV $2. ;
       informat percent_expected_dose best32. ;
       informat diagnosis_ACEARB best32. ;
       informat diagnosis_poprednisone best32. ;
       format record_id best12. ;
       format time best12. ;
       format CRR best12. ;
       format CRRPRR best12. ;
       format Prednisone best12. ;
       format IVsteroids best12. ;
       format ACEARB best12. ;
       format Ritux best12. ;
       format dose $16. ;
       format age_cyc1_yrs best12. ;
       format gender $6. ;
       format race5 $33. ;
       format diagnosis_GFR10 best12. ;
       format upc_ratio best12. ;
       format cyclophosphamide_use_2 $46. ;
       format diagnosis_hypertension $3. ;
       format duration_ln_to_cyc1 best12. ;
       format duration_sle_to_cyc1 best12. ;
       format lupus_nephritis_class___5 $9. ;
       format lupus_nephritis_IIIvsIV $2. ;
       format percent_expected_dose best12. ;
       format diagnosis_ACEARB best12. ;
       format diagnosis_poprednisone best12. ;
    input
                record_id
                time
                CRR
                CRRPRR
                Prednisone
                IVsteroids
                ACEARB
                Ritux
                dose  $
                age_cyc1_yrs
                gender  $
                race5  $
                diagnosis_GFR10
                upc_ratio
                cyclophosphamide_use_2  $
                diagnosis_hypertension  $
                duration_ln_to_cyc1
                duration_sle_to_cyc1
                lupus_nephritis_class___5  $
                lupus_nephritis_IIIvsIV  $
                percent_expected_dose
                diagnosis_ACEARB
                diagnosis_poprednisone
    ;
    if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
    run;

/*
mod <-  geeglm(CRR ~ dose*time + percent_expected_dose + age_cyc1_yrs + diagnosis_GFR10 + upc_ratio + 
                 cyclophosphamide_use_2 + duration_sle_to_cyc1 + gender + race5 + 
                 Prednisone + IVsteroids_per_SD + ACEARB + Ritux, 
               id=record_id, data=data, family=binomial, scale.fix=TRUE, corstr="ar1")
*/

data alldata;
set alldata;
IVsteroids_per_SD = IVsteroids/3014.263;
run;

ods rtf file="W:\Shared Projects\Claire\Active\Cooper, Jennifer (1st project)\Resubmission\Results\PROC GENMOD results.rtf";
proc genmod data=alldata descending;
class dose time cyclophosphamide_use_2 gender race5  ACEARB Ritux record_id CRR;
model CRR =  percent_expected_dose age_cyc1_yrs diagnosis_GFR10 upc_ratio cyclophosphamide_use_2 duration_sle_to_cyc1 gender race5 
                 Prednisone IVsteroids_per_SD ACEARB Ritux dose*time / dist=bin type3;
repeated subject=record_id / type=CS;
run;

proc genmod data=alldata descending;
class dose time cyclophosphamide_use_2 gender race5  ACEARB Ritux record_id CRRPRR;
model CRRPRR =  percent_expected_dose age_cyc1_yrs diagnosis_GFR10 upc_ratio cyclophosphamide_use_2 duration_sle_to_cyc1 gender race5 
                 Prednisone IVsteroids_per_SD ACEARB Ritux dose*time / dist=bin type3;
repeated subject=record_id / type=CS;
run;
ods rtf close;
