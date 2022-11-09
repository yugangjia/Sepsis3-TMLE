--                                      confirm the maximum possible range of sepsis groups                              --

-- --------------------------------------------------------------------------------------------------------------------- --
-- 1. diagnosis infection, sepsis, severe sepsis, septic shock
-- 2. use antibiotics
-- 3. existing fluid culture
-- adults

drop table if exists `db_name.sepsis_basic_eicu`;
create table `db_name.sepsis_basic_eicu` as

with cohort_eicu_adults as (
  SELECT * 
  FROM `physionet-data.eicu_crd_derived.icustay_detail`
  where cast(case when age = '> 89' then '91.4' when age = '' then '0' else age end as numeric) >= 16
)


, diagnosis_info_basic as (
  SELECT distinct patientunitstayid
  FROM `physionet-data.eicu_crd.diagnosis`
  where lower(diagnosisstring) like '%severe sepsis%'
  or lower(diagnosisstring) like '%septic shock%'
  or lower(diagnosisstring) like '%infectious%'
  or lower(diagnosisstring) like '%infection%'  
  or lower(diagnosisstring) like '%sepsis%'  
) 

, antibiotic_info_basic as (
  select distinct patientunitstayid
  from `db_name.antibiotic_eicu`
)

, treatment_info_basic as (
  select distinct patientunitstayid
  FROM `physionet-data.eicu_crd.treatment`
  where lower(treatmentstring) like '%infectious%'
  or lower(treatmentstring) like '%infection%'
  or lower(treatmentstring) like '%culture%'
  -- reference: Matthieu
  or lower(treatmentstring) like '%antibiotic%'
  or lower(treatmentstring) like '%antibact%'
  or lower(treatmentstring) like '%antibio%'
  or lower(treatmentstring) like '%amika%'
  or lower(treatmentstring) like '%genta%'
  or lower(treatmentstring) like '%tobra%'
  or lower(treatmentstring) like '%cillin%'
  or lower(treatmentstring) like '%vanco%'
  or lower(treatmentstring) like '%cyclin%'
  or lower(treatmentstring) like '%xacin%'
  or lower(treatmentstring) like '%mycin%'
  or lower(treatmentstring) like '%metrondaz%'
  or lower(treatmentstring) like '%clavula%'
  or lower(treatmentstring) like '%tazob%'
  or lower(treatmentstring) like '%vancin%'
  or lower(treatmentstring) like '%penem%'
  or lower(treatmentstring) like '%micin%'
  or lower(treatmentstring) like '%cefur%'
  or lower(treatmentstring) like '%ceftri%'
  or lower(treatmentstring) like '%cephal%'

  or lower(treatmentstring) like '%sepsis%'
  or lower(treatmentstring) like '%severe sepsis%'
  or lower(treatmentstring) like '%sepsis%severe%'
  or lower(treatmentstring) like '%septic shock%'
  or lower(treatmentstring) like '%infectious diseases|procedures|vascular catheter - removal|tip cultured%'
  or lower(treatmentstring) like '%infectious diseases|procedures|vascular catheter - change|tip cultured%'
)

, apache_diagnosis_basic as (
  select distinct patientunitstayid
  from `physionet-data.eicu_crd.patient`
  where lower(apacheadmissiondx) like '%severe sepsis%'
  or lower(apacheadmissiondx) like '%septic shock%'
  or lower(apacheadmissiondx) like '%infectious%'
  or lower(apacheadmissiondx) like '%infection%'
  or lower(apacheadmissiondx) like '%sepsis%'
)

, sepsis_basic_eicu as (
  select distinct patientunitstayid
  from (
    select *
    from diagnosis_info_basic
    union all
    select *
    from antibiotic_info_basic
    union all
    select *
    from treatment_info_basic
    union all
    select *
    from apache_diagnosis_basic
  )
  where patientunitstayid in (select patientunitstayid from cohort_eicu_adults)
)

select *
from sepsis_basic_eicu
order by patientunitstayid;



--                                           1. antibiotic + culture + sofa/organ failure score                                         --
--
----------------------------------------------------------------------------------------------------------------------------
drop table if exists `db_name.sepsis_part1_eicu`;
create table `db_name.sepsis_part1_eicu` as
-- inspected infection + sofa >= 2 [T_insected infection - 2days, 1 day + T_insected infection]

with antibiotic_info_0 as (
  select patientunitstayid, drugstartoffset as antibiotic_offset
  from `db_name.antibiotic_eicu`
  union all
  select patientunitstayid, treatmentoffset as antibiotic_offset
  from `physionet-data.eicu_crd.treatment`
  where (lower(treatmentstring) like '%antibiotic%'
  or lower(treatmentstring) like '%antibact%'
  or lower(treatmentstring) like '%antibio%'
  or lower(treatmentstring) like '%amika%'
  or lower(treatmentstring) like '%genta%'
  or lower(treatmentstring) like '%tobra%'
  or lower(treatmentstring) like '%cillin%'
  or lower(treatmentstring) like '%vanco%'
  or lower(treatmentstring) like '%cyclin%'
  or lower(treatmentstring) like '%xacin%'
  or lower(treatmentstring) like '%mycin%'
  or lower(treatmentstring) like '%metrondaz%'
  or lower(treatmentstring) like '%clavula%'
  or lower(treatmentstring) like '%tazob%'
  or lower(treatmentstring) like '%vancin%'
  or lower(treatmentstring) like '%penem%'
  or lower(treatmentstring) like '%micin%'
  or lower(treatmentstring) like '%cefur%'
  or lower(treatmentstring) like '%ceftri%'
  or lower(treatmentstring) like '%cephal%'
  )
)

, antibiotic_info as (
  select distinct *
  from antibiotic_info_0
  where patientunitstayid in (select patientunitstayid from `db_name.sepsis_basic_eicu`)
)

, culture_info as (
  select distinct patientunitstayid, treatmentoffset as culture_offset
  from `physionet-data.eicu_crd.treatment`
  where patientunitstayid in (select patientunitstayid from `db_name.sepsis_basic_eicu`)
  and treatmentstring in (
    'infectious diseases|cultures / immuno-assays|cultures|BAL/PBS|bacterial'
    , 'infectious diseases|cultures / immuno-assays|cultures|catheter tip'
    , 'surgery|infection|cultures|sputum|fungal'
    , 'surgery|infection|cultures|sputum'
    , 'infectious diseases|cultures / immuno-assays|cultures|blood|drawn from central line'
    , 'surgery|infection|cultures|biopsy material'
    , 'surgery|infection|cultures|urine'
    , 'infectious diseases|cultures / immuno-assays|cultures|blood|peripheral'
    , 'infectious diseases|procedures|vascular catheter - removal|tip cultured'
    , 'infectious diseases|cultures / immuno-assays|cultures|BAL/PBS'
    , 'infectious diseases|cultures / immuno-assays|cultures|urine|suprapubic aspiration'
    , 'infectious diseases|cultures / immuno-assays|cultures|sputum|fungal'
    , 'infectious diseases|cultures / immuno-assays|cultures|pericardial fluid'
    , 'infectious diseases|cultures / immuno-assays|cultures|blood'
    , 'infectious diseases|cultures / immuno-assays|cultures|BAL/PBS|comprehensive (bacterial, viral, fungal, afb, etc.'
    , 'infectious diseases|cultures / immuno-assays|cultures|catheter tip|quantitative'
    , 'surgery|infection|cultures|blood|peripheral'
    , 'surgery|infection|cultures|urine|voided'
    , 'infectious diseases|cultures / immuno-assays|cultures|biopsy material'
    , 'surgery|infection|cultures|blood'
    , 'infectious diseases|procedures|vascular catheter - change|tip cultured'
    , 'infectious diseases|cultures / immuno-assays|cultures|CSF'
    , 'infectious diseases|cultures / immuno-assays|cultures|peritoneal fluid'
    , 'infectious diseases|cultures / immuno-assays|cultures|pleural fluid'
    , 'infectious diseases|cultures / immuno-assays|cultures|BAL/PBS|AFB'
    , 'infectious diseases|cultures / immuno-assays|cultures|sputum'
    , 'infectious diseases|cultures / immuno-assays|cultures|urine'
    , 'surgery|infection|cultures|surgical specimen'
    , 'infectious diseases|cultures / immuno-assays|cultures|urine|catheterized'
    , 'surgery|infection|cultures'
    , 'infectious diseases|cultures / immuno-assays|cultures|sputum|AFB'
    , 'infectious diseases|cultures / immuno-assays|cultures'
    , 'infectious diseases|cultures / immuno-assays|cultures|surgical specimen'
    , 'infectious diseases|cultures / immuno-assays|cultures|sputum|bacterial'
    , 'surgery|infection|cultures|sputum|AFB'
    , 'infectious diseases|cultures / immuno-assays|cultures|stool'
    , 'infectious diseases|cultures / immuno-assays|cultures|urine|voided'
  )  
)

, infection_info_0 as (
  select patientunitstayid, antibiotic_offset as infection_offset
  from (
    select ai.patientunitstayid, ai.antibiotic_offset, ci.culture_offset
    from antibiotic_info ai
    inner join culture_info ci
    on ai.patientunitstayid = ci.patientunitstayid
    and ai.antibiotic_offset <= ci.culture_offset
    and ai.antibiotic_offset + 24*60 >= ci.culture_offset
  )
  group by patientunitstayid, antibiotic_offset
)

, infection_info_1 as (
  select patientunitstayid, culture_offset as infection_offset
  from (
    select ci.patientunitstayid, ci.culture_offset, ai.antibiotic_offset
    from culture_info ci
    inner join antibiotic_info ai
    on ai.patientunitstayid = ci.patientunitstayid
    and ci.culture_offset <= ai.antibiotic_offset
    and ci.culture_offset + 3*24*60 >= ai.antibiotic_offset
  )
  group by patientunitstayid, culture_offset
)

, infection_info_2 as (
  select distinct patientunitstayid, infection_offset
  from (
    select *
    from infection_info_0
    union all
    select *
    from infection_info_1    
  )
)

, infection_info as (
  select ii.patientunitstayid, ii.infection_offset
  , (ii.infection_offset - 48*60) as startoffsetwd
  , (ii.infection_offset + 24*60) as endoffsetwd
  , icud.unitdischargeoffset  
  from infection_info_2 ii 
  inner join `physionet-data.eicu_crd_derived.icustay_detail` icud
  on ii.patientunitstayid = icud.patientunitstayid
)

, sepsis_onset_part10 as (
  select di.*, sf.hr
  , ROW_NUMBER() OVER (PARTITION BY di.patientunitstayid ORDER BY sf.hr) as rn
  from infection_info di
  inner join `db_name.pivoted_sofa_eicu` sf
  on di.patientunitstayid = sf.patientunitstayid
  and sf.hr <= di.endoffsetwd/60
  and sf.hr >= di.startoffsetwd/60
  and sf.SOFA_24hours >= 2
)

, sepsis_onset_part11 as (
  select ii.*, di.diagnosisoffset
  , ROW_NUMBER() OVER (PARTITION BY ii.patientunitstayid ORDER BY di.diagnosisoffset) as rn
  from infection_info ii 
  inner join `physionet-data.eicu_crd.diagnosis` di
  on ii.patientunitstayid = di.patientunitstayid
  and (lower(diagnosisstring) like '%organ failure%'
  or lower(diagnosisstring) like '%organ dysfunction%'
  )
  and di.diagnosisoffset >= ii.infection_offset - 48*60
  and di.diagnosisoffset <= ii.infection_offset + 24*60
)

, sepsis_onset_part1 as (
  select sop.patientunitstayid, antibiotic_culture_sofa_hr
  , ROW_NUMBER() OVER (PARTITION BY sop.patientunitstayid ORDER BY antibiotic_culture_sofa_hr) as rn
  from (
    select patientunitstayid
    , hr as antibiotic_culture_sofa_hr
    from sepsis_onset_part10
    where rn = 1
    union all
    select patientunitstayid
    , round(diagnosisoffset/60, 1) as antibiotic_culture_sofa_hr
    from sepsis_onset_part11
    where rn = 1
  ) sop
  inner join `physionet-data.eicu_crd_derived.icustay_detail` icud
  on sop.patientunitstayid = icud.patientunitstayid
  and sop.antibiotic_culture_sofa_hr < icud.unitdischargeoffset/60
)

select sop.patientunitstayid
, antibiotic_culture_sofa_hr
from sepsis_onset_part1 sop
where rn = 1
order by sop.patientunitstayid;



--                                            2. infection/sepsis + sofa/organ failure score                                        --
--
----------------------------------------------------------------------------------------------------------------------------
drop table if exists `db_name.sepsis_part2_eicu`;
create table `db_name.sepsis_part2_eicu` as

with sepsis_diagnosis_info_00 as (
    SELECT patientunitstayid, diagnosisoffset
    FROM `physionet-data.eicu_crd.diagnosis`
    where (
      lower(diagnosisstring) like '%sepsis%'
      or lower(diagnosisstring) like '%infectious%'
      or lower(diagnosisstring) like '%infection%'
    )
    and patientunitstayid in (select patientunitstayid from `db_name.sepsis_basic_eicu`)
)

, sepsis_diagnosis_info_01 as (
    SELECT distinct patientunitstayid
    FROM `physionet-data.eicu_crd.patient`
    where (
      lower(apacheadmissiondx) like '%sepsis%'
      or lower(apacheadmissiondx) like '%infectious%'
      or lower(apacheadmissiondx) like '%infection%'
    )
    and patientunitstayid in (select patientunitstayid from `db_name.sepsis_basic_eicu`)
)

, sepsis_diagnosis_info_02 as (
  select patientunitstayid, diagnosisoffset
  , ROW_NUMBER() OVER (PARTITION BY patientunitstayid ORDER BY diagnosisoffset) as rn
  from (
    select patientunitstayid, diagnosisoffset
    from sepsis_diagnosis_info_00
    union all
    select patientunitstayid, 0 as diagnosisoffset
    from sepsis_diagnosis_info_01
  )
)

, sepsis_diagnosis_info as (
  select di.patientunitstayid, di.diagnosisoffset as infection_offset
  , (di.diagnosisoffset - 48*60) as startoffsetwd
  , (di.diagnosisoffset + 24*60) as endoffsetwd
  , icud.unitdischargeoffset
  from sepsis_diagnosis_info_02 di
  inner join `physionet-data.eicu_crd_derived.icustay_detail` icud
  on di.patientunitstayid = icud.patientunitstayid
  where rn = 1  
)

, sepsis_onset_part20 as (
  select di.*, sf.hr
  , ROW_NUMBER() OVER (PARTITION BY di.patientunitstayid ORDER BY sf.hr) as rn
  from sepsis_diagnosis_info di
  inner join `db_name.pivoted_sofa_eicu` sf
  on di.patientunitstayid = sf.patientunitstayid
  and sf.hr <= di.endoffsetwd/60
  and sf.hr >= di.startoffsetwd/60
  and sf.SOFA_24hours >= 2
)

, sepsis_onset_part21 as (
  select ii.*, di.diagnosisoffset
  , ROW_NUMBER() OVER (PARTITION BY ii.patientunitstayid ORDER BY di.diagnosisoffset) as rn
  from sepsis_diagnosis_info ii 
  inner join `physionet-data.eicu_crd.diagnosis` di
  on ii.patientunitstayid = di.patientunitstayid
  and (lower(diagnosisstring) like '%organ failure%'
  or lower(diagnosisstring) like '%organ dysfunction%'
  )
  and di.diagnosisoffset >= ii.infection_offset - 48*60
  and di.diagnosisoffset <= ii.infection_offset + 24*60
)

, sepsis_onset_part2 as (
  select sop.patientunitstayid, infectiondiagnosis_sofa_hr
  , ROW_NUMBER() OVER (PARTITION BY sop.patientunitstayid ORDER BY infectiondiagnosis_sofa_hr) as rn
  from (
    select patientunitstayid
    , hr as infectiondiagnosis_sofa_hr
    from sepsis_onset_part20
    where rn = 1
    union all
    select patientunitstayid
    , round(diagnosisoffset/60, 1) as infectiondiagnosis_sofa_hr
    from sepsis_onset_part21
    where rn = 1
  ) sop
  inner join `physionet-data.eicu_crd_derived.icustay_detail` icud
  on sop.patientunitstayid = icud.patientunitstayid
  and sop.infectiondiagnosis_sofa_hr < icud.unitdischargeoffset/60
)

select sop.patientunitstayid
, infectiondiagnosis_sofa_hr
from sepsis_onset_part2 sop
where rn = 1
order by sop.patientunitstayid;


--                             3. diagnosis sepsis/infection + organ failure                                        --
--
---------------------------------------------------------------------------------------------------------------------
drop table if exists `db_name.sepsis_part3_eicu`;
create table `db_name.sepsis_part3_eicu` as

with diagnosis_sepsis_organ as (
  select patientunitstayid, diagnosisoffset
  from `physionet-data.eicu_crd.diagnosis`
  where (
    lower(diagnosisstring) like '%sepsis%organ failure%'
    or lower(diagnosisstring) like '%sepsis%organ dysfunction%'
  )
  and patientunitstayid in (select patientunitstayid from `db_name.sepsis_basic_eicu`)
)

, apache_diagnosis_sepsis_organ as (
  select patientunitstayid, 0 as diagnosisoffset
  from `physionet-data.eicu_crd.patient`
  where 
  (lower(apacheadmissiondx) like '%infectious%organ failure%'
  or lower(apacheadmissiondx) like '%infectious%organ dysfunction%'
  or lower(apacheadmissiondx) like '%organ failure%infectious%'
  or lower(apacheadmissiondx) like '%organ dysfunction%infectious%'
  or lower(apacheadmissiondx) like '%infection%organ failure%'
  or lower(apacheadmissiondx) like '%infection%organ dysfunction%'
  or lower(apacheadmissiondx) like '%organ failure%infection%'
  or lower(apacheadmissiondx) like '%organ dysfunction%infection%'
  or lower(apacheadmissiondx) like '%organ dysfunction%sepsis%'
  or lower(apacheadmissiondx) like '%sepsis%organ dysfunction%'
  )
  and patientunitstayid in (select patientunitstayid from `db_name.sepsis_basic_eicu`)
)

, sepsis_onset_part3 as (
  select patientunitstayid
  , min(round(diagnosisoffset/60,1)) as sepsisdiagnosis_offset_hr 
  from (
    select *
    from diagnosis_sepsis_organ
    union all
    select *
    from apache_diagnosis_sepsis_organ
  )
  group by patientunitstayid
)

select *
from sepsis_onset_part3
order by patientunitstayid;


--                                            4. severe sepsis/septic shock                                        --
--
---------------------------------------------------------------------------------------------------------------------
drop table if exists `db_name.sepsis_part4_eicu`;
create table `db_name.sepsis_part4_eicu` as

with diagnosis_info_part1 as (
  SELECT patientunitstayid, min(diagnosisoffset) as severe_sepsis_onset
  FROM `physionet-data.eicu_crd.diagnosis`
  where lower(diagnosisstring) like '%severe sepsis%'
  or lower(diagnosisstring) like '%septic shock%'
  group by patientunitstayid
)

, treatment_info_part1 as (
  select patientunitstayid, min(treatmentoffset) as severe_sepsis_onset
  FROM `physionet-data.eicu_crd.treatment`
  where lower(treatmentstring) like '%severe sepsis%'
  or lower(treatmentstring) like '%sepsis%severe%'
  or lower(treatmentstring) like '%septic shock%'
  group by patientunitstayid
)

, apache_diagnosis_part1 as (
  select patientunitstayid, 0 as severe_sepsis_onset
  from `physionet-data.eicu_crd.patient`
  where lower(apacheadmissiondx) like '%severe sepsis%'
  or lower(apacheadmissiondx) like '%septic shock%'
)

, severe_sepsis as (
  select patientunitstayid, min(severe_sepsis_onset) as severe_sepsis_onset
  from (
    select *
    from diagnosis_info_part1
    union all
    select *
    from treatment_info_part1
    union all
    select *
    from apache_diagnosis_part1
  )
  where patientunitstayid in (select patientunitstayid from `db_name.sepsis_basic_eicu`)
  group by patientunitstayid
)

select patientunitstayid, round(severe_sepsis_onset/60, 1) as severe_sepsis_onset_hr
from severe_sepsis
order by patientunitstayid;



drop table if exists `db_name.sepsis_adult_eicu`;
create table `db_name.sepsis_adult_eicu` as

with sepsis_info_eicu as (
  select patientunitstayid
  from `db_name.sepsis_part1_eicu`
  union all
  select patientunitstayid
  from `db_name.sepsis_part2_eicu`
  union all
  select patientunitstayid
  from `db_name.sepsis_part3_eicu`
  union all
  select patientunitstayid
  from `db_name.sepsis_part4_eicu`
)

select sie.patientunitstayid
, s1.antibiotic_culture_sofa_hr
, s2.infectiondiagnosis_sofa_hr
, s3.sepsisdiagnosis_offset_hr
, s4.severe_sepsis_onset_hr
from (
  select distinct patientunitstayid
  from sepsis_info_eicu
) sie
left join `db_name.sepsis_part1_eicu` s1
on sie.patientunitstayid = s1.patientunitstayid
left join `db_name.sepsis_part2_eicu` s2
on sie.patientunitstayid = s2.patientunitstayid
left join `db_name.sepsis_part3_eicu` s3
on sie.patientunitstayid = s3.patientunitstayid
left join `db_name.sepsis_part4_eicu` s4
on sie.patientunitstayid = s4.patientunitstayid
order by sie.patientunitstayid;




drop table if exists `db_name.sepsis_basic_eicu`;
drop table if exists `db_name.sepsis_part1_eicu`;
drop table if exists `db_name.sepsis_part2_eicu`;
drop table if exists `db_name.sepsis_part3_eicu`;
drop table if exists `db_name.sepsis_part4_eicu`;