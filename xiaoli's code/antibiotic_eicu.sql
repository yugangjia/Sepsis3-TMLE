-- antibiotic information
drop table if exists `db_name.antibiotic_eicu`;
create table `db_name.antibiotic_eicu` as

with meds_part10 as
(
  select
    patientunitstayid
    , drugorderoffset
    , drugstartoffset, drugstopoffset

    -- assign our own identifier based off HICL codes
    , case
        when drughiclseqno in (3941, 3938, 3937, 3935, 6077, 3940, 3936, 3942, 3951, 3953, 3952, 3963) then 'penicilin'
        when drughiclseqno in (3956, 39531, 3957, 5886, 35575, 3946, 3960) then 'penicilin_anti_staph'
        when drughiclseqno in (32900, 3943, 8738, 33427, 3965) then 'penicilin_anti_pseudo'
        when drughiclseqno in (3962, 3948) then 'augmentin_unasyn'
        when drughiclseqno in (3796, 25040, 3795, 26808, 26809, 25040, 35282, 13908, 3972, 22796  ) then 'cephalosporin_1st_gen'
        when drughiclseqno in (3998, 35169, 8949, 3979, 33772, 3978, 3992, 3990, 25953, 6009, 6012, 3991, 3983, 6313) then 'cephalosporin_2nd_gen'
        when drughiclseqno in (33094, 3996, 3995, 8630, 15717, 13429, 37769, 26487, 26488, 13417, 13426, 3984, 3985, 18548, 29980, 6945, 10246) then 'cephalosporin_3rd_gen'
        when drughiclseqno in (37243, 35848, 10132, 37021) then 'cephalosporin_4th_5th_gen'
        when drughiclseqno in (4054, 11254, 23241, 35075) then 'carbapenems'
        when drughiclseqno in (4053, 6447) then 'monobactam'
        when drughiclseqno in (6701, 6702, 112384, 12383, 25388) then 'fq'
        when drughiclseqno in (4042, 37442, 8466, 10093) then 'vancomycin'
        when drughiclseqno in (12090, 35990, 4035, 4032, 33290, 4030, 34362, 17765, 16926, 4034, 5887, 3532, 4033) then 'amg'
        when drughiclseqno in (4050, 4051, 4049, 36242) then 'polymixins'
        when drughiclseqno in (21157) then 'linezolid'
        when drughiclseqno in (25673) then 'dapto'
        when drughiclseqno in (4043, 4045, 4704) then 'clinda'
        when drughiclseqno in (4014, 4012, 35908, 36033, 35229, 4013, 5263, 4003, 4015) then 'doxycyclin'
        when drughiclseqno in (4020, 34503, 6334) then 'macrolides'
        when drughiclseqno in (4071) then 'sulfa'
        when drughiclseqno in (4157, 8259, 4156, 7779) then 'metronidazole'
        when drughiclseqno in (4087, 4089, 6322) then 'nitrofurantoin'
        when drughiclseqno in (32986) then 'tigecycline'
        -- below determined by empirical review of available drugs
        -- see "antibiotic-hicl-search.ipynb"
        when drughiclseqno in (3996,33094) then 'ceftriaxone'
        when drughiclseqno in (3984, 3985) then 'cefotaxime'
        when drughiclseqno in (3948,3952,3953) then 'ampicillin_sulbactam'
        when drughiclseqno in (12383, 12384) then 'levofloxacin'
        when drughiclseqno in (20690,25388) then 'moxifloxacin'
        when drughiclseqno in (8738, 32900, 33427) then 'piperacillin_tazobactam'
        when drughiclseqno in (10132,35848,37021) then 'cefepim'
        when drughiclseqno in (11254) then 'meropenem'
        when drughiclseqno in (4054,11254) then 'imipenem'
        when drughiclseqno in (35075) then 'doripenem'
        when drughiclseqno in (3530,4030,4032,33290,34362) then 'gentamicin'
        when drughiclseqno in (3532,4034,5887,17765,39399) then 'tobramycin'
        when drughiclseqno in (4035,35990) then 'amikacin'
      else null end
        as drugname_structured

    -- raw identifiers
    , drugname, drughiclseqno, gtc

    -- delivery info
    , dosage, routeadmin, frequency, prn
    -- , loadingdose
  from `physionet-data.eicu_crd.medication` m
  -- only non-zero dosages
  where dosage is not null
    and drugordercancelled = 'No'
    and routeadmin NOT IN ('OU','OS','OD','AU','AS','AD', 'TP')
    and lower(routeadmin) NOT LIKE '%ear%'
    and lower(routeadmin) NOT LIKE '%eye%'
    and lower(drugname) NOT LIKE '%cream%'
    and lower(drugname) not like '%desensitization%'
    and lower(drugname) not like '%ophth oint%'
    and lower(drugname) not like '%gel%'
)

, meds_part1 as (
    select patientunitstayid, drugorderoffset, drugstartoffset, drugstopoffset
    , drugname, dosage, frequency, routeadmin, drugname_structured
    from meds_part10
    where drugname_structured is not null   
)

, meds_part20 as (
    select *
    , case
      when lower(drugname) like '%azithromycin%' then 1                    -- rank 2  
      when lower(drugname) like '%bacitracin%' then 1                      -- rank 
      when lower(drugname) like '%cefazolin%' then 1                       -- rank 1
      when lower(drugname) like '%cefepime%' then 1                        -- rank 3  
      when lower(drugname) like '%ceftriaxone%' then 1                     -- rank 2 
      when lower(drugname) like '%ceftriaxone sodium%' then 1              -- rank 2
      when lower(drugname) like '%cephulac%' then 1                        -- rank 
      when lower(drugname) like '%ciprofloxacin%' then 1                   -- rank 2
      when lower(drugname) like '%clindamycin%' then 1                     -- rank 2
      when lower(drugname) like '%flagyl%' then 1                          -- rank 1
      when lower(drugname) like '%levofloxacin%' then 1                    -- rank 2
      when lower(drugname) like '%meropenem%' then 1                       -- rank 4
      when lower(drugname) like '%merrem%' then 1                          -- rank 
      when lower(drugname) like '%metronidazole%' then 1                   -- rank 1
      when lower(drugname) like '%mupirocin%' then 1                       -- rank 
      when lower(drugname) like '%piperacillin sod-tazobactam%' then 1     -- rank 3
      when lower(drugname) like '%piperacillin%tazobactam%' then 1         -- rank 3
      when lower(drugname) like '%vancocin%' then 1                        -- rank 2
      when lower(drugname) like '%vancomycin%' then 1                      -- rank 2 
      when lower(drugname) like '%zosyn%' then 1                           -- rank 
      when lower(drugname) like '%levaquin%' then 1                        -- rank 
      when lower(drugname) like '%nafcillin%' then 1                       -- rank 1
      when lower(drugname) like '%rocephin%' then 1                        -- rank 2
      when lower(drugname) like '%tazobactam%' then 1                      -- rank 3
      when lower(drugname) like '%piperacillin%' then 1                    -- rank 3
      when lower(drugname) like '%amika%' then 1
      when lower(drugname) like '%genta%' then 1
      when lower(drugname) like '%tobra%' then 1
      when lower(drugname) like '%cyclin%' then 1
      when lower(drugname) like '%xacin%' then 1
      when lower(drugname) like '%mycin%' then 1
      when lower(drugname) like '%metrondaz%' then 1
      when lower(drugname) like '%clavula%' then 1
      when lower(drugname) like '%tazob%' then 1
      when lower(drugname) like '%penem%' then 1
      when lower(drugname) like '%vancin%' then 1
      when lower(drugname) like '%micin%' then 1
      when lower(drugname) like '%cefur%' then 1
      when lower(drugname) like '%ceftri%' then 1
      when lower(drugname) like '%cephal%' then 1

      else 0 end as antibiotic
    from `physionet-data.eicu_crd.medication` m
    -- only non-zero dosages
    where dosage is not null
        and drugordercancelled = 'No'
        and routeadmin NOT IN ('OU','OS','OD','AU','AS','AD', 'TP')
        and lower(routeadmin) NOT LIKE '%ear%'
        and lower(routeadmin) NOT LIKE '%eye%'
        and lower(drugname) NOT LIKE '%cream%'
        and lower(drugname) not like '%desensitization%'
        and lower(drugname) not like '%ophth oint%'
        and lower(drugname) not like '%gel%'
)

, meds_part2 as (
    select patientunitstayid, drugorderoffset, drugstartoffset, drugstopoffset
    , drugname, dosage, frequency, routeadmin, drugname as drugname_structured
    from meds_part20
    where antibiotic = 1    
)

, meds_antibiotic as (
  select patientunitstayid, drugorderoffset, drugstartoffset, drugstopoffset
  , COALESCE(drugname, drugname_structured) as antibiotic, dosage, frequency, routeadmin
  from (
    select *
    from meds_part1
    union all
    select *
    from meds_part2
  )
)

select distinct *
from meds_antibiotic
order by patientunitstayid, drugstartoffset;