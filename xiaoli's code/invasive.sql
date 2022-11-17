-- v2
-- by xiaoli liu, 2022.01.10

drop table if exists `icu-elos.eicu_sepsis.ventaliation_info_time_0_eicu`;
create table `icu-elos.eicu_sepsis.ventaliation_info_time_0_eicu` as

with ventaliation_info_initial as (
    select ce.patientunitstayid
	, activeupondischarge
    , hospitaldischargeoffset
	, cplitemoffset
	, cplgroup
	, cplitemvalue
	, case 
	when cplitemvalue in (
        'Intubated/nasal ETT'
        , 'Intubated/oral ETT'
        , 'Intubated/trach-acute'
        , 'Intubated/trach-chronic'
	    

    ) then 'invasive'
    when cplitemvalue = "/&/" then 'non-invasive' else null end as vent_type
    , case
    when cplitemvalue = '/&/' then 0 else 1 end as vent_start_flag   
    from `physionet-data.eicu_crd.careplangeneral` ce 
    inner join `physionet-data.eicu_crd_derived.icustay_detail` icud 
    on ce.patientunitstayid = icud.patientunitstayid
    and ce.cplitemoffset <= icud.hospitaldischargeoffset -- we drop the info out of the same icu admitted
	where cplgroup in ('Airway', 'Ventilation') 
	and cplitemvalue in (
        'Intubated/nasal ETT'
        , 'Intubated/oral ETT'
        , 'Intubated/trach-acute'
        , 'Intubated/trach-chronic'
    )
)

, ventaliation_info_time_00 as (
    select patientunitstayid, cplitemoffset
    , hospitaldischargeoffset
    , case when vent_type = 1 then 'invasive' 
    when vent_type = 0 then 'non-invasive'
    else null end as vent_type 
    , activeupondischarge, vent_start_flag
    from (
        select patientunitstayid, cplitemoffset, hospitaldischargeoffset
        , max(case when vent_type = 'invasive' then 1 
                when vent_type = 'non-invasive' then 0 
                else null end
                ) as vent_type
        , max(case when activeupondischarge is true then 1 else 0 end) as activeupondischarge
        , max(case when vent_start_flag = 1 then 1 else 0 end) as vent_start_flag
        from ventaliation_info_initial
        group by patientunitstayid, cplitemoffset, hospitaldischargeoffset
    )
)

-- drop: the first row with 'Spontaneous - adequate' and activeupondischarge = 'False'
-- later existing ventilation records
, ventaliation_info_time_01 as (
    select patientunitstayid, hospitaldischargeoffset, cplitemoffset, vent_type, vent_start_flag, activeupondischarge
    from (
        select *, case when rn = 1 and vent_start_flag = 0 and activeupondischarge = 0 then 1 else 0 end as drop_flag
        from (
            select *, ROW_NUMBER() OVER (PARTITION BY patientunitstayid ORDER BY cplitemoffset) as rn
            from ventaliation_info_time_00
        )
    )
    where drop_flag = 0
)

-- drop patients: the first row with 'Spontaneous - adequate' and activeupondischarge = 'True' and no existing 'invasive'
-- we thought they didn't receive ventilation
, drop_info_part_0 as (
    select v1.*, ROW_NUMBER() OVER (PARTITION BY v1.patientunitstayid ORDER BY cplitemoffset) as rn
    from ventaliation_info_time_01 v1
    inner join (
        select patientunitstayid, max(case when vent_type = 'invasive' then 1 else 0 end) as flag
        from ventaliation_info_time_01
        group by patientunitstayid
    ) v2
    on v1.patientunitstayid = v2.patientunitstayid
    and v2.flag = 0
)

, drop_info_part as (
    select distinct patientunitstayid
    from drop_info_part_0
    where rn = 1 and activeupondischarge = 1
)

, ventaliation_info_time_0 as (
    select patientunitstayid, hospitaldischargeoffset, cplitemoffset, vent_type, vent_start_flag, activeupondischarge
    from ventaliation_info_time_01
    where patientunitstayid not in (
        select patientunitstayid from drop_info_part
    )
)

select *
from ventaliation_info_time_0;


drop table if exists `icu-elos.eicu_sepsis.ventaliation_info_use_4_eicu`;
create table `icu-elos.eicu_sepsis.ventaliation_info_use_4_eicu` as

with ventaliation_info_initial as (
    select ce.patientunitstayid
	, activeupondischarge
    , hospitaldischargeoffset
	, cplitemoffset
	, cplgroup
	, cplitemvalue
	, case 
	when cplitemvalue in (
        'Intubated/nasal ETT'
        , 'Intubated/oral ETT'
        , 'Intubated/trach-acute'
        , 'Intubated/trach-chronic'

    ) then 'invasive'
    when cplitemvalue = "/&/" then 'non-invasive' else null end as vent_type
    , case
    when cplitemvalue = '/&/' then 0 else 1 end as vent_start_flag   
    from `physionet-data.eicu_crd.careplangeneral` ce 
    inner join `physionet-data.eicu_crd_derived.icustay_detail` icud 
    on ce.patientunitstayid = icud.patientunitstayid
    and ce.cplitemoffset <= icud.hospitaldischargeoffset -- we drop the info out of the same icu admitted
	where cplgroup in ('Airway', 'Ventilation') 
	and cplitemvalue in (
        'Intubated/nasal ETT'
        , 'Intubated/oral ETT'
        , 'Intubated/trach-acute'
        , 'Intubated/trach-chronic'

    )
)

, ventaliation_info_time_0 as (
    select *
    from `icu-elos.eicu_sepsis.ventaliation_info_time_0_eicu`
)

-- identify the 'non-invasive' dischargestatus true, while existing 'invasive' type
-- which should be changed to end before the 'invasive' start
, change_info_part as (
    select v1.patientunitstayid, v1.cplitemoffset
    from (
        select *
        from ventaliation_info_time_0
        where vent_type = 'non-invasive'
        and activeupondischarge = 1
    ) v1
    inner join (
        select *
        from ventaliation_info_time_0
        where vent_type = 'invasive'
    ) v2
    on v1.patientunitstayid = v2.patientunitstayid
    and v1.cplitemoffset < v2.cplitemoffset
    where v1.patientunitstayid in (
        select distinct patientunitstayid 
        from ventaliation_info_initial
        where vent_type = 'non-invasive'
        and activeupondischarge is true
        and patientunitstayid in (
            select distinct patientunitstayid
            from ventaliation_info_initial
            where vent_type = 'invasive' 
        )
    )
)

, ventaliation_info_time_1 as (
    select v0.patientunitstayid, v0.cplitemoffset
    , v0.hospitaldischargeoffset
    , case 
    when v0.patientunitstayid = ci.patientunitstayid and v0.cplitemoffset = ci.cplitemoffset then 0
    else v0.activeupondischarge end as activeupondischarge 
    , vent_type, vent_start_flag
    from ventaliation_info_time_0 v0 
    left join change_info_part ci 
    on v0.patientunitstayid = ci.patientunitstayid
    and v0.cplitemoffset = ci.cplitemoffset
)

, ventaliation_info_time as (
    select *
    from (
        select patientunitstayid, cplitemoffset, vent_type, vent_start_flag
        from ventaliation_info_time_1
        union all
        select patientunitstayid, hospitaldischargeoffset as cplitemoffset, vent_type, 0 as vent_start_flag
        from ventaliation_info_time_1
        where activeupondischarge = 1
        union all -- the last record was vent with false status, since we didn't know the end time, we set adding 60min as endtime
        select patientunitstayid, (cplitemoffset + 60) as cplitemoffset, vent_type, 0 as vent_start_flag
        from (
            select *
            from (
                select *, ROW_NUMBER() OVER (PARTITION BY patientunitstayid ORDER BY cplitemoffset desc) as rn
                from ventaliation_info_time_1
            )
            where rn = 1 and activeupondischarge = 0 and vent_start_flag = 1
        )
    )
    order by patientunitstayid, cplitemoffset
)

-- user_id, key, sort
-- patientid, vent_start_flag, cplitoffset
-- https://www.postgresql.org/message-id/20130204103454.0b3c6b23@tucholsky.experteer.muc
-- https://www.postgresql.org/message-id/CAGnEbohhKmW55oB0FpQd3naXBkwi74E%3D8DRZBGFjS-MeGpXLaA%40mail.gmail.com
-- https://stackoverflow.com/questions/10614505/window-functions-and-more-local-aggregation/10624628#10624628

, ventaliation_info_use_0 AS (
    SELECT patientunitstayid, vent_start_flag, cplitemoffset
    , CASE 
    WHEN lag(vent_start_flag) OVER (PARTITION BY patientunitstayid ORDER BY patientunitstayid, cplitemoffset) = vent_start_flag 
    THEN NULL ELSE 1 END r
    FROM ventaliation_info_time
)

,  ventaliation_info_use_1 AS (
    SELECT patientunitstayid, vent_start_flag, cplitemoffset, r
    , sum(r) OVER (ORDER BY patientunitstayid, cplitemoffset) grp
    FROM ventaliation_info_use_0
)

, ventaliation_info_use_2 as (
    SELECT min(patientunitstayid) as patientunitstayid, min(vent_start_flag) as vent_start_flag,
    min(cplitemoffset) as sort_first,
    max(cplitemoffset) as sort_last
    FROM ventaliation_info_use_1
    GROUP BY grp
)

-- get the start and end time of each patient
, ventaliation_info_use_3 as (
    select patientunitstayid, vent_start_flag
    , case when vent_start_flag = 1 then sort_first
    when vent_start_flag = 0 then sort_last
    end as cplitemoffset
    from ventaliation_info_use_2
)

-- here we check the abnormal types:
-- non-invasive cplitemoffset = hospitaldischargeoffset
-- only existing 'Spontaneous - adequate' records
, ventaliation_info_use_4 as (
    select *
    from ventaliation_info_use_3
    where patientunitstayid not in (
        select distinct patientunitstayid
        from (
            select patientunitstayid, sum(vent_start_flag) as num
            from ventaliation_info_use_3
            group by patientunitstayid
        )
        where num = 0
    )
)

select * -- patientunitstayid, cplitemoffset as starttime
from ventaliation_info_use_4
order by patientunitstayid, cplitemoffset;



drop table if exists `icu-elos.eicu_sepsis.invasive`;
create table `icu-elos.eicu_sepsis.invasive` as

-- can't cover 4 patients with special types, we manually set them by checking initial info
-- patientunitstayid in (1565479, 1571346, 1571446, 1589211)

with ventaliation_info_use_4 as (
    select *
    from `icu-elos.eicu_sepsis.ventaliation_info_use_4_eicu`
)

, pivoted_vent_eicu_0 as (
    select *
    from ventaliation_info_use_4
    where patientunitstayid not in (1565479, 1571346, 1571446, 1589211)
    union all
    select patientunitstayid
    , 1 as vent_start_flag
    , case when patientunitstayid = 1571346 then 2091
    when patientunitstayid = 1571446 then 1430
    when patientunitstayid = 1589211 then 2372
    else null end as cplitemoffset
    from ventaliation_info_use_4
    where patientunitstayid in (1571346, 1571446, 1589211)  -- drop 1565479
    union all
    select patientunitstayid
    , 0 as vent_start_flag
    , case when patientunitstayid = 1571346 then 5170
    when patientunitstayid = 1571446 then (1430+60)
    when patientunitstayid = 1589211 then 4903
    else null end as cplitemoffset
    from ventaliation_info_use_4
    where patientunitstayid in (1571346, 1571446, 1589211)  -- drop 1565479
) 

, pivoted_vent_eicu_1 as (
    select patientunitstayid, vent_start_flag, cplitemoffset
    from pivoted_vent_eicu_0
    order by patientunitstayid, cplitemoffset, vent_start_flag
)

, pivoted_vent_eicu_s as (
    select patientunitstayid, cplitemoffset as starttime
    , ROW_NUMBER() OVER (PARTITION BY patientunitstayid ORDER BY cplitemoffset desc) as rn
    from pivoted_vent_eicu_0
    where vent_start_flag = 1
)

, pivoted_vent_eicu_e as (
    select patientunitstayid, cplitemoffset as endtime
    , ROW_NUMBER() OVER (PARTITION BY patientunitstayid ORDER BY cplitemoffset desc) as rn
    from pivoted_vent_eicu_1
    where vent_start_flag = 0
)

select ps.patientunitstayid, ps.starttime, pe.endtime
from pivoted_vent_eicu_s ps 
inner join pivoted_vent_eicu_e pe 
on ps.patientunitstayid = pe.patientunitstayid
and ps.rn = pe.rn
order by ps.patientunitstayid, ps.starttime;



drop table if exists `icu-elos.eicu_sepsis.ventaliation_info_time_0_eicu`;
drop table if exists `icu-elos.eicu_sepsis.ventaliation_info_use_4_eicu`;