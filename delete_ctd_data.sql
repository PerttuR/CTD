--NOTE: replace both year parameters
with tmp as (
	select t.id as trip_id, h.id as haul_id from suomu.trip t left join suomu.haul h on h.trip_fk = t.id
	where t.year = 2021
), tmp_meta as (
	select id from suomu.ctd_metadata cm where cm.trip_fk in (select trip_id from tmp) or cm.haul_fk in (select haul_id from tmp)
)
delete from suomu.ctd_data cd where cd.ctd_metadata_fk in (select id from tmp_meta);


with tmp as (
	select t.id as trip_id, h.id as haul_id from suomu.trip t left join suomu.haul h on h.trip_fk = t.id
	where t.year = 2021
)
delete from suomu.ctd_metadata meta where meta.trip_fk in (select tmp.trip_id from tmp) or meta.haul_fk in (select tmp.haul_id from tmp);
