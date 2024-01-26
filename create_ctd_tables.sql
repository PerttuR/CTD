set role suomu_adm;
begin;

CREATE TABLE suomu.ctd_metadata (
    id BIGINT PRIMARY KEY,
    handler_fk BIGINT references suomu.handler (id),
    trip_fk BIGINT references suomu.trip (id),
    haul_fk BIGINT references suomu.haul (id),
    ctd_calculation_time TIMESTAMP WITH TIME ZONE,
    "location" GEOMETRY(POINT, 4326),
    bottom_depth DOUBLE PRECISION, -- Using DOUBLE PRECISION for floating-point
    device_category_code integer,
    aranda_index integer,
    ctd_device TEXT,
    CONSTRAINT one_of_trip_or_haul CHECK (
        (trip_fk IS NOT NULL AND haul_fk IS NULL) OR
        (trip_fk IS NULL AND haul_fk IS NOT NULL)
    )
);

grant select on suomu.ctd_metadata to suomu_r;
grant all on suomu.ctd_metadata to suomu_rw;

end;