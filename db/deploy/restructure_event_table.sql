-- Deploy events:restucture_event_table to pg

BEGIN;

  -- move data to event_data
  insert into event_data (
    id,
    title,
    description,
    time_start,
    time_end,
    location,
    location_google_maps_link,
    ics_sequence
  )
  select
    id,
    title,
    description,
    time_start,
    time_end,
    location,
    location_google_maps_link,
    0
  from events;

  -- remove columns from events
  alter table events
    drop column title,
    drop column description,
    drop column time_start,
    drop column time_end,
    drop column location,
    drop column location_google_maps_link;

COMMIT;
