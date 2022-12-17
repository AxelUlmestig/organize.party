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
    location_google_maps_link
  )
  select
    id,
    title,
    description,
    time_start,
    time_end,
    location,
    location_google_maps_link
  from events;

  -- remove columns from events
  alter table events
    add column created_at timestamptz not null default now(),
    drop column title,
    drop column description,
    drop column time_start,
    drop column time_end,
    drop column location,
    drop column location_google_maps_link;

COMMIT;
