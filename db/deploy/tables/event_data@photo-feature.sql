-- Deploy events:tables/event_data to pg

BEGIN;

create table if not exists event_data (
  id                        uuid not null,
  title                     text not null,
  description               text not null,
  time_start                timestamp with time zone not null,
  time_end                  timestamp with time zone,
  location                  text not null,
  location_google_maps_link text,
  created_at                timestamptz not null default now(),
  superseded_at             timestamp with time zone,

  foreign key (id) references events (id)
);

create unique index unique_event_data_id_idx
  on event_data (id)
  where superseded_at is null;

COMMIT;
