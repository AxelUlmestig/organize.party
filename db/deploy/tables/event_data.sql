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
  photo_id                  uuid,

  foreign key (id) references events (id),
  foreign key (photo_id) references photos (id)
);

create unique index if not exists unique_event_data_id_idx
  on event_data (id)
  where superseded_at is null;

create index if not exists event_data_photo_id_idx
  on event_data (photo_id);

COMMIT;
