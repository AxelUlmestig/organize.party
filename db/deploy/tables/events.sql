-- Deploy events:tables/events to pg

begin;

create table events (
  id                        uuid primary key default md5(random()::text || clock_timestamp()::text)::uuid,
  title                     text not null,
  description               text not null,
  time_start                timestamp with time zone not null,
  time_end                  timestamp with time zone not null,
  location                  text not null,
  location_google_maps_link text
);

commit;
