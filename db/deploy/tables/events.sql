-- Deploy events:tables/events to pg

begin;

create table if not exists events (
  id                        uuid primary key default md5(random()::text || clock_timestamp()::text)::uuid,
  title                     text not null,
  description               text not null,
  time_start                timestamp with time zone not null,
  time_end                  timestamp with time zone,
  location                  text not null,
  location_google_maps_link text,
  password_salt             text not null,
  password_hash             text not null
);

commit;
