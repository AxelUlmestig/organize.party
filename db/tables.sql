create extension citext;
create domain email as citext
  check ( value ~ '^[a-zA-Z0-9.!#$%&''*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$' );

create table events (
  id                        uuid primary key default md5(random()::text || clock_timestamp()::text)::uuid,
  title                     text not null,
  description               text not null,
  time_start                timestamp with time zone not null,
  time_end                  timestamp with time zone not null,
  location                  text not null,
  location_google_maps_link text
);

create type attendee_status as enum (
  'coming',
  'maybe_coming',
  'not_coming'
);

create table attendees (
  event_id uuid not null references events (id),
  email email not null,
  first_name text not null,
  last_name text not null,
  status attendee_status not null,
  plus_one bool not null,
  rsvp_at timestamp with time zone not null default now(),
  superseded_at timestamp with time zone
);

create unique index unique_attendee_idx
  on attendees (event_id, email)
  where superseded_at is null;
