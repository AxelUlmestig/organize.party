-- Deploy events:tables/attendees to pg

begin;

create extension citext;
create domain email as citext
  check ( value ~ '^[a-zA-Z0-9.!#$%&''*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$' );

create type attendee_status as enum (
  'coming',
  'maybe_coming',
  'not_coming'
);

create table attendees (
  event_id uuid not null references events (id),
  email email not null,
  name text not null,
  status attendee_status not null,
  plus_one bool not null,
  rsvp_at timestamp with time zone not null default now(),
  superseded_at timestamp with time zone
);

create unique index unique_attendee_idx
  on attendees (event_id, email)
  where superseded_at is null;

commit;
