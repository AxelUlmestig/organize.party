-- Deploy events:tables/attendees to pg

begin;

create extension if not exists citext;
do $$
  begin
    create domain email as citext
      check ( value ~ '^[a-zA-Z0-9.!#$%&''*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$' );
  exception
    when duplicate_object then null;
end
$$;

do $$
  begin
    create type attendee_status as enum (
      'coming',
      'maybe_coming',
      'not_coming'
    );
  exception
    when duplicate_object then null;
end
$$;

create table if not exists attendees (
  event_id uuid not null references events (id),
  email email not null,
  name text not null,
  status attendee_status not null,
  plus_one bool not null,
  rsvp_at timestamp with time zone not null default now(),
  superseded_at timestamp with time zone,
  comment text
);

create unique index if not exists unique_attendee_idx
  on attendees (event_id, email)
  where superseded_at is null;

-- 👇 Alterations below 👇

alter table attendees
  add column if not exists
  comment text;

commit;
