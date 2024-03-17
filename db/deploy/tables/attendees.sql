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
  id bigint not null generated always as identity,
  event_id uuid not null references events (id),
  email email,
  gravatar_url text,
  deleted_at timestamptz,

  primary key (id)
);

create unique index if not exists unique_attendee_idx
  on attendees (event_id, email);

-- 👇 Alterations below 👇

alter table attendees
  add column if not exists id bigint not null generated always as identity,
  add column if not exists gravatar_url text generated always as ('https://www.gravatar.com/avatar/' || md5(email)) stored,
  add primary key (id);

create table if not exists attendee_data (
  attendee_id bigint not null,
  name text not null,
  status attendee_status,
  plus_one bool not null default false,
  get_notified_on_comments bool not null default false,
  rsvp_at timestamp with time zone not null default now(),
  superseded_at timestamp with time zone,

  foreign key (attendee_id)
    references attendees(id)
    on delete cascade
);

insert into attendee_data (
  attendee_id,
  name,
  status,
  plus_one,
  get_notified_on_comments,
  rsvp_at,
  superseded_at
)
select
  (select id from attendees where attendees.email = to_insert.email and superseded_at is null and event_id = to_insert.event_id) as attendee_id,
  name,
  status,
  plus_one,
  get_notified_on_comments,
  rsvp_at,
  superseded_at
from attendees as to_insert;

delete from attendees where superseded_at is not null;

alter table attendees
  drop column superseded_at,
  add column deleted_at timestamptz,
  alter column email drop not null,
  drop column name,
  drop column status,
  drop column rsvp_at,
  drop column plus_one,
  drop column get_notified_on_comments,
  add constraint email_unless_deleted check (
    (deleted_at is null and email is not null and gravatar_url is not null)
    or (deleted_at is not null and email is null and gravatar_url is null)
  );

create unique index if not exists unique_attendee_idx
  on attendees (event_id, email);

commit;
