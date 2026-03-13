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
  gravatar_url text generated always as ('https://www.gravatar.com/avatar/' || md5(email)) stored,
  deleted_at timestamptz,
  unsubscribed_at timestamptz,
  unsubscribe_id uuid not null default md5(random()::text || clock_timestamp()::text)::uuid,
  ics_email_sent bool not null default false,

  primary key (id)
);

create unique index if not exists unique_attendee_idx
  on attendees (event_id, email);

create unique index if not exists unique_unsubscribe_id
  on attendees (unsubscribe_id);

-- 👇 Alterations below 👇

alter table attendees add column ics_email_sent bool;

update attendees set
  ics_email_sent = exists(
    select 1
    from attendee_data as ad
    where
      ad.attendee_id = attendees.id
      and ad.status in ('coming', 'maybe_coming')
  );

alter table attendees
  alter column ics_email_sent set not null,
  alter column ics_email_sent set default false;

commit;
