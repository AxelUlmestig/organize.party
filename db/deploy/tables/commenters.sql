-- Deploy events:tables/commenters to pg

BEGIN;

  create table if not exists commenters (
    event_id uuid not null references events(id),
    email email not null,
    name text,
    gravatar_url text not null,
    deleted_at timestamptz,

    primary key (event_id, email)
  );

  create index if not exists idx_commenters_event_id
    on commenters (event_id);

  -- 👇 Alterations below 👇

  alter table commenters
    add column if not exists created_at timestamptz;

COMMIT;
