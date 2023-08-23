-- Deploy events:tables/comments to pg

BEGIN;

  create table if not exists comments (
    event_id uuid not null references events(id),
    email email not null,
    name text not null,
    created_at timestamptz not null default now(),
    comment text not null,

    check (comment <> '')
  );

  create index if not exists idx_comments_event_id
    on comments (event_id);

COMMIT;
