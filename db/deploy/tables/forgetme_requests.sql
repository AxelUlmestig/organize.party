-- Deploy events:tables/forgetme to pg

BEGIN;

  create table if not exists forgetme_requests (
    id            uuid default md5(random()::text || clock_timestamp()::text)::uuid,
    email         text,
    requested_at  timestamptz not null default now(),
    deleted_at    timestamptz,

    primary key (id),

    -- once the request is fulfilled, the email is removed and the deleted_at
    -- is set
    constraint deleted_at_or_email_required
      check (deleted_at is null <> email is null)
  );

  create index if not exists forgetme_requests_email_idx
    on forgetme_requests (email);

COMMIT;
