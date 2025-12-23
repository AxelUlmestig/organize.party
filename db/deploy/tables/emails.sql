-- Deploy events:tables/emails to pg

BEGIN;

  create table if not exists emails (
    id uuid not null default uuidv7(),
    created_at timestamptz not null default now(),
    sent_at timestamptz,
    recipient_email text not null,
    recipient_name text,
    subject text,
    body text,

    primary key (id)
  );

COMMIT;
