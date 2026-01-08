-- Deploy events:email/tables/emails to pg

BEGIN;

  create table if not exists email.emails (
    id uuid not null default uuidv7(),
    created_at timestamptz not null default now(),
    sent_at timestamptz,
    state_machine_id bigint not null,
    recipient_email text not null,
    recipient_name text,
    subject text,
    body text,
    materialized_status text not null default 'queued',

    primary key (id)
  );

  create index if not exists idx_email_emails_state_machine_id
    on email.emails (state_machine_id);

COMMIT;
