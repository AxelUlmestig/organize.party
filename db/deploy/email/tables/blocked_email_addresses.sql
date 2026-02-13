-- Deploy events:email/tables/blocked_email_addresses to pg

BEGIN;

  create table if not exists email.blocked_email_addresses (
    created_at timestamptz not null default now(),
    email_address text not null,
    reason text not null,

    primary key (email_address, reason),

    foreign key (reason)
      references email.block_reasons (reason)
      on update cascade
  );

COMMIT;
