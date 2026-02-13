-- Deploy events:email/tables/block_reasons to pg

BEGIN;

  create table if not exists email.block_reasons (
    reason text not null,

    primary key (reason)
  );

  insert into email.block_reasons (reason)
  values
    ('bounced'),
    ('marked_as_spam')
  on conflict do nothing;

COMMIT;
