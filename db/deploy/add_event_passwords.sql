-- Deploy events:add_event_passwords to pg

BEGIN;

create extension pgcrypto;

alter table events
  add column if not exists
  password_salt text not null default md5(random()::text || clock_timestamp()::text);

alter table events
  add column if not exists
  password_hash text;

update events
  set password_hash = digest('hunter2' || password_salt, 'sha256')
  where password_hash is null;

alter table events
  alter column password_hash
  set not null;

COMMIT;
