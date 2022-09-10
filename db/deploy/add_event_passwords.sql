-- Deploy events:add_event_passwords to pg

BEGIN;

create extension pgcrypto;

-- password_salt
alter table events
  add column if not exists
  password_salt text;

update events
  set password_salt = md5(random()::text || clock_timestamp()::text)
  where password_salt is null;

alter table events
  alter column password_salt
  set not null;

-- password_hash
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
