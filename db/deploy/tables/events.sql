-- Deploy events:tables/events to pg

begin;

create table if not exists events (
  id                        uuid primary key default md5(random()::text || clock_timestamp()::text)::uuid,
  password_salt             text not null,
  password_hash             text not null
);

commit;
