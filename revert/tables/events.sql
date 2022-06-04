-- Revert events:tables/events from pg

begin;

drop table if exists events;

commit;
