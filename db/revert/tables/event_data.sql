-- Revert events:tables/event_data from pg

BEGIN;

  drop table if exists event_data;

COMMIT;
