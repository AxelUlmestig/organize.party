-- Revert events:tables/attendee_data from pg

BEGIN;

  drop table if exists attendee_data;

COMMIT;
