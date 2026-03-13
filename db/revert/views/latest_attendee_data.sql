-- Revert events:views/latest_attendee_data from pg

BEGIN;

  drop view latest_attendee_data;

COMMIT;
