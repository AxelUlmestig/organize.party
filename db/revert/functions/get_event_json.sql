-- Revert events:functions/get_event_json from pg

BEGIN;

  drop function get_event_json;

COMMIT;
