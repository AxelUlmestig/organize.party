-- Revert events:functions/create_event from pg

BEGIN;

  drop function if exists create_event;

COMMIT;
