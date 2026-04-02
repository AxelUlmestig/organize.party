-- Revert events:functions/edit_event from pg

BEGIN;

  drop function edit_event;

COMMIT;
