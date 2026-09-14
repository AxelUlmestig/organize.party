-- Revert events:functions/add_comment from pg

BEGIN;

  drop function add_comment;

COMMIT;
