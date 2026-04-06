-- Revert events:schemas/email from pg

BEGIN;

  drop schema email;

COMMIT;
