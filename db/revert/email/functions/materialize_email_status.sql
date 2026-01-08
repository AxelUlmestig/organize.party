-- Revert events:email/functions/materialize_email_status from pg

BEGIN;

  drop function if exists email.materialize_email_status;

COMMIT;
