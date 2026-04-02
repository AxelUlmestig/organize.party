-- Revert events:email/functions/materialize_email_status from pg

BEGIN;

  drop function email.materialize_email_status;

COMMIT;
