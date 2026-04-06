-- Revert events:functions/forget_email_address from pg

BEGIN;

  drop function forget_email_address;

COMMIT;
