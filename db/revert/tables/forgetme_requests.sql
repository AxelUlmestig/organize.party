-- Revert events:tables/forgetme from pg

BEGIN;

  drop function forget_email_address;

COMMIT;
