-- Revert events:email/functions/check_if_email_address_is_blocked from pg

BEGIN;

  drop function email.check_if_email_address_is_blocked;

COMMIT;
