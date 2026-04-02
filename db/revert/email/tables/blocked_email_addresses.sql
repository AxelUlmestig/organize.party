-- Revert events:email/tables/blocked_email_addresses from pg

BEGIN;

  drop table email.blocked_email_addresses;

COMMIT;
