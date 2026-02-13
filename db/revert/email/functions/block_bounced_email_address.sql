-- Revert events:email/functions/block_bounced_email_address from pg

BEGIN;

  drop function email.blocked_bounced_email_address;

COMMIT;
