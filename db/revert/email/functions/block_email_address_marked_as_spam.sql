-- Revert events:email/functions/block_email_address_marked_as_spam from pg

BEGIN;

  drop function email.block_email_address_marked_as_spam;

COMMIT;
