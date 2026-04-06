-- Revert events:email/functions/update_email_sent_at from pg

BEGIN;

  drop function email.update_email_sent_at;

COMMIT;
