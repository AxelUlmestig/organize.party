-- Revert events:email/functions/enqueue_send_email_job from pg

BEGIN;

  drop function email.enqueue_send_email_job;

COMMIT;
