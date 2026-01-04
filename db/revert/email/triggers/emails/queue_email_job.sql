-- Revert events:email/triggers/emails/queue_email_job from pg

BEGIN;

  drop function if exists email.queue_email_job();

COMMIT;
