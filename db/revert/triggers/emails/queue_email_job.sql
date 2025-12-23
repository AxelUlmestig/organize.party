-- Revert events:triggers/emails/queue_email_job from pg

BEGIN;

  drop function if exists queue_email_job();

COMMIT;
