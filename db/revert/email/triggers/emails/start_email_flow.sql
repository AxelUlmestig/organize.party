-- Revert events:email/triggers/emails/start_email_flow from pg

BEGIN;

  drop trigger if exists start_email_flow on email.emails;
  drop function if exists email.trig_start_email_flow();

COMMIT;
