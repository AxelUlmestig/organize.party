-- Revert events:email/triggers/emails/start_email_flow from pg

BEGIN;

  drop trigger start_email_flow on email.emails;
  drop function email.trig_start_email_flow();

COMMIT;
