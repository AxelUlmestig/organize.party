-- Revert events:email/triggers/emails/create_email_flow from pg

BEGIN;

    drop trigger if exists create_email_flow on email.emails;
    drop function if exists email.trig_create_email_flow();

COMMIT;
