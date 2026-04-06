-- Revert events:email/triggers/emails/create_email_flow from pg

BEGIN;

    drop trigger create_email_flow on email.emails;
    drop function email.trig_create_email_flow();

COMMIT;
