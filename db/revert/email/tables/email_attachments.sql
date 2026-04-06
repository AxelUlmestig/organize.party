-- Revert events:email/tables/email_attachments from pg

BEGIN;

  drop table email.email_attachments;

COMMIT;
