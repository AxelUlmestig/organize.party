-- Revert events:email/tables/email_attachments from pg

BEGIN;

  drop table if exists email.email_attachments;

COMMIT;
