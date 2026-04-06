-- Revert events:aws/tables/sns_webhook_messages from pg

BEGIN;

  drop table aws.sns_webhook_messages;

COMMIT;
