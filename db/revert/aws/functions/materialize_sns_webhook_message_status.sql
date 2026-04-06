-- Revert events:aws/functions/materialize_sns_webhook_message_status from pg

BEGIN;

  drop function aws.materialize_sns_webhook_message_status;

COMMIT;
