-- Revert events:aws/triggers/sns_webhook_messages/start_sns_webhook_message_flow from pg

BEGIN;

  drop trigger if exists start_sns_webhook_message_flow on aws.sns_webhook_messages;
  drop function if exists aws.trig_start_sns_webhook_message_flow();

COMMIT;
