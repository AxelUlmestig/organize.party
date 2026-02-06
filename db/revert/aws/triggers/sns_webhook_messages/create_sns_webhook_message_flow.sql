-- Revert events:aws/triggers/sns_webhook_messages/create_sns_webhook_message_flow from pg

BEGIN;

    drop trigger if exists create_sns_webhook_message_flow on aws.sns_webhook_messages;
    drop function if exists aws.trig_create_sns_webhook_messages_flow();

COMMIT;
