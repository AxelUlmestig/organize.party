-- Revert events:aws/triggers/sns_webhook_messages/create_sns_webhook_message_flow from pg

BEGIN;

    drop trigger create_sns_webhook_message_flow on aws.sns_webhook_messages;
    drop function aws.trig_create_sns_webhook_message_flow;

COMMIT;
