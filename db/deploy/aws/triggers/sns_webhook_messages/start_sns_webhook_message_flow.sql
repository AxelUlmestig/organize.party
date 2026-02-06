-- Deploy events:aws/triggers/sns_webhook_messages/start_sns_webhook_message_flow to pg

BEGIN;

    create or replace function aws.trig_start_sns_webhook_message_flow()
    returns trigger as
    $$
        begin
            perform fsm.start_machine(
                1,
                new.state_machine_id
            )
            from aws.sns_webhook_messages
            where id = new.id;

            return null;
        end
    $$ language plpgsql volatile security definer;

    create trigger start_sns_webhook_message_flow
    after insert
    on aws.sns_webhook_messages
    for each row
    execute function aws.trig_start_sns_webhook_message_flow();

COMMIT;
