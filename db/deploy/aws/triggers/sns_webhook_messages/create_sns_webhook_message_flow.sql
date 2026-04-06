-- Deploy events:aws/triggers/sns_webhook_messages/create_sns_webhook_message_flow to pg

BEGIN;

    create or replace function aws.trig_create_sns_webhook_message_flow()
    returns trigger as
    $$
        begin
            if new.state_machine_id is null
            then
                select s.id into new.state_machine_id
                from fsm.create_state_machine_with_latest_statechart(
                    shard_id_ => 1,
                    named => 'aws.sns_webhook_message_flow'
                ) s;
            end if;
            return new;
        end
    $$ language plpgsql volatile security definer;

    set client_min_messages TO warning;
    drop trigger if exists create_sns_webhook_message_flow on aws.sns_webhook_messages;
    reset client_min_messages;

    create trigger create_sns_webhook_message_flow
    before insert
    on aws.sns_webhook_messages
    for each row
    execute function aws.trig_create_sns_webhook_message_flow();

COMMIT;
