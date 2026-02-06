-- Deploy events:aws/functions/materialize_sns_webhook_message_status to pg

BEGIN;

    create or replace function aws.materialize_sns_webhook_message_status(event_payload fsm_event_payload)
        returns void as
    $$
        declare
            state_machine_id_ bigint := (event_payload).machine_id;
            to_state_ text := (event_payload).to_state;
        begin
            update aws.sns_webhook_messages set
              materialized_status = to_state_
            where
              state_machine_id = state_machine_id_;

            return;
        end
    $$ language plpgsql strict;

COMMIT;
