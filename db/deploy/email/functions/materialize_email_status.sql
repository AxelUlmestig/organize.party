-- Deploy events:email/functions/materialize_email_status to pg

BEGIN;

    create or replace function email.materialize_email_status(event_payload fsm_event_payload)
        returns void as
    $$
        declare
            state_machine_id_ bigint := (event_payload).machine_id;
            to_state_ text := (event_payload).to_state;
        begin
            update email.emails set
              materialized_status = to_state_
            where
              state_machine_id = state_machine_id_;

            return;
        end
    $$ language plpgsql strict;

COMMIT;
