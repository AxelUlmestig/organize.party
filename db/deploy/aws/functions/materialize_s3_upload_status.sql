-- Deploy events:aws/functions/materialize_s3_upload_status to pg

BEGIN;

    create or replace function aws.materialize_s3_upload_status(event_payload fsm_event_payload)
        returns void as
    $$
        declare
            state_machine_id_ bigint := (event_payload).machine_id;
            to_state_ text := (event_payload).to_state;
        begin
            update aws.photo_uploads set
              materialized_status = to_state_
            where
              state_machine_id = state_machine_id_;

            return;
        end
    $$ language plpgsql strict;

COMMIT;
