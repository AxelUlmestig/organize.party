-- Deploy events:aws/functions/promote_uploaded_photo to pg

BEGIN;

    create or replace function aws.promote_uploaded_photo(event_payload fsm_event_payload)
        returns void as
    $$
        declare
            state_machine_id_ bigint := (event_payload).machine_id;
        begin
            -- TODO
            return;
        end
    $$ language plpgsql strict;



COMMIT;
