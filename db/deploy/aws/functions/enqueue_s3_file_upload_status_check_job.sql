-- Deploy events:aws/functions/enqueue_s3_file_upload_status_check_job to pg

BEGIN;

    create or replace function aws.enqueue_s3_file_upload_status_check_job(event_payload fsm_event_payload)
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
