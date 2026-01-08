-- Deploy events:email/functions/update_email_sent_at to pg

BEGIN;

    create or replace function email.update_email_sent_at(event_payload fsm_event_payload)
        returns void as
    $$
        declare
            state_machine_id_ bigint := (event_payload).machine_id;
        begin
            update email.emails set
              sent_at = now()
            where
              state_machine_id = state_machine_id_;

            return;
        end
    $$ language plpgsql strict;

COMMIT;
