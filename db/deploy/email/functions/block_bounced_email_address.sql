-- Deploy events:email/functions/block_bounced_email_address to pg

BEGIN;

    create or replace function email.block_bounced_email_address(event_payload fsm_event_payload)
        returns void as
    $$
        declare
            state_machine_id_ bigint := (event_payload).machine_id;
        begin
            insert into email.blocked_email_addresses (
              email_address,
              reason,
              context
            )
            select
              recipient_email,
              'bounced',
              (event_payload).data->>'block_context'
            from email.emails
            where state_machine_id = state_machine_id_
            on conflict do nothing;

            return;
        end
    $$ language plpgsql strict;

COMMIT;
