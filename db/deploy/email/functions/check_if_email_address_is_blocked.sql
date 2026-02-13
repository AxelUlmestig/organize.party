-- Deploy events:email/functions/check_if_email_address_is_blocked to pg

BEGIN;

    create or replace function email.check_if_email_address_is_blocked(event_payload fsm_event_payload)
        returns void as
    $$
        declare
            state_machine_id_ bigint := (event_payload).machine_id;
            blocked_ bool := false;
            event_ text;
        begin
            select exists(
              select 1
              from email.emails
              join email.blocked_email_addresses
                on blocked_email_addresses.email_address = emails.recipient_email
              where emails.state_machine_id = state_machine_id_
            ) into blocked_;

            if blocked_ then
              event_ = 'email.address_blocked';
            else
              event_ = 'email.address_not_blocked';
            end if;

            perform fsm.notify_state_machine(
              shard => 1,
              machine => state_machine_id_,
              event => event_
            );

            return;
        end
    $$ language plpgsql strict;

COMMIT;
