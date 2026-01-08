-- Deploy events:email/triggers/emails/create_email_flow to pg

BEGIN;

    create or replace function email.trig_create_email_flow()
    returns trigger as
    $$
        begin
            if new.state_machine_id is null
            then
                select s.id into new.state_machine_id
                from fsm.create_state_machine_with_latest_statechart(
                    1,
                    'email.email_flow'
                ) s;
            end if;
            return new;
        end
    $$ language plpgsql volatile security definer;

    set client_min_messages TO warning;
    drop trigger if exists create_email_flow on email.emails;
    reset client_min_messages;

    create trigger create_email_flow
    before insert
    on email.emails
    for each row
    execute function email.trig_create_email_flow();

COMMIT;
