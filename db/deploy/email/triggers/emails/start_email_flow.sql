-- Deploy events:email/triggers/emails/start_email_flow to pg

BEGIN;

    create or replace function email.trig_start_email_flow()
    returns trigger as
    $$
        begin

            perform fsm.start_machine(
                shard => 1,
                machine_id => new.state_machine_id
            )
            from email.emails
            where id = new.id;

            return null;
        end
    $$ language plpgsql volatile security definer;

    create trigger start_email_flow
    after insert
    on email.emails
    for each row
    execute function email.trig_start_email_flow();

COMMIT;
