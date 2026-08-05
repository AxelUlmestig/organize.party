-- Deploy events:aws/triggers/photo_uploads/create_s3_photo_upload_flow to pg

BEGIN;

    create or replace function aws.trig_create_s3_photo_upload_flow()
    returns trigger as
    $$
        begin
            if new.state_machine_id is null
            then
                select s.id into new.state_machine_id
                from fsm.create_state_machine_with_latest_statechart(
                    shard_id_ => 1,
                    named => 'aws.s3_photo_upload_flow'
                ) s;
            end if;
            return new;
        end
    $$ language plpgsql volatile security definer;

    set client_min_messages TO warning;
    drop trigger if exists create_s3_photo_upload_flow on aws.photo_uploads;
    reset client_min_messages;

    create trigger create_s3_photo_upload_flow
    before insert
    on aws.photo_uploads
    for each row
    execute function aws.trig_create_s3_photo_upload_flow();

COMMIT;
