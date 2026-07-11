-- Deploy events:aws/triggers/photo_uploads/start_s3_photo_upload_flow to pg

BEGIN;

    create or replace function aws.trig_start_s3_photo_upload_flow()
    returns trigger as
    $$
        begin
            perform fsm.start_machine(
                shard => 1,
                machine_id => new.state_machine_id
            )
            from aws.photo_uploads
            where id = new.id;

            return null;
        end
    $$ language plpgsql volatile security definer;

    create trigger start_s3_photo_upload_flow
    after insert
    on aws.photo_uploads
    for each row
    execute function aws.trig_start_s3_photo_upload_flow();

COMMIT;
