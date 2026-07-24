-- Deploy events:aws/functions/promote_uploaded_photo to pg

BEGIN;

    create or replace function aws.promote_uploaded_photo(event_payload fsm_event_payload)
        returns void as
    $$
        declare
            state_machine_id_ bigint := (event_payload).machine_id;
            event_payload_ jsonb := (event_payload).data;
        begin
            if event_payload_->>'photoUrl' is null then
              raise exception 'Expected photoUrl to be present in promote_upload_photo payload: %', event_payload_;
            end if;

            with
              inserted_photos as (
                insert into photos (id, name, file_base64_sha256, photo_url)
                select id, file_name, file_base64_sha256, event_payload_->>'photoUrl'
                from aws.photo_uploads
                where photo_uploads.state_machine_id = state_machine_id_
                returning *
              )

              update aws.photo_uploads set
                photo_id = inserted_photos.id
              from inserted_photos
              where photo_uploads.state_machine_id = state_machine_id_;

            return;
        end
    $$ language plpgsql strict;



COMMIT;
