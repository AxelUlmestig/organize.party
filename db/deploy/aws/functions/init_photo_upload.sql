-- Deploy events:aws/functions/init_photo_upload to pg

BEGIN;

  create or replace function aws.init_photo_upload(
    file_name_ text,
    base64_sha256_ text,
    photo_upload_url_ text,
    upload_url_lifetime_seconds_ int,
    upload_id_ uuid
  )
    returns jsonb as
    $$
      declare
        output_ jsonb := jsonb_build_object(
          'id', null,
          'uploadUrl', null,
          'materializedStatus', null,
          'photoId', null
        );

        in_progress_upload_expires_at_ timestamptz;
        existing_photo_id_ uuid;
      begin
        -- first check if there's already a perfect match
        select id
        into existing_photo_id_
        from photos
        where
          photos.file_base64_sha256 = base64_sha256_
          and photos.name = file_name_;

        if existing_photo_id_ is not null then
          return output_ || jsonb_build_object('photoId', existing_photo_id_, 'materializedStatus', 'upload_completed');
        end if;

        -- then check if there's a file with the same hash but different name.
        -- If so we make a new photo with the new name but the old photo_url
        with
          same_file as (
            select *
            from photos
            where
              photos.file_base64_sha256 = base64_sha256_
            limit 1
          )

          insert into photos (photo_url, name, file_base64_sha256)
          select photo_url, file_name_, base64_sha256_
          from same_file
          returning id
          into existing_photo_id_;

        if existing_photo_id_ is not null then
          return output_ || jsonb_build_object('photoId', existing_photo_id_, 'materializedStatus', 'upload_completed');
        end if;

        -- then check if there's matching file that's in the process of being
        -- uploaded
        select upload_url_expires_at
        into in_progress_upload_expires_at_
        from aws.photo_uploads
        where
          file_base64_sha256 = base64_sha256_
          and materialized_status not in ('upload_completed', 'error')
          and upload_url_expires_at < now();

        if in_progress_upload_expires_at_ is not null then
          raise exception '%', jsonb_build_object('expiresAt', in_progress_upload_expires_at_)::text
            -- 23505 is 'unique_violation', see https://www.postgresql.org/docs/current/errcodes-appendix.html
            using errcode = '23505';
        end if;

        -- if there's no trace of the file being uploaded before we'll start
        -- the upload
        insert into aws.photo_uploads (id, file_name, upload_url, file_base64_sha256, upload_url_expires_at)
        values (upload_id_, file_name_, photo_upload_url_, base64_sha256_, now() + upload_url_lifetime_seconds_ * interval '1 second')
        returning jsonb_build_object(
          'id', id,
          'uploadUrl', upload_url,
          'materializedStatus', materialized_status,
          'photoId', photo_id
        )
        into output_;

        return output_;
      end
    $$ language plpgsql strict;

COMMIT;
