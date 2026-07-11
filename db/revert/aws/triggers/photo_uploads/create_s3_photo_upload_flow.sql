-- Revert events:aws/triggers/photo_uploads/create_s3_photo_upload_flow from pg

BEGIN;

  drop trigger create_s3_photo_upload_flow on aws.photo_uploads;
  drop function aws.trig_create_s3_photo_upload_flow();

COMMIT;
