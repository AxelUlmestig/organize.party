-- Revert events:aws/triggers/photo_uploads/start_s3_photo_upload_flow from pg

BEGIN;

  drop trigger start_s3_photo_upload_flow on aws.photo_uploads;
  drop function aws.trig_start_s3_photo_upload_flow();

COMMIT;
