-- Revert events:aws/functions/init_photo_upload from pg

BEGIN;

  drop function aws.init_photo_uploads;

COMMIT;
