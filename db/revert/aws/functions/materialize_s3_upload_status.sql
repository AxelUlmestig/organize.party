-- Revert events:aws/functions/materialize_s3_upload_status from pg

BEGIN;

  drop function if exists aws.materialize_s3_upload_status;

COMMIT;
