-- Revert events:aws/functions/enqueue_s3_file_upload_status_check_job from pg

BEGIN;

  drop function if exists aws.enqueue_s3_file_upload_status_check_job;

COMMIT;
