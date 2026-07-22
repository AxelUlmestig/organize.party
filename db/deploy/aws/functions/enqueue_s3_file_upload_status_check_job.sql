-- Deploy events:aws/functions/enqueue_s3_file_upload_status_check_job to pg

BEGIN;
  create or replace function aws.enqueue_s3_file_upload_status_check_job(event_payload fsm_event_payload)
    returns void as
    $$
      insert into job_queue.queued_worker_jobs(
        run_at,
        definition
      )
      select
        now() + interval '250 ms',
        jsonb_build_object(
          'type', 'PollS3FileUpload',
          'payload', jsonb_build_object(
            'photoUploadId', id
          )
        )
      from aws.photo_uploads
      where
        photo_uploads.state_machine_id = (event_payload).machine_id
    $$ language sql strict;

COMMIT;
