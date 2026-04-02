-- Revert events:aws/functions/enqueue_process_sns_webhook_job from pg

BEGIN;

  drop function aws.enqueue_process_sns_webhook_job;

COMMIT;
