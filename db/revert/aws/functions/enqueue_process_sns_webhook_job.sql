-- Revert events:aws/functions/enqueue_process_sns_webhook_job from pg

BEGIN;

  drop function email.enqueue_process_sns_webhook_job;

COMMIT;
