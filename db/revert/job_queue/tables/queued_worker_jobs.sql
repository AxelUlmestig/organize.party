-- Revert events:job_queue/tables/queued_worker_jobs from pg

BEGIN;

  drop table if exists job_queuequeued_worker_jobs;

COMMIT;
