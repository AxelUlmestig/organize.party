-- Revert events:job_queue/tables/failed_worker_jobs from pg

BEGIN;

  drop table if exists job_queue.failed_worker_jobs;

COMMIT;
