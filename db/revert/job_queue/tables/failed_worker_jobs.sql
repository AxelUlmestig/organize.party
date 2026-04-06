-- Revert events:job_queue/tables/failed_worker_jobs from pg

BEGIN;

  drop table job_queue.failed_worker_jobs;

COMMIT;
