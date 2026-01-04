-- Revert events:job_queue/tables/completed_worker_jobs from pg

BEGIN;

  drop table if exists job_queue.completed_worker_jobs;

COMMIT;
