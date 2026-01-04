-- Revert events:job_queue/tables/in_progress_worker_jobs from pg

BEGIN;

  drop table if exists job_queue.in_progress_worker_jobs;

COMMIT;
