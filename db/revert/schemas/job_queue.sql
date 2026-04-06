-- Revert events:schemas/job_queue from pg

BEGIN;

  drop schema job_queue;

COMMIT;
