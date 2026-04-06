-- Deploy events:job_queue/tables/completed_worker_jobs to pg

BEGIN;

  create table if not exists job_queue.completed_worker_jobs(
    id uuid not null,
    run_at timestamptz not null,
    picked_up_at timestamptz not null,
    finished_at timestamptz not null default clock_timestamp(),
    failed_attempts int not null,
    definition jsonb not null,

    primary key (id)
  );

COMMIT;
