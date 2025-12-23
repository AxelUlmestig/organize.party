-- Deploy events:tables/in_progress_worker_jobs to pg

BEGIN;

  create table if not exists in_progress_worker_jobs(
    id uuid not null,
    run_at timestamptz not null,
    failed_attempts int not null,
    definition jsonb not null,

    primary key (id)
  );

COMMIT;
