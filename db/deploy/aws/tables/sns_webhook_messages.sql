-- Deploy events:aws/tables/sns_webhook_messages to pg

BEGIN;

  create table if not exists aws.sns_webhook_messages (
    id uuid not null default uuidv7(),
    received_at timestamptz not null default now(),
    state_machine_id bigint not null,
    aws_sns_id text not null,
    materialized_status text not null default 'queued',
    contents jsonb not null,

    primary key (id)
  );

  create unique index if not exists idx_aws_sns_webhook_messages_state_machine_id
    on aws.sns_webhook_messages (state_machine_id);

  create unique index if not exists idx_aws_sns_webhook_messages_aws_sns_id
    on aws.sns_webhook_messages (aws_sns_id);

  comment on  column aws.sns_webhook_messages.aws_sns_id is
  $$
  The ID given by AWS so we can detect if we get the same message sent to us twice
  $$;

COMMIT;
