-- Deploy events:tables/commenters to pg

BEGIN;

  create table if not exists commenters (
    event_id uuid not null references events(id),
    email email not null,
    name text,
    gravatar_url text not null,

    primary key (event_id, email)
  );

  create index if not exists idx_commenters_event_id
    on commenters (event_id);

  -- 👇 Alterations below 👇

  with
    unique_comment_names as (
      select distinct on (event_id, email)
        event_id,
        email,
        name
      from comments
      order by event_id, email, created_at desc
    )

  insert into commenters
  select
    event_id,
    email,
    name,
    'https://www.gravatar.com/avatar/' || md5(email)
  from unique_comment_names;

COMMIT;
