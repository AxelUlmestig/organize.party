-- Deploy events:tables/comments to pg

BEGIN;

  create table if not exists comments (
    event_id uuid not null references events(id),
    email email not null,
    created_at timestamptz not null default now(),
    comment text not null,
    force_notification_on_comment bool not null default false,

    check (comment <> ''),

    foreign key (event_id, email)
      references commenters(event_id, email)
      on update cascade
  );

  create index if not exists idx_comments_event_id
    on comments (event_id);

  -- trigger for syncing commenter names with attendees
  create or replace function trig_sync_commenters_attendees_names()
    returns trigger as
    $$
      begin
        update attendees
        set name = commenters.name
        from commenters
        where attendees.event_id = commenters.event_id
          and attendees.email = commenters.email
          and commenters.event_id = new.event_id
          and commenters.email = new.email;

        if found then
          update commenters
            set name = null
            where event_id = new.event_id
              and email = new.email;
        end if;

        return new;
      end;
    $$
    language plpgsql;

    set client_min_messages TO warning;
    drop trigger if exists sync_commenters_attendees_names on comments;
    reset client_min_messages;

    create trigger sync_commenters_attendees_names
    before insert
    on comments
    for each row
    execute function trig_sync_commenters_attendees_names();

  -- 👇 Alterations below 👇

  alter table comments
    drop column if exists name;

COMMIT;
