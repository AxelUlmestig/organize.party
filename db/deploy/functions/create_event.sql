-- Deploy events:functions/create_event to pg

BEGIN;

  create or replace function create_event(
    title_ text,
    description_ text,
    time_start_ timestamptz,
    time_end_ timestamptz,
    location_ text,
    photo_id_ uuid,
    password_ text
  )
  returns jsonb
  as
  $$
    declare
      event_id_ uuid;
    begin
      insert into events (password_salt, password_hash)
        select salt, digest(password_ || salt, 'sha256')
        from (
          select md5(random()::text || clock_timestamp()::text) as salt
        ) t
      returning id into event_id_;

      insert into event_data (id, title, description, time_start, time_end, location, photo_id)
      values (event_id_, title_, description_, time_start_, time_end_, location_, photo_id_);

      return get_event_json(event_id_);
    end;
  $$ language plpgsql;

COMMIT;
