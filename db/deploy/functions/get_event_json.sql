-- Deploy events:functions/get_event_json to pg

BEGIN;

  create or replace function get_event_json(event_id_ uuid)
  returns jsonb
  as
  $$
    declare
      output_ jsonb;
      attendees_ jsonb;
      comments_ jsonb;
    begin
      -- Get event base info
      select
        jsonb_build_object(
          'id', event_data.id,
          'title', event_data.title,
          'description', event_data.description,
          'startTime', event_data.time_start,
          'endTime', event_data.time_end,
          'location', event_data.location,
          'googleMapsLink', event_data.location_google_maps_link,
          'createdAt', events.created_at,
          'modifiedAt', event_data.created_at
        )
      into output_
      from event_data
      join events
        on events.id = event_data.id
      where
        event_data.id = event_id_
        and event_data.superseded_at is null;

      if output_ is null then
        return null;
      end if;

      -- Get event attendees
      select
        coalesce(
          jsonb_agg(
            jsonb_build_object(
              'name', name,
              'plusOne', plus_one,
              'status',
                case status
                  when 'coming' then 'Coming'
                  when 'maybe_coming' then 'MaybeComing'
                  when 'not_coming' then 'NotComing'
                  else status::text
                end
            )
          ),
          '[]'::jsonb
        )
      into attendees_
      from latest_attendee_data
      where
        event_id = event_id_
        and status is not null -- status is null when you comment without RSVPing
        and deleted_at is null;

      output_ := jsonb_set(output_, '{attendees}', attendees_);

      -- Get event comments
      select
        coalesce(
          jsonb_agg(
            jsonb_build_object(
              'commenterName', attendee_data.name,
              'comment', comments.comment,
              'timestamp', comments.created_at,
              'gravatarUrl', attendees.gravatar_url
            )
          ),
          '[]'::jsonb
        )
      into comments_
      from attendees
      join comments
        on comments.attendee_id = attendees.id
      join attendee_data
        on attendee_data.attendee_id = attendees.id
        and attendee_data.superseded_at is null
      where
        attendees.event_id = event_id_;

      output_ := jsonb_set(output_, '{comments}', comments_);

      return output_;
    end;
  $$ language plpgsql;

COMMIT;
