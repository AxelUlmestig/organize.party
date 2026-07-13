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
      photo_ jsonb := '{}'::jsonb;
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
        and event_data.superseded_at is null
      where
        event_data.id = event_id_;

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
              'commenterName', latest_attendee_data.name,
              'comment', comments.comment,
              'timestamp', comments.created_at,
              'gravatarUrl', latest_attendee_data.gravatar_url
            )
          ),
          '[]'::jsonb
        )
      into comments_
      from comments
      join latest_attendee_data
        on latest_attendee_data.id = comments.attendee_id
      where
        latest_attendee_data.event_id = event_id_;

      output_ := jsonb_set(output_, '{comments}', comments_);

      -- Get photo
      select
        coalesce(
          jsonb_build_object(
            'id', photos.id,
            'url', photos.photo_url,
            'name', photos.name
          ),
          '{}'::jsonb
        )
      into photo_
      from event_data
      join photos
        on photos.id = event_data.photo_id
      where
        event_data.id = event_id_
        and event_data.superseded_at is null;

      output_ := jsonb_set(output_, '{photo}', photo_);

      return output_;
    end;
  $$ language plpgsql;

COMMIT;
