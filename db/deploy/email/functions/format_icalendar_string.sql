-- Deploy events:email/functions/format_icalendar_string to pg

BEGIN;

  create or replace function email.create_icalendar_string(
    event_ event_data,
    created_at_ timestamptz,
    host_url_ text,
    email_ text
  )
  returns text
  as
  $$
  declare
    formatted_start text;
    formatted_end text;
    formatted_created text;
    formatted_modified text;
    formatted_description text;
  begin
    formatted_start := to_char(event_.time_start at time zone 'UTC', 'YYYYMMDD"T"HH24MISS"Z"');
    formatted_end := to_char(
      coalesce(event_.time_end, event_.time_start + interval '1 hour')
      at time zone 'UTC', 'YYYYMMDD"T"HH24MISS"Z"'
    );
    formatted_modified := to_char(event_.created_at at time zone 'UTC', 'YYYYMMDD"T"HH24MISS"Z"');
    formatted_created := to_char(created_at_ at time zone 'UTC', 'YYYYMMDD"T"HH24MISS"Z"');

    -- Format description (escape newlines and include event details)
    formatted_description := regexp_replace(
      event_.description || E'\n\n' || (host_url_ || '/e/' || event_.id::text),
      E'[\\n\\r]', '\\n', 'g'
    );

    return 
'BEGIN:VCALENDAR

CALSCALE:GREGORIAN
VERSION:2.0
PRODID:-//organize.party/event//calendar//EN
METHOD:REQUEST

BEGIN:VEVENT

UID:' || event_.id::text || '
X-MICROSOFT-CDO-OWNERAPPTID:' || event_.id::text || '

DTSTAMP:' || formatted_modified || '
ORGANIZER;CN=organize.party:MAILTO:noreply@organize.party
DTSTART:' || formatted_start || '
DTEND:' || formatted_end || '
SUMMARY:' || event_.title || '
DESCRIPTION:' || formatted_description || '
CREATED:' || formatted_created || '
LAST-MODIFIED:' || formatted_modified || '
LOCATION:' || event_.location || '
SEQUENCE:0

STATUS:CONFIRMED
TRANSP:TRANSPARENT

ATTENDEE;CUTYPE=INDIVIDUAL;ROLE=REQ-PARTICIPANT;PARTSTAT=ACCEPTED;RSVP=TRUE
 ;CN=' || email_ || ';X-NUM-GUESTS=0:mailto:' || email_ || '

END:VEVENT

END:VCALENDAR';
  end;
  $$
  language plpgsql
  immutable
  parallel safe
  cost 50;

COMMIT;
