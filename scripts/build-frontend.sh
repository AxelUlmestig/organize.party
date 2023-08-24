#!/bin/sh

set -e

optimize=false

if [ ! -z "$1" ]; then
  if [ "$1" != "--optimize" ]; then
    echo "error: the only allowed argument is '--optimize'"
    exit 1
  fi

  optimize=true

  if ! [ -x "$(command -v uglifyjs)" ]; then
    echo "Error: uglifyjs is not installed. Please install it and try again" >&2
    exit 1
  fi

  if ! [ -x "$(command -v sha256sum)" ]; then
    echo "Error: sha256sum is not installed. Please install it and try again" >&2
    exit 1
  fi
fi

if [ $optimize = true ]; then
  # ------------ Compiled JS -------------------------
  RAW_JS_FILE=deleteme.elm.js
  MIN_JS_FILE=elm.min.js

  (cd frontend; elm make src/Main.elm --optimize --output=$RAW_JS_FILE)
  (cd frontend; uglifyjs $RAW_JS_FILE --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output $MIN_JS_FILE)
  rm frontend/$RAW_JS_FILE

  JS_CHECKSUM=$(sha256sum frontend/$MIN_JS_FILE | cut -d " " -f1)
  CACHE_BUST_JS_FILE=elm.min."$JS_CHECKSUM".js
  mv frontend/$MIN_JS_FILE frontend/static/$CACHE_BUST_JS_FILE

  ELM_JS_FILE=$CACHE_BUST_JS_FILE

  # ------------ style.css ---------------------------
  STYLE_CSS=style.$(sha256sum frontend/static/style.css | cut -d " " -f1).css
  cp frontend/static/style.css frontend/static/$STYLE_CSS

  # ------------ expanding-textarea.css --------------
  EXPANDING_TEXTAREA_CSS=expanding-textarea.$(sha256sum frontend/static/expanding-textarea.css | cut -d " " -f1).css
  cp frontend/static/expanding-textarea.css frontend/static/$EXPANDING_TEXTAREA_CSS
else
  # ------------ Compiled JS -------------------------
  ELM_JS_FILE=elm.js

  (cd frontend; elm make src/Main.elm --output=$ELM_JS_FILE)
  mv frontend/$ELM_JS_FILE frontend/static/$ELM_JS_FILE

  # ------------ style.css ---------------------------
  STYLE_CSS=style.css

  # ------------ expanding-textarea.css --------------
  EXPANDING_TEXTAREA_CSS=expanding-textarea.css
fi

cat << EOF > frontend/index.html
<!DOCTYPE html>
<html>
  <head>
    <script src="/$ELM_JS_FILE"></script>

    <link rel="stylesheet" href="/$STYLE_CSS">
    <link rel="stylesheet" href="/$EXPANDING_TEXTAREA_CSS">
    <link rel="stylesheet" href="/datepicker.css">
    <link
      rel="stylesheet"
      href="https://cdn.jsdelivr.net/npm/bootstrap@5.2.0-beta1/dist/css/bootstrap.min.css"
      integrity="sha384-0evHe/X+R7YkIZDRvuzKMRqM+OrBnVFBL6DOitfPri4tjfHxaWutUpFmBp4vmVor"
      crossorigin="anonymous"
    >

    <link rel="icon" href="data:image/svg+xml,<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 100 100%22>
      <text y=%22.9em%22 font-size=%2290%22>📅</text>
    </svg>">
  </head>

  <body>
    <div id="myapp"></div>
  </body>

  <script type="text/javascript">
    // convert from storing all attendee input on the top level mapped from
    // event ids to a more future proof structure
    if(!localStorage.getItem('events')) {
      const events =
        Object
          .keys(localStorage)
          .map(eventId => ({[eventId]: { attendeeInput: JSON.parse(localStorage.getItem(eventId))}}))
          .reduce((obj1, obj2) => ({...obj1, ...obj2}), {})

      localStorage.clear()

      localStorage.setItem('events', JSON.stringify(events))
    }

    const app = Elm.Main.init({
      node: document.getElementById('myapp')
    })

    app.ports.writeToLocalStorage.subscribe(
      ({ eventId, attendeeInput }) => {
        const events = JSON.parse(localStorage.getItem('events') || '{}')
        const updatedEvent = { ...(events[eventId]), attendeeInput: attendeeInput }
        const updatedEvents = { ...events, [eventId]: updatedEvent }

        localStorage.setItem('events', JSON.stringify(updatedEvents))
      }
    );

    app.ports.requestLocalStorageAttendeeInput.subscribe(
      (eventId) => {
        const events = JSON.parse(localStorage.getItem('events') || {})
        const attendeeInput = events[eventId]?.attendeeInput

        if(attendeeInput) {
          app
            .ports
            .localStorageAttendeeInputReceiver
            .send(JSON.stringify(attendeeInput))
        } else {
          app
            .ports
            .localStorageAttendeeInputReceiver
            .send(null)
        }
      }
    );
  </script>
</html>
EOF

