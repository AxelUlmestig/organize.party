#!/bin/sh

set -e

optimize=false

if [ ! -z "$1" ]; then
  if [ "$1" != "--optimize" ]; then
    echo "error: the only allowed argument is '--optimize'"
    exit 1
  fi

  optimize=true

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
  # (cd frontend; uglifyjs $RAW_JS_FILE --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output $MIN_JS_FILE)
  (cd frontend; npm i; RAW_JS_FILE=$RAW_JS_FILE MIN_JS_FILE=$MIN_JS_FILE npm run minify)
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

  # ------------ datepicker.css ---------------------------
  DATEPICKER_CSS=datepicker.$(sha256sum frontend/static/datepicker.css | cut -d " " -f1).css
  cp frontend/static/datepicker.css frontend/static/$DATEPICKER_CSS

  # ------------ navbar.css ---------------------------
  NAVBAR_CSS=navbar.$(sha256sum frontend/static/navbar.css | cut -d " " -f1).css
  cp frontend/static/navbar.css frontend/static/$NAVBAR_CSS

  # ------------ about.css ----------------------------
  ABOUT_CSS=about.$(sha256sum frontend/static/about.css | cut -d " " -f1).css
  cp frontend/static/about.css frontend/static/$ABOUT_CSS
else
  # ------------ Compiled JS -------------------------
  ELM_JS_FILE=elm.js

  (cd frontend; elm make src/Main.elm --output=$ELM_JS_FILE)
  mv frontend/$ELM_JS_FILE frontend/static/$ELM_JS_FILE

  # ------------ CSS ---------------------------------
  STYLE_CSS=style.css
  EXPANDING_TEXTAREA_CSS=expanding-textarea.css
  DATEPICKER_CSS=datepicker.css
  NAVBAR_CSS=navbar.css
  ABOUT_CSS=about.css
fi

cat << EOF > frontend/index.html
<!DOCTYPE html>
<html prefix="og: https://ogp.me/ns#">
  <head>
    <script src="/$ELM_JS_FILE"></script>

    <link rel="stylesheet" href="/$STYLE_CSS">
    <link rel="stylesheet" href="/$EXPANDING_TEXTAREA_CSS">
    <link rel="stylesheet" href="/$DATEPICKER_CSS">
    <link rel="stylesheet" href="/$NAVBAR_CSS">
    <link rel="stylesheet" href="/$ABOUT_CSS">
    <link
      rel="stylesheet"
      href="https://cdn.jsdelivr.net/npm/bootstrap@5.2.0-beta1/dist/css/bootstrap.min.css"
      integrity="sha384-0evHe/X+R7YkIZDRvuzKMRqM+OrBnVFBL6DOitfPri4tjfHxaWutUpFmBp4vmVor"
      crossorigin="anonymous"
    >

    <link rel="icon" href="data:image/svg+xml,<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 100 100%22>
      <text y=%22.9em%22 font-size=%2290%22>📅</text>
    </svg>">

    <!-- OPEN_GRAPH_PLACEHOLDER -->
  </head>

  <body>
    <div id="myapp"></div>
  </body>

  <script type="text/javascript">
    const eventsLocalStorageKey = 'events';
    const nameAndEmailLocalStorageKey = 'lastUsedNameAndEmail';

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

    // Store the input from looking at specific events
    app.ports.writeAttendeeInputToLocalStorage.subscribe(
      ({ eventId, attendeeInput }) => {
        const events = JSON.parse(localStorage.getItem(eventsLocalStorageKey) || '{}')
        const updatedEvent = { ...(events[eventId]), attendeeInput: attendeeInput }
        const updatedEvents = { ...events, [eventId]: updatedEvent }

        localStorage.setItem(eventsLocalStorageKey, JSON.stringify(updatedEvents))
      }
    );

    // Store the submitted name and email and use it when looking at a new
    // event
    app.ports.storeNameAndEmailGlobally.subscribe(
      (nameAndEmail) => {
        localStorage.setItem(nameAndEmailLocalStorageKey, JSON.stringify(nameAndEmail))
      }
    );

    app.ports.requestLocalStorageAttendeeInput.subscribe(
      (eventId) => {
        const events = JSON.parse(localStorage.getItem(eventsLocalStorageKey) || {})
        const attendeeInput = events[eventId]?.attendeeInput

        if(attendeeInput) {
          app
            .ports
            .localStorageAttendeeInputReceiver
            .send(JSON.stringify(attendeeInput))
        } else {
          const nameAndEmail = localStorage.getItem(nameAndEmailLocalStorageKey)

          app
            .ports
            .localStorageAttendeeInputReceiver
            .send(nameAndEmail)
        }
      }
    );
  </script>
</html>
EOF

