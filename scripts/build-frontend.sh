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

  # ------------ variables.css -----------------------
  VARIABLES_CSS=variables.$(sha256sum frontend/static/variables.css | cut -d " " -f1).css
  cp frontend/static/variables.css frontend/static/$VARIABLES_CSS

  # ------------ style.css ---------------------------
  STYLE_CSS=style.$(sha256sum frontend/static/style.css | cut -d " " -f1).css
  cp frontend/static/style.css frontend/static/$STYLE_CSS

  # ------------ datepicker.css ---------------------------
  DATEPICKER_CSS=datepicker.$(sha256sum frontend/static/datepicker.css | cut -d " " -f1).css
  cp frontend/static/datepicker.css frontend/static/$DATEPICKER_CSS

  # ------------ navbar.css ---------------------------
  NAVBAR_CSS=navbar.$(sha256sum frontend/static/navbar.css | cut -d " " -f1).css
  cp frontend/static/navbar.css frontend/static/$NAVBAR_CSS
else
  # ------------ Compiled JS -------------------------
  ELM_JS_FILE=elm.js

  (cd frontend; elm make src/Main.elm --output=$ELM_JS_FILE)
  mv frontend/$ELM_JS_FILE frontend/static/$ELM_JS_FILE

  # ------------ CSS ---------------------------------
  VARIABLES_CSS=variables.css
  STYLE_CSS=style.css
  DATEPICKER_CSS=datepicker.css
  NAVBAR_CSS=navbar.css
fi

cat << EOF > frontend/index.html
<!DOCTYPE html>
<html prefix="og: https://ogp.me/ns#">
  <head>
    <meta name="viewport" content="width=device-width, initial-scale=1">

    <script src="/$ELM_JS_FILE"></script>

    <link rel="stylesheet" href="/$VARIABLES_CSS">
    <link rel="stylesheet" href="/$STYLE_CSS">
    <link rel="stylesheet" href="/$DATEPICKER_CSS">
    <link rel="stylesheet" href="/$NAVBAR_CSS">

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

    const app = Elm.Main.init({
      node: document.getElementById('myapp')
    })

    // Store the input from looking at specific events
    app.ports.writeAttendeeInputToLocalStorage.subscribe(
      ({ eventId, attendeeInput }) => {
        try {
          const events = JSON.parse(localStorage.getItem(eventsLocalStorageKey) || '{}')
          const updatedEvent = { ...(events[eventId]), attendeeInput: attendeeInput }
          const updatedEvents = { ...events, [eventId]: updatedEvent }

          localStorage.setItem(eventsLocalStorageKey, JSON.stringify(updatedEvents))
        } catch (e) { console.warn('localStorage unavailable', e) }
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
        try {
          const events = JSON.parse(localStorage.getItem(eventsLocalStorageKey) || '{}')
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
        } catch (e) {
            console.warn('localStorage unavailable', e)

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

