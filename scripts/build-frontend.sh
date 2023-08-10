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
  RAW_JS_FILE=deleteme.elm.js
  MIN_JS_FILE=elm.min.js

  (cd frontend; elm make src/Main.elm --optimize --output=$RAW_JS_FILE)
  (cd frontend; uglifyjs $RAW_JS_FILE --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output $MIN_JS_FILE)
  rm frontend/$RAW_JS_FILE

  CHECKSUM=$(sha256sum frontend/$MIN_JS_FILE | cut -d " " -f1)
  CACHE_BUST_JS_FILE=elm.min."$CHECKSUM".js
  mv frontend/$MIN_JS_FILE frontend/static/$CACHE_BUST_JS_FILE

  ELM_JS_FILE=$CACHE_BUST_JS_FILE
else
  ELM_JS_FILE=elm.js

  (cd frontend; elm make src/Main.elm --output=$ELM_JS_FILE)
  mv frontend/$ELM_JS_FILE frontend/static/$ELM_JS_FILE
fi

cat << EOF > frontend/index.html
<!DOCTYPE html>
<html>
  <head>
    <script src="/$ELM_JS_FILE"></script>

    <link rel="stylesheet" href="/datepicker.css">
    <link rel="stylesheet" href="/style.css">
    <link rel="stylesheet" href="/expanding-textarea.css">
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
    const app = Elm.Main.init({
      node: document.getElementById('myapp')
    })

    app.ports.writeToLocalStorage.subscribe(
      ({ eventId, attendeeInput }) => localStorage.setItem(eventId, JSON.stringify(attendeeInput))
    );

    app.ports.requestLocalStorageAttendeeInput.subscribe(
        (eventId) => {
          app
            .ports
            .localStorageAttendeeInputReceiver
            .send(
              localStorage.getItem(eventId)
            )
        }
    );
  </script>
</html>
EOF

