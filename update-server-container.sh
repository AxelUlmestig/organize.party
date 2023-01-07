#!/bin/sh

if ! [ -x "$(command -v uglifyjs)" ]; then
  echo "Error: uglifyjs is not installed. Please install it and try again" >&2
  exit 1
fi

RAW_JS_FILE=deleteme.elm.js
MIN_JS_FILE=elm.min.js

(cd frontend; elm make src/Main.elm --optimize --output=$RAW_JS_FILE)
(cd frontend; uglifyjs $RAW_JS_FILE --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output static/$MIN_JS_FILE)
rm frontend/$RAW_JS_FILE

cat << EOF > frontend/index.html
<!DOCTYPE html>
<html>
  <head>
    <script src="/$MIN_JS_FILE"></script>
  </head>
  <body>
    <script type="text/javascript">
      Elm.Main.init();
    </script>
  </body>
</html>
EOF

docker-compose up --force-recreate --build -d server
docker image prune -f
docker-compose exec db sqitch --chdir db deploy
