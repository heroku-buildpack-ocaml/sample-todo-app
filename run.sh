env
mkdir -p ./tmp
sed "s/HEROKU_PORT/$PORT/" heroku.conf > ./tmp/heroku.conf
/app/vendor/ocamlbrew/opamlib/system/bin/ocsigenserver.opt -c ./tmp/heroku.conf -v
