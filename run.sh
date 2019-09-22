set -euo pipefail

WEB_OUT=cloud/src/main/webapp

elm make Main.elm --output $WEB_OUT/my.js

cp index.html $WEB_OUT/
cp styles.css $WEB_OUT/

pushd cloud

mvn -Dhttps.protocols=TLSv1.2 clean package
mvn -Dhttps.protocols=TLSv1.2 appengine:devserver

popd
