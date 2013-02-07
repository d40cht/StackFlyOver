#!/bin/bash

# Install authbind to enable user bind to port 80
#
# sudo touch /etc/authbind/byport/80
# sudo chown alex:alex /etc/authbind/byport/80
# chmod 755 /etc/authbind/byport/80
#
# Then: ../../play-2.0.2/play stage
# generates: ./target/start
#
set -e

java -Dconfig.resource=dev -DapplyEvolutions.default=true -Djava.net.preferIPv4Stack=true -DLD_PRELOAD=/usr/lib/authbind/libauthbind.so.1 -Dhttp.port=9000 -cp "`dirname $0`/target/staged/*" play.core.server.NettyServer `dirname $0`
