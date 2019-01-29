#!/usr/bin/env bash

# Automatically recompile sass on change
sass --watch css/mystyles.scss:css/mystyles.css &

# Start initial server. This can handle all changes but those to site.hs
stack exec ftzm-blog watch -- --host "0.0.0.0" &
# get the pid of the above server
SERVER_PID=$!

# watch site.hs and trigger body on change
while inotifywait -e modify site.hs; do
    # kill the server
    kill -9 $SERVER_PID
    # rebuild site.hs
    stack build
    # rebuild site files
    stack exec ftzm-blog rebuild
    # relaunch server
    stack exec ftzm-blog watch -- --host "0.0.0.0" &
    # get the pid of the above server
    SERVER_PID=$!
    # notify that the server is relaunched
    notify-send 'Hakyll relaunched'
done
