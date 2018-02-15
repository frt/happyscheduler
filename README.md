# Happy Scheduler

Procrastinate sadness, maximize happyness.

## Frontend

The frontend of this application is inside `frontend` directory, look for the
README.md there to know how to build the frontend, wich is necessary to use
this application.

Executing the following commands should build the frontend and put it's bundle's
where they are needed.

    cd frontend
    ./ng-build

## Backend

To execute it in "development mode" just do the following, after the frontend was
built.

    stack install yesod-bin --install-ghc
    stack exec -- yesod devel

## Donations

[![Flattr this git repo](http://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=frteodoro&url=https://github.com/frt/happyscheduler&title=happyscheduler&language=en_GB&tags=github,todo-list,yesod,angular,todo,todolist,angular4&category=software)

<a href='https://ko-fi.com/M4M09AL6' target='_blank'><img height='36' style='border:0px;height:36px;' src='https://az743702.vo.msecnd.net/cdn/kofi5.png?v=0' border='0' alt='Buy Me a Coffee at ko-fi.com' /></a>
