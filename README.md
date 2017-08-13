[![Flattr this git repo](http://api.flattr.com/button/flattr-badge-large.png)](https://flattr.com/submit/auto?user_id=frteodoro&url=https://github.com/frt/happyscheduler&title=happyscheduler&language=en_GB&tags=github,todo-list,yesod,angular,todo,todolist,angular4&category=software)

# Happy Scheduler

Procrastinate sadness, maximize happyness.

## Frontend

The frontend of this application is inside `frontend` directory, look for the
README.md there to know how to build the frontend, wich is necessary to use
this application.

Executing the following commands should build the frontend and put it's bundle's
where they are needed.

    cd frontend
    ng build --output-path ../static/frontend

## Backend

To execute it in "development mode" just do the following, after the frontend was
built.

    stack exec -- yesod devel
