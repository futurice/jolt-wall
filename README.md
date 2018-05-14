# jolt-wall
A small elm project to display jolts from Flowdock to create a jolt dashboard. This project searches through the messages of a single flow and extracts all messages with a `jolt` in them. The app automatically updates the jolts every 5 minutes.

## Development
Make sure you have `create-elm-app` installed globally.

* use `elm-app install` to install necessary packages
* create a copy of the `sample.env` as `.env` and fill in the needed tokens and urls
* use `elm-app start` for starting up a development server

## Deployment

`elm-app build` creates a static build of the app in the `./build` folder. You can put this build folder on any kind of static hosting provider (e.g. netlify) or build a docker container based on the included Dockerfile. The resulting container serves the app. Futurice employees can look at [here](https://futuswarm-mainpage.play.futurice.com/) for hosting a docker container.
