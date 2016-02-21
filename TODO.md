MVP:
* given a user's pinterest board url, grab a list of all pins
* return a list of recipes by day

Next Steps:
* [x] error handling of http requests
* [x] return something other than just the first 7 results
* [x] add tests
* [x] add photo urls to pins
* [ ] take input for days to return (e.g. Monday, Thursday, Friday)
* [ ] create UI
* [ ] create oauth flow
* [ ] add to personal website

Later...
* [ ] recommend recipes outside of pinterest
* [ ] email user recipe list
* [ ] fetch additional pages from the board
* [ ] cache boards/pins in local db
* [ ] persist plans (as a pinterest board??)
* [ ] decision to prevent repeats
* [ ] add ability to customize plan (delete recipes)
* [ ] add recommendations
* [ ] parse ingredients
* [ ] create shopping list
* [ ] filter pins that contain a recipe
* [ ] oauth tool
* [ ] breakout language sdk
* [ ] add dining out recommendations/to plan


Other:
* add scalac options to build.sbt
* use https://github.com/typesafehub/config instead of .properties files?
