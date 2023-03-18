# Readme

 Application without any specific purpose other than learning haskell. It makes a call to external api via a scheduled job that parses json and saves it to the sqlite db (for simplicity). There is also simple api to retrieve the results. Some pagination might be added at later point. The app is dockerized with an multistage build otherwise the image size is unnecessarily large.

 Known issue: the app doesn't work on mac m1; there are some issues after an upgrade to mac os ventura 13.2.

 Cmds:
 ```stack build --force-dirty && stack exec my-project-exe```