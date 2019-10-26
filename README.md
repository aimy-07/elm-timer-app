# elm-timer-app

## To use
```
# Clone this repository
git clone https://github.com/aimy-07/elm-timer-app

# Go into the repository
cd elm-timer-app

# Install dependencies
npm install

# Create bundle.js(development)
npm run build

# Run the app for browser
npm start

# Run the app for electron
npm run electron
```

## To build electron app package
```
# Create bundle.js(production)
npm run build:prod

# Create app package (For mac)
npx electron-packager . ElmTimerApp --platform=darwin --arch=x64 --overwrite --icon=img/elm-timer-app.icns
```