require('./main.scss');

const { Elm } = require('./Main.elm');

const app = Elm.Main.init({
    node: document.getElementById('root')
});



const alarmSound = new Audio();
alarmSound.src = './audio/alarmSound.mp3';

app.ports.playAlarmSound.subscribe(() => {
    alarmSound.play();
})

app.ports.stopAlarmSound.subscribe(() => {
    alarmSound.pause();
    alarmSound.currentTime = 0;
})
