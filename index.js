"use strict";

const { app, BrowserWindow } = require("electron");

app.on('ready', function() {
    const mainWindow = new BrowserWindow({
        width: 1280,
        height: 720,
    });

    mainWindow.loadURL('file://' + __dirname + '/index.html');

    mainWindow.on('closed', function() {
        mainWindow = null;
    });
});

app.on('window-all-closed', function() {
    if (process.platform != 'darwin') {
        app.quit();
    }
});