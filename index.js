"use strict";

const { app, BrowserWindow, Menu } = require("electron");

/* ---------------------------------
	メニューの設定
---------------------------------- */
const templateMenu = [
    {
        label: 'Menu',
            submenu: [{
                label: 'Quit',
                role: 'quit'
        }
    ]
}];

/* ---------------------------------
	メイン画面の設定
---------------------------------- */
app.on('ready', function() {
    const menu = Menu.buildFromTemplate(templateMenu);
    Menu.setApplicationMenu(menu);

    let mainWindow = new BrowserWindow({
        width: 340,
        height: 500,
    });

    mainWindow.loadURL('file://' + __dirname + '/index.html');

    // 起動オプションに "--debug"があれば開発者ツールを起動
    if (process.argv.find((arg) => arg === '--debug')) {
        win.webContents.openDevTools()
    }

    mainWindow.on('closed', function() {
        mainWindow = null;
    });
});

app.on('window-all-closed', () => {
    if (process.platform !== 'darwin') {
        app.quit()
    }
});