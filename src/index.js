'use strict';

import "./tailwind.css";
require("./styles.scss");

const DATA_POINTS = 'dataPoints'

const { Elm } = require('./Main');

const getDataPoints = () => {
    const storage = localStorage.getItem(DATA_POINTS)
    if (storage !== null && storage !== undefined) {
        return JSON.parse(storage)
    }
    return null
}

const storeDataPoints = (dataPoints) => {
    const json = JSON.stringify(dataPoints)
    localStorage.setItem(DATA_POINTS, json)
}

const app = Elm.Main.init({ flags: getDataPoints() || [] });

app.ports.storeDataPoints.subscribe(data => {
    storeDataPoints(data)
})

