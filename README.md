# purescript-kitty-monitor

[![Build status](https://travis-ci.org/f-o-a-m/foam.monitor.svg?branch=browser)](https://travis-ci.org/f-o-a-m/foam.master?branch=browser)

`purescript-web3` + kitties

## What

This app displays the latest [crypto kitties](https://www.cryptokitties.co/) that have been sold. You can view the contract information [on ethersca](https://etherscan.io/address/0x06012c8cf97bead5deae237070f9587f8e7a266d).

![screenshot](https://github.com/f-o-a-m/purescript-kitty-monitor/blob/master/screenshot.png)

Hosted [here](https://f-o-a-m.github.io/purescript-kitty-monitor/)

## How

This app uses [`purescript-web3`](https://github.com/f-o-a-m/purescript-web3) and [`purescript-thermite`](https://github.com/paf31/purescript-thermite)

For more information, see the [this talk](https://www.youtube.com/watch?v=ozUlodxjH7Y)

## requirements
- [metamask](https://chrome.google.com/webstore/detail/metamask/nkbihfbeogaeaoehlefnkodbefgpgknn?hl=en)
- access to a main-net node where you can install filters
- npm


## Build instructions
```bash
> npm i
> npm run webpack-dev-server
```
