/* global Elm */

(function () {
  'use strict';

  var telmnet = Elm.fullscreen(Elm.Telmnet, {
    receiveMessage: ''
  });

  function isHidden(el) {
    return el.offsetParent === null;
  }

  function focusEl(elId, timeout) {
    if (elId === '') {
      return;
    }

    timeout = timeout || 50;

    setTimeout(function () {
      var el = document.getElementById(elId);

      if (isHidden(el)) {
        return;
      }

      if (document.activeElement !== el) {
        console.log('Re-focussing: ' + elId);
        document.getElementById(elId).focus();
        var value = el.value;
        el.value = '';
        el.value = value;
      }
    }, timeout);
  }

  focusEl('server', 500);
  telmnet.ports.reFocus.subscribe(focusEl);
  telmnet.ports.sendMessage.subscribe(function (msg) {
    if (msg === '') {
      return;
    }

    console.log('Sent: ' + msg);

    setTimeout(function () {
      console.log('Received: ' + msg);
      telmnet.ports.receiveMessage.send(msg);
    }, 500);
  });

}());
