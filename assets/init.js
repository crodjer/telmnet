/* global Elm */

(function () {
  'use strict';

  var socket,
      initial = {
        receiveMessage: '',
        disconnected: null
      };
  // var telmnet = Elm.fullscreenDebug('Telmnet', 'Telmnet.elm', initial);
  var telmnet = Elm.fullscreen(Elm.Telmnet, initial);

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
        // Don't try to focus the element if it is hidden.
        return;
      }

      if (document.activeElement !== el) {
        el.focus();
      }

      // On focus, the cursor moves to the start of a input box, the following
      // moves it to the end.
      var value = el.value;
      el.value = '';
      el.value = value;

    }, timeout);
  }

  telmnet.ports.reFocus.subscribe(focusEl);

  telmnet.ports.connection.subscribe(function (model) {
    var connected = model.connected,
        server = model.server;

    if (connected) {
      try {
        socket = new WebSocket(server, ['binary']);
      } catch (e) {
        telmnet.ports.disconnected.send(e.message);
        return;
      }
      socket.onmessage = function (msg) {
        var reader = new FileReader();
        reader.addEventListener('loadend', function() {
          telmnet.ports.receiveMessage.send(reader.result);
        });
        reader.readAsBinaryString(msg.data);
      };
      socket.onerror = function (/* err */) {
        if (connected) {
          telmnet.ports.disconnected.send('Server error.');
          connected = false;
        }
      };
      socket.onclose = function () {
        if (connected) {
          telmnet.ports.disconnected.send(null);
          connected = false;
        }
      };
    } else {
      if (socket) {
        socket.close();
      }
      socket = null;
    }
  });

  telmnet.ports.sendMessage.subscribe(function (msg) {
    socket.send(msg);
  });

  // TODO: Avoid the following app logic in JavaScript
  focusEl('server');
  window.onfocus = function () {
    // Focus the prompt
    focusEl('prompt');
  };
}());
