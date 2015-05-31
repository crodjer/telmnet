/* global Elm */

(function () {
  'use strict';

  var socket;
  var telmnet = Elm.fullscreen(Elm.Telmnet, {
    receiveMessage: '',
    disconnected: null
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
        el.focus();
      }

      var value = el.value;
      el.value = '';
      el.value = value;

    }, timeout);
  }

  focusEl('server');
  window.onfocus = function () {
    focusEl('prompt');
  };

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
      socket.close();
      socket = null;
    }
  });

  telmnet.ports.sendMessage.subscribe(function (msg) {
    socket.send(msg);
  });
}());
