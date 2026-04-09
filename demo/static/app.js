var log = document.getElementById('log');
var form = document.getElementById('form');
var msg = document.getElementById('msg');
var ws = null;

function appendLog(text, cls) {
  var line = document.createElement('div');
  line.className = cls;
  line.textContent = text;
  log.appendChild(line);
  log.scrollTop = log.scrollHeight;
}

function connect(onOpen) {
  if (ws && ws.readyState <= 1) return;
  appendLog('[status] connecting...', 'status');
  ws = new WebSocket('ws://' + location.host + '/ws');
  ws.onopen = function() {
    appendLog('[status] connected', 'status');
    if (onOpen) onOpen();
  };
  ws.onmessage = function(e) {
    appendLog('[recv] ' + e.data, 'recv');
  };
  ws.onclose = function() {
    appendLog('[status] disconnected', 'status');
  };
  ws.onerror = function() {};
}

form.onsubmit = function(e) {
  e.preventDefault();
  var text = msg.value;
  if (!text) return;
  if (!ws || ws.readyState !== 1) {
    connect(function() {
      ws.send(text);
      appendLog('[sent] ' + text, 'sent');
      msg.value = '';
    });
    return;
  }
  ws.send(text);
  appendLog('[sent] ' + text, 'sent');
  msg.value = '';
};

connect();
