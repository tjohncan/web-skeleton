var log = document.getElementById('log');
var form = document.getElementById('form');
var msg = document.getElementById('msg');

function appendLog(text, cls) {
  var line = document.createElement('div');
  line.className = cls;
  line.textContent = text;
  log.appendChild(line);
  log.scrollTop = log.scrollHeight;
}

appendLog('[status] connecting...', 'status');
var ws = new WebSocket('ws://' + location.host + '/ws');

ws.onopen = function() {
  appendLog('[status] connected', 'status');
};
ws.onmessage = function(e) {
  appendLog('[recv] ' + e.data, 'recv');
};
ws.onclose = function() {
  appendLog('[status] disconnected', 'status');
};
ws.onerror = function() {
  appendLog('[error] connection error', 'err');
};

form.onsubmit = function(e) {
  e.preventDefault();
  var text = msg.value;
  if (!text) return;
  ws.send(text);
  appendLog('[sent] ' + text, 'sent');
  msg.value = '';
};
