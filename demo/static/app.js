const log = document.getElementById('log');
const form = document.getElementById('form');
const msg = document.getElementById('msg');
let ws = null;

function appendLog(text, cls) {
  const line = document.createElement('div');
  line.className = cls;
  line.textContent = text;
  log.appendChild(line);
  log.scrollTop = log.scrollHeight;
}

function connect(onOpen) {
  if (ws && ws.readyState <= 1) return;
  appendLog('[status] connecting...', 'status');
  const proto = location.protocol === 'https:' ? 'wss://' : 'ws://';
  ws = new WebSocket(proto + location.host + '/ws');
  ws.onopen = function() {
    appendLog('[status] connected', 'status');
    if (onOpen) onOpen();
  };
  ws.onmessage = function(e) {
    appendLog('[recv] ' + e.data, 'recv');
  };
  ws.onclose = function() {
    appendLog('[status] disconnected — reconnecting...', 'status');
    setTimeout(connect, 2000);
  };
  ws.onerror = function(e) {
    appendLog('[error] connection error', 'err');
  };
}

form.onsubmit = function(e) {
  e.preventDefault();
  const text = msg.value;
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
