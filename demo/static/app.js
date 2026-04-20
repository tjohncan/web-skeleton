const log = document.getElementById('log');
const form = document.getElementById('form');
const msg = document.getElementById('msg');
let ws = null;
let reconnectAttempts = 0;
const maxReconnectAttempts = 3;

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
    reconnectAttempts = 0;
    appendLog('[status] connected', 'status');
    if (onOpen) onOpen();
  };
  ws.onmessage = function(e) {
    appendLog('[recv] ' + e.data, 'recv');
  };
  ws.onclose = function() {
    if (reconnectAttempts < maxReconnectAttempts) {
      reconnectAttempts++;
      appendLog('[status] disconnected — reconnecting (' + reconnectAttempts + '/' + maxReconnectAttempts + ')...', 'status');
      setTimeout(connect, 2000);
    } else {
      appendLog('[status] disconnected', 'status');
    }
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
