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
  // Already open: run the callback now. Already connecting: hand it to the
  // pending onopen. Returning without doing either dropped the message the
  // caller was about to send, and left it in the input looking unsent.
  if (ws && ws.readyState === 1) { if (onOpen) onOpen(); return; }
  if (ws && ws.readyState === 0) {
    if (onOpen) {
      const prev = ws.onopen;
      ws.onopen = function (e) { if (prev) prev.call(ws, e); onOpen(); };
    }
    return;
  }
  appendLog('[status] connecting...', 'status');
  const proto = location.protocol === 'https:' ? 'wss://' : 'ws://';
  ws = new WebSocket(proto + location.host + '/ws');
  ws.onopen = function() {
    reconnectAttempts = 0;
    appendLog('[status] connected', 'status');
    if (onOpen) onOpen();
  };
  ws.onmessage = function(e) {
    // Wire form is "<seq>	<text>". The sequence is shown because it is the
    // mechanism: it is what each worker compares against to know what it has
    // not yet handed to the connections it owns.
    const tab = e.data.indexOf('	');
    const seq = tab < 0 ? '' : e.data.slice(0, tab);
    const text = tab < 0 ? e.data : e.data.slice(tab + 1);
    appendLog((seq ? '#' + seq + '  ' : '') + text, 'recv');
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

// No local echo. Your own line arrives the way everyone else's does — posted
// to the shared buffer, then handed out by each worker on its own next tick.
// Painting it here immediately would feel faster and would show you an
// ordering nobody else sees. The pause before it appears is the fan-out, and
// it is the thing worth watching.
form.onsubmit = function(e) {
  e.preventDefault();
  const text = msg.value;
  if (!text) return;
  if (!ws || ws.readyState !== 1) {
    connect(function() { ws.send(text); msg.value = ''; });
    return;
  }
  ws.send(text);
  msg.value = '';
};

connect();
