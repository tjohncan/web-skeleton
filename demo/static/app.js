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


// ---------------------------------------------------------------------------
// sternum and limbs — server-derived, polled
//
// Everything here comes from /census, which is CONNECTION-CENSUS plus two
// application facts (uptime, and the wake interval the fan-out rides on).
// Aggregate only: counts and states, nothing about any one visitor.

const sternum = document.getElementById('sternum');
const limbRows = document.getElementById('limb-rows');

function duration(secs) {
  const d = Math.floor(secs / 86400);
  const h = Math.floor((secs % 86400) / 3600);
  const m = Math.floor((secs % 3600) / 60);
  const s = secs % 60;
  if (d) return d + 'd ' + h + 'h';
  if (h) return h + 'h ' + m + 'm';
  if (m) return m + 'm ' + s + 's';
  return s + 's';
}

function n(x) { return (x || 0).toLocaleString(); }

// Counters are read off the object rather than named, the same way the
// states are. The census contract says new keys land here and asks a
// consumer not to match against a closed list.
function work(counters) {
  if (!counters) return '\u2014';
  const parts = [];
  if (counters.responses) parts.push(n(counters.responses) + ' responses');
  if (counters.ws_frames) parts.push(n(counters.ws_frames) + ' frames');
  if (counters.client_error) parts.push(n(counters.client_error) + ' refused');
  return parts.length ? parts.join('  \u00b7  ') : '\u2014';
}

function renderCensus(c) {
  const k = c.counters || {};
  sternum.textContent =
    'up ' + duration(c.uptime) +
    '  \u00b7  ' + c.total + (c.total === 1 ? ' connection' : ' connections') +
    '  \u00b7  ' + c.workers + ' workers' +
    '  \u00b7  ' + n(k.responses) + ' responses' +
    '  \u00b7  ' + n(k.ws_frames) + ' frames out' +
    '  \u00b7  fan-out every ' + c.cadence_ms + 'ms';

  limbRows.textContent = '';
  c.per_worker.forEach(function (w, i) {
    // The state keys are read off the object rather than matched against a
    // known list. The census contract calls them diagnostic and says they
    // change with the state machine; a panel that named them would go quietly
    // blank on the first one that was added.
    const states = Object.keys(w.states)
      .map(function (k) { return k + ' ' + w.states[k]; })
      .join(', ');
    const row = document.createElement('tr');
    const conns = String(w.total) + (states ? '   (' + states + ')' : '');
    [String(i), conns, work(w.counters)].forEach(function (v) {
      const cell = document.createElement('td');
      cell.textContent = v;
      row.appendChild(cell);
    });
    limbRows.appendChild(row);
  });
}

function pollCensus() {
  fetch('/census')
    .then(function (r) { return r.json(); })
    .then(renderCensus)
    .catch(function () { sternum.textContent = 'census unavailable'; });
}

pollCensus();
setInterval(pollCensus, 2000);


// ---------------------------------------------------------------------------
// Tabs
//
// Panels are shown and hidden, never unloaded, and nothing here touches the
// WebSocket. That is the point: the socket opened on x-ray stays open while
// you are reading x-periments, so this visit keeps being counted by the
// worker that owns it. Tearing it down on a tab switch would make the census
// a report on which tab you happened to be looking at.

const tabs = document.getElementById('tabs');

function showPanel(name) {
  Array.prototype.forEach.call(tabs.querySelectorAll('.tab'), function (t) {
    const on = t.getAttribute('data-panel') === name;
    t.setAttribute('aria-selected', on ? 'true' : 'false');
  });
  ['x-ray', 'x-periments'].forEach(function (id) {
    document.getElementById(id).hidden = (id !== name);
  });
}

tabs.addEventListener('click', function (e) {
  const t = e.target.closest('.tab');
  if (t) showPanel(t.getAttribute('data-panel'));
});


// ---------------------------------------------------------------------------
// x-periments: the refusal bench
//
// The bytes rendered here come from /bench, not from this file. What the page
// shows and what the server parses are then the same string by construction —
// a panel holding its own copy could display one thing and run another, which
// is the failure the whole page argues against.

const bench = document.getElementById('bench');

function benchCard(c) {
  const card = document.createElement('div');
  card.className = 'case';

  const title = document.createElement('div');
  title.className = 'case-title';
  title.textContent = c.title;
  card.appendChild(title);

  const pre = document.createElement('pre');
  pre.className = 'case-bytes';
  pre.textContent = c.bytes.replace(/
/g, '
').replace(/
+$/, '');
  card.appendChild(pre);

  const row = document.createElement('div');
  row.className = 'case-row';
  const button = document.createElement('button');
  button.textContent = 'send';
  const out = document.createElement('span');
  out.className = 'case-out';
  out.textContent = '';
  button.addEventListener('click', function () {
    button.disabled = true;
    out.textContent = '...';
    fetch('/bench?case=' + encodeURIComponent(c.id))
      .then(function (r) { return r.json(); })
      .then(function (d) {
        out.textContent = d.result;
        out.className = 'case-out ' +
          (d.result.indexOf('accepted') === 0 ? 'accepted' : 'refused');
      })
      .catch(function () { out.textContent = 'no answer'; })
      .then(function () { button.disabled = false; });
  });
  row.appendChild(button);
  row.appendChild(out);
  card.appendChild(row);

  const why = document.createElement('div');
  why.className = 'case-why';
  why.textContent = c.why;
  card.appendChild(why);
  return card;
}

fetch('/bench')
  .then(function (r) { return r.json(); })
  .then(function (cases) {
    bench.textContent = '';
    cases.forEach(function (c) { bench.appendChild(benchCard(c)); });
  })
  .catch(function () { bench.textContent = 'bench unavailable'; });
