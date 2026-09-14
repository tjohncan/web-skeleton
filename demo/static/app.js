const log = document.getElementById('log');
const form = document.getElementById('form');
const msg = document.getElementById('msg');
let ws = null;
let reconnectAttempts = 0;
// Which worker owns this socket, once it has said so. Constant for the life
// of the connection, and not the worker that will serve the next request
// from this same browser — which is the reason it is worth showing.
let myWorker = null;
// This connection's handle, as the server names it. Used to pick our own
// lines out of a broadcast we share with strangers.
let myHandle = null;
let lastCensus = null;
const maxReconnectAttempts = 3;

function appendLog(text, cls) {
  const line = document.createElement('div');
  line.className = cls;
  line.textContent = text;
  log.appendChild(line);
  log.scrollTop = log.scrollHeight;
}

// A posted line, with the sequence it was given and the handle that sent it.
// The handle is instance:worker:connection — enough to follow one stranger
// through a conversation, and nothing about who they are. Built as elements
// rather than one string so the parts can be styled apart and so nothing
// here is ever parsed as markup.
function appendBulletin(seq, when, who, text) {
  const line = document.createElement('div');
  line.className = 'recv' + (who === myHandle ? ' own' : '');
  // The sequence is on the line, not in it. It is the mechanism and worth
  // being able to find, but it counts from server start and only goes up,
  // so printing it in front of every sentence is seven digits of noise on
  // a process that has been up for a month.
  line.title = 'post #' + seq;

  const s = document.createElement('span');
  s.className = 'when';
  s.textContent = when;
  line.appendChild(s);

  const w = document.createElement('span');
  w.className = 'who';
  w.textContent = who;
  line.appendChild(document.createTextNode('  '));
  line.appendChild(w);

  line.appendChild(document.createTextNode('  ' + text));
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
    // Ask which worker owns this socket. Binary, because the bulletin box
    // sends text and only text — so a control frame cannot collide with
    // anything a visitor is able to type, in either direction.
    ws.send(new TextEncoder().encode('worker'));
    if (onOpen) onOpen();
  };
  ws.onmessage = function(e) {
    // A posted line is four fields on three TABs — sequence, stamp, sender,
    // text — parsed below.
    //
    // A frame with no TAB in it is the server talking rather than a line
    // somebody posted: a control answer, or word that posts are being
    // dropped. %BULLETIN-PAYLOAD always writes three and %SANITIZE-LINE
    // strips any that were typed, so neither can be mistaken for the other.
    const tab = e.data.indexOf('\t');
    if (tab < 0) {
      const m = /^worker (\d+) (\S+)$/.exec(e.data);
      if (m) {
        myWorker = Number(m[1]);
        myHandle = m[2];
        if (lastCensus) renderCensus(lastCensus);
      } else {
        appendLog('[status] ' + e.data, 'status');
      }
      return;
    }
    // <seq> TAB <utc> TAB <handle> TAB <text>. Split on the first three
    // only. The text cannot contain a tab — the server strips every byte
    // below 32 before posting — but splitting on all of them would be
    // trusting that rather than needing only the three the format puts there.
    const parts = [];
    let from = 0;
    for (let i = 0; i < 3; i++) {
      const at = e.data.indexOf('\t', from);
      if (at < 0) break;
      parts.push(e.data.slice(from, at));
      from = at + 1;
    }
    parts.push(e.data.slice(from));
    // A frame that is not the shape above is shown rather than dropped: a
    // line nobody can read beats a line nobody knows arrived.
    if (parts.length === 4) {
      appendBulletin(parts[0], parts[1], parts[2], parts[3]);
    } else {
      appendLog(e.data, 'recv');
    }
  };
  ws.onclose = function() {
    // The worker and handle belonged to that socket. A reconnect may land on
    // another worker under a new serial, and asks again when it opens; until
    // then — and for good, once reconnecting gives up — no row is this page's
    // socket, and marking one would be showing a socket the page no longer has.
    myWorker = null;
    myHandle = null;
    if (lastCensus) renderCensus(lastCensus);
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

// A few counters, by name, for a narrow cell. That is the closed list the
// census contract warns a consumer against, taken on purpose: this cell is a
// summary of a worker's day, not a rendering of the census, so a counter
// added later not appearing here is the intent rather than a silent loss.
// The whole set is one request away at /census.
//
// Labels say which counter they are. client_error is every 4xx — a lab 400,
// a 404, a parser refusal — and "refused" belongs to a different counter
// entirely, the accepts a worker at its limit turned away.
function work(counters) {
  if (!counters) return '\u2014';
  const parts = [];
  if (counters.responses) parts.push(n(counters.responses) + ' responses');
  if (counters.ws_frames) parts.push(n(counters.ws_frames) + ' frames');
  if (counters.client_error) parts.push(n(counters.client_error) + ' 4xx');
  if (counters.refused) parts.push(n(counters.refused) + ' refused');
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
    // The row for the worker holding this page's WebSocket. One socket, one
    // worker, for as long as it stays open — while the requests the lab tab
    // makes land wherever the kernel chooses to put them.
    if (i === myWorker) row.className = 'mine';
    const conns = String(w.total) + (states ? '   (' + states + ')' : '');
    const who = String(i) + (i === myWorker ? '   \u2190 your socket' : '');
    [who, conns, work(w.counters)].forEach(function (v) {
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
    .then(function (c) { lastCensus = c; renderCensus(c); })
    .catch(function () { sternum.textContent = 'census unavailable'; });
}

// Polled only while someone can see the result. The numbers render on x-ray
// alone, so a page in a background tab, or open on x-periments, would be
// asking every two seconds for a picture nobody is looking at. Coming back
// into view asks at once rather than waiting out the interval.
function censusInView() {
  return !document.hidden && !document.getElementById('x-ray').hidden;
}

function pollIfInView() {
  if (censusInView()) pollCensus();
}

pollCensus();
setInterval(pollIfInView, 2000);
document.addEventListener('visibilitychange', pollIfInView);


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
  pollIfInView();
}

tabs.addEventListener('click', function (e) {
  const t = e.target.closest('.tab');
  if (t) showPanel(t.getAttribute('data-panel'));
});


// ---------------------------------------------------------------------------
// x-periments: the lab
//
// Three ordinary GETs, each shown whole — the request as the server parsed
// it, the response as the browser received it, and both clocks.
//
// The request block is the server's account of what arrived, not this file's
// account of what it sent. fetch() does not expose the bytes the browser put
// on the wire: it adds headers of its own and normalises what it is handed.
// A panel drawing its own version would be showing a reconstruction and
// calling it the request, and the server is the only witness that was there.
//
// The response block claims no HTTP version, because this side cannot see
// one. A proxy in front may be speaking h2 to the browser while speaking 1.1
// to the server, and printing "HTTP/1.1" here would be a guess dressed as a
// reading. The request block can print a version because the server reported
// the one it parsed.
//
// Its body is the text that arrived, not the JSON parsed and printed again,
// which would be this page's formatting presented as the server's. Its
// headers are as fetch() exposes them — lowercased and in fetch's order, not
// the wire's — and the label says so, because that much this side cannot
// get back.

const lab = document.getElementById('lab');

// iat a year back, exp in 2030, so both relative forms are on screen at once.
// The signature is text rather than a signature: this endpoint decodes and
// says out loud that it does not verify.
const SAMPLE_JWT =
  'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9' +
  '.eyJzdWIiOiJkZW1vIiwibmFtZSI6IndlYi1za2VsZXRvbiIsImlhdCI6MTc1NzYzNTIwMCwi' +
  'ZXhwIjoxODkzNDU2MDAwfQ' +
  '.bm90IGEgcmVhbCBzaWduYXR1cmUsIHRoaXMgZW5kcG9pbnQgZG9lcyBub3QgdmVyaWZ5';

const LAB_TOOLS = [
  {
    id: 'base64',
    title: 'base64',
    path: '/lab/base64',
    fields: [
      { name: 'op', type: 'select', options: ['encode', 'decode',
                                              'encode-url', 'decode-url'] },
      { name: 's', type: 'text', wide: true, value: 'web-skeleton',
        placeholder: 'text' }
    ],
    answer: function (r) { return r.out; }
  },
  {
    id: 'hash',
    title: 'digest',
    // Every lab input is a GET parameter, so it is in the URL, and a URL is
    // what a proxy writes to its access log. For a digest that only matters
    // for the key, which is the one input here that could be a secret.
    note: 'the hmac key travels in the URL and lands in proxy logs — use a throwaway key',
    path: '/lab/hash',
    fields: [
      { name: 'alg', type: 'select', options: ['sha256', 'sha1',
                                               'hmac-sha256'] },
      { name: 's', type: 'text', wide: true, value: 'web-skeleton',
        placeholder: 'text' },
      { name: 'key', type: 'text', placeholder: 'key (hmac only)' }
    ],
    answer: function (r) { return r.hex; }
  },
  {
    id: 'jwt',
    title: 'jwt',
    note: 'decoded, never verified — do not paste a token you care about',
    path: '/lab/jwt',
    fields: [
      { name: 'token', type: 'text', wide: true, value: SAMPLE_JWT,
        placeholder: 'eyJhbGciOi...' }
    ],
    answer: function (r) {
      return JSON.stringify({ header: r.header, payload: r.payload,
                              times: r.times,
                              signature_bytes: r.signature_bytes }, null, 2);
    }
  }
];

function clockOf(d) {
  function p(n, w) { return String(n).padStart(w, '0'); }
  return p(d.getHours(), 2) + ':' + p(d.getMinutes(), 2) + ':' +
         p(d.getSeconds(), 2) + '.' + p(d.getMilliseconds(), 3);
}

function headerBlock(headers) {
  const out = [];
  headers.forEach(function (v, k) { out.push(k + ': ' + v); });
  return out.join('\n');
}

function el(tag, cls, text) {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  if (text !== undefined) e.textContent = text;
  return e;
}

function labCard(tool) {
  const card = el('div', 'tool');
  card.appendChild(el('div', 'tool-title', tool.title));
  if (tool.note) card.appendChild(el('div', 'tool-note', tool.note));

  const form = el('form', 'tool-form');
  const inputs = {};
  tool.fields.forEach(function (f) {
    let input;
    if (f.type === 'select') {
      input = el('select');
      f.options.forEach(function (o) {
        const opt = el('option', null, o);
        opt.value = o;
        input.appendChild(opt);
      });
    } else {
      input = el('input');
      input.type = 'text';
      if (f.placeholder) input.placeholder = f.placeholder;
      if (f.value) input.value = f.value;
      if (f.wide) input.className = 'wide';
    }
    input.setAttribute('aria-label', f.name);
    inputs[f.name] = input;
    form.appendChild(input);
  });
  const run = el('button', null, 'run');
  run.type = 'submit';
  form.appendChild(run);
  card.appendChild(form);

  const out = el('div', 'tool-out');
  out.hidden = true;
  card.appendChild(out);

  function show(sentAt, ms, res, body, raw) {
    out.hidden = false;
    out.textContent = '';

    // The answer first, because it is what someone came for. Everything
    // below it is the evidence for it.
    if (body && body.result) {
      const a = el('pre', 'tool-answer', tool.answer(body.result));
      out.appendChild(a);
      if (body.result.note) {
        out.appendChild(el('div', 'tool-note', body.result.note));
      }
    } else if (body && body.error) {
      out.appendChild(el('pre', 'tool-answer bad', body.error));
    }

    const t = el('div', 'tool-times');
    t.appendChild(el('span', null, 'sent ' + clockOf(sentAt)));
    t.appendChild(el('span', null, 'finished ' + clockOf(new Date())));
    t.appendChild(el('span', null, 'round trip ' + ms.toFixed(1) + ' ms'));
    if (body && typeof body.server_us === 'number') {
      t.appendChild(el('span', null, 'server ' + body.server_us + ' µs'));
    }
    if (body && body.worker !== null && body.worker !== undefined) {
      t.appendChild(el('span', 'tool-worker', 'worker ' + body.worker));
    }
    out.appendChild(t);

    if (body && body.request) {
      out.appendChild(el('div', 'tool-label', 'request, as the server parsed it'));
      out.appendChild(el('pre', 'tool-wire',
                         body.request.replace(/\r\n/g, '\n').replace(/\n+$/, '')));
    }

    out.appendChild(el('div', 'tool-label',
                       'response: body as received, headers as fetch() exposes them'));
    const status = res.status + (res.statusText ? ' ' + res.statusText : '');
    out.appendChild(el('pre', 'tool-wire',
                       status + '\n' + headerBlock(res.headers) + '\n\n' + raw));
  }

  form.addEventListener('submit', function (e) {
    e.preventDefault();
    const params = new URLSearchParams();
    tool.fields.forEach(function (f) {
      const v = inputs[f.name].value;
      // An empty optional field is left out rather than sent empty: the
      // request shown below should be the one that was made.
      if (v !== '') params.set(f.name, v);
    });
    run.disabled = true;
    const sentAt = new Date();
    const t0 = performance.now();
    let res = null;
    fetch(tool.path + '?' + params.toString())
      .then(function (r) { res = r; return r.text(); })
      .then(function (raw) {
        // Parsed for the answer and the timings; shown as it came for the
        // response block. A body that is not JSON still gets shown.
        let body = null;
        try { body = JSON.parse(raw); } catch (e) { body = null; }
        show(sentAt, performance.now() - t0, res, body, raw);
      })
      .catch(function (err) {
        out.hidden = false;
        out.textContent = 'no answer: ' + err;
      })
      .then(function () { run.disabled = false; });
  });

  return card;
}

LAB_TOOLS.forEach(function (t) { lab.appendChild(labCard(t)); });

// ---------------------------------------------------------------------------
// x-periments: the appendix, a bench of refusals
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

  const req = document.createElement('div');
  req.className = 'tool-label';
  req.textContent = 'request, the exact bytes handed to the parser';
  card.appendChild(req);

  const pre = document.createElement('pre');
  pre.className = 'case-bytes';
  pre.textContent = c.bytes.replace(/\r\n/g, '\n').replace(/\n+$/, '');
  card.appendChild(pre);

  // The response the server builds for this case, serialized by the
  // framework at startup rather than drawn here. Nothing sent it: a browser
  // cannot put the request above on the wire, which is the whole reason this
  // panel exists, so there is no round trip to time and no response headers
  // this page could honestly claim to have received.
  if (c.response) {
    const rl = document.createElement('div');
    rl.className = 'tool-label';
    rl.textContent = 'response the server builds for it: built once at startup, ' +
                     'so its date is then, and never sent';
    card.appendChild(rl);

    const rp = document.createElement('pre');
    rp.className = 'case-bytes';
    rp.textContent = c.response.replace(/\r\n/g, '\n').replace(/\n+$/, '');
    card.appendChild(rp);
  }

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
